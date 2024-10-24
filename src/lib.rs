mod marc8;

use core::str;
use std::{
    borrow::Cow,
    fmt::{Display, Write},
    str::Utf8Error,
};

use marc8::Marc8Decoder;
use thiserror::Error;
use winnow::{
    ascii::digit1,
    combinator::{fail, repeat, separated, seq},
    error::{
        AddContext, ContextError, ErrMode, FromExternalError, ParseError, StrContext,
        StrContextValue,
    },
    stream::Stream,
    token::{any, one_of, take, take_while},
    PResult, Parser,
};

/// Parse a set of MARC records from bytes
//
/// This requires bytes because a MARC record can have various encodings,
/// UTF-8 and MARC-8 being the most common ones.
/// It assumes all the records are valid and complete and will also fail if
/// any extra content is found.
///
/// Right now, the parser enforces a few constraints:
/// - It only accepts MARC records with a value of 2 for the indicator count;
/// - It only accepts singular character subfields (described as 2 for the marker);
/// - It only accepts directory entries using the standard 4-5-0-0 schema
/// - MARC-8 encoding support is limited to latin character sets

pub fn parse_records(data: &[u8]) -> Result<Vec<Record>, Error> {
    Ok(repeat(0.., Record::parse_next).parse(data)?)
}

#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Unexpected EOF")]
    UnexpectedEof,
    #[error("Failed to parse: {} at byte offset `{}`", reason, offset)]
    ParseFailed { reason: String, offset: usize },
}

impl From<ParseError<&[u8], ContextError>> for Error {
    fn from(value: ParseError<&[u8], ContextError>) -> Self {
        Self::ParseFailed {
            // reason: value
            //     .inner()
            //     .context()
            //     .map(|ctx| ctx.to_string())
            //     .collect::<Vec<String>>()
            //     .join("\n"),
            reason: value.to_string(),
            offset: value.offset(),
        }
    }
}

pub struct Record {
    pub leader: Leader,
    pub fields: Vec<Field>,
}

/// A MARC record describes a content or piece of content using a series of fields.
/// Fields are identified by a three-digit ASCII code (e.g. `001`) and contain
/// either control or field data. Control fields are identified by being in the 000-099 range
/// while data fields are all the others.
/// Control fields contain a single piece of information
impl Record {
    fn parse_next(input: &mut &[u8]) -> PResult<Self> {
        let leader = Leader::parse_next
            .context(StrContext::Label("leader"))
            .parse_next(input)?;
        let directory = parse_directory_entries
            .context(StrContext::Label("directory"))
            .parse_next(input)?;
        let decoder = leader.coding_scheme.decoder();
        let fields = parse_data(&decoder, &directory, input)?;
        #[allow(const_item_mutation)]
        RECORD_SEPARATOR
            .context(StrContext::Label("record separator"))
            .context(StrContext::Expected(StrContextValue::Description(
                "record eof marker",
            )))
            .parse_next(input)?;

        Ok(Record { leader, fields })
    }
}

#[derive(Debug)]
pub struct Leader {
    pub record_length: u16,
    pub status: Status,
    pub record_type: char,
    pub bibliographical_level: char,
    pub control_type: char,
    pub coding_scheme: CodingScheme,
    pub data_base_address: u16,
    pub encoding_level: char,
    pub descriptive_cataloging_form: char,
    pub multipart_resource_record_level: char,
}

impl Leader {
    pub fn parse_next(input: &mut &[u8]) -> PResult<Self> {
        let length = parse_fixed_num::<5>
            .context(StrContext::Label("record length"))
            .context(StrContext::Expected(StrContextValue::Description(
                "integer length",
            )))
            .parse_next(input)?;
        let status = parse_status
            .context(StrContext::Label("status"))
            .parse_next(input)?;
        let record_type = parse_char(input)?;
        let bibliographical_level = parse_char(input)?;
        let control_type = parse_char(input)?;
        let coding_scheme = parse_coding_scheme(input)?;
        '2'.parse_next(input)?; // Indicator count
        '2'.parse_next(input)?; // Subfield code count
        let data_base_address = parse_fixed_num::<5>(input)?;
        let encoding_level = parse_char(input)?;
        let descriptive_cataloging_form = parse_char(input)?;
        let multipart_resource_record_level = parse_char(input)?;
        '4'.parse_next(input)?;
        '5'.parse_next(input)?;
        '0'.parse_next(input)?;
        '0'.parse_next(input)?;

        Ok(Self {
            record_length: length,
            status,
            record_type,
            bibliographical_level,
            control_type,
            coding_scheme,
            data_base_address,
            encoding_level,
            descriptive_cataloging_form,
            multipart_resource_record_level,
        })
    }
}

fn parse_fixed_num<const I: usize>(input: &mut &[u8]) -> PResult<u16> {
    take(I).and_then(digit1).parse_to().parse_next(input)
}

#[derive(Debug, PartialEq, Eq)]
pub enum Status {
    IncreaseInEncoding,
    Corrected,
    Deleted,
    New,
    IncreaseFromPrepublication,
}

#[inline]
fn parse_char(input: &mut &[u8]) -> PResult<char> {
    let ch = any.parse_next(input)? as char;
    Ok(ch)
}

fn parse_status(input: &mut &[u8]) -> PResult<Status> {
    // SAFETY: the `as char` is safe because we know it's one of the limited values
    let ch = one_of(['a', 'c', 'd', 'n', 'p']).parse_next(input)? as char;

    match ch {
        'a' => Ok(Status::IncreaseInEncoding),
        'c' => Ok(Status::Corrected),
        'd' => Ok(Status::Deleted),
        'n' => Ok(Status::New),
        'p' => Ok(Status::IncreaseFromPrepublication),
        _ => fail(input),
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum CodingScheme {
    Marc8,
    Ucs,
}

impl CodingScheme {
    fn decoder(&self) -> Decoder {
        match self {
            CodingScheme::Marc8 => Decoder::Marc8(marc8::Marc8Decoder {}),
            CodingScheme::Ucs => Decoder::Utf8(Utf8Decoder {}),
        }
    }
}

enum Decoder {
    Marc8(Marc8Decoder),
    Utf8(Utf8Decoder),
}

impl TextDecoder for Decoder {
    fn decode<'a>(&self, text: &'a [u8]) -> Result<Cow<'a, str>, DecodeError> {
        match self {
            Decoder::Marc8(marc8_decoder) => marc8_decoder.decode(text),
            Decoder::Utf8(utf8_decoder) => utf8_decoder.decode(text),
        }
    }
}

fn parse_coding_scheme(input: &mut &[u8]) -> PResult<CodingScheme> {
    // SAFETY: the `as char` is safe because we know it's one of the limited values
    let ch = one_of([' ', 'a']).parse_next(input)? as char;

    match ch {
        ' ' => Ok(CodingScheme::Marc8),
        'a' => Ok(CodingScheme::Ucs),
        _ => fail(input),
    }
}

const RECORD_SEPARATOR: u8 = 0x1D;
const FIELD_SEPARATOR: u8 = 0x1E;
const SUBFIELD_SEPARATOR: u8 = 0x1F;

#[derive(Debug)]
pub struct DirectoryEntry {
    pub tag: [char; 3],
    pub field_length: usize,
    pub starting_pos: usize,
}

impl DirectoryEntry {
    fn is_control(&self) -> bool {
        &self.tag[0..2] == ['0', '0']
    }
}

#[allow(const_item_mutation)]
fn parse_directory_entries(input: &mut &[u8]) -> PResult<Vec<DirectoryEntry>> {
    let entries = repeat(0.., parse_dir_entry).parse_next(input)?;
    FIELD_SEPARATOR.parse_next(input)?;
    Ok(entries)
}

fn parse_dir_entry(input: &mut &[u8]) -> PResult<DirectoryEntry> {
    let mut tag = [' '; 3];
    let t: Vec<char> = repeat(
        3usize,
        any.verify_map(|c: u8| {
            if c >= '0' as u8 && c <= '9' as u8 {
                Some(c as char)
            } else {
                None
            }
        }),
    )
    .parse_next(input)?;
    tag.copy_from_slice(&t);
    let field_length = parse_fixed_num::<4>.parse_next(input)? as usize;
    let starting_pos = parse_fixed_num::<5>.parse_next(input)? as usize;

    Ok(DirectoryEntry {
        tag,
        field_length,
        starting_pos,
    })
}

fn parse_data(
    decoder: &dyn TextDecoder,
    dir: &[DirectoryEntry],
    input: &mut &[u8],
) -> PResult<Vec<Field>> {
    // At this point, we should be at the beginning of the data section. We need
    // to use the offset from each entry to locate the field.
    let mut fields = Vec::with_capacity(dir.len());

    let mut total_size = 0usize;
    for entry in dir {
        let sub_input = &mut &input[entry.starting_pos..(entry.starting_pos + entry.field_length)];
        total_size += entry.field_length;
        fields.push(parse_field(decoder, entry, sub_input)?);
    }

    take(total_size)
        .context(StrContext::Label("field data"))
        .parse_next(input)?;
    Ok(fields)
}

fn parse_field(
    decoder: &dyn TextDecoder,
    entry: &DirectoryEntry,
    input: &mut &[u8],
) -> PResult<Field> {
    if entry.is_control() {
        let data = take(entry.field_length - 1)
            .verify_map(|data: &[u8]| std::str::from_utf8(data).map(|s| s.to_owned()).ok())
            .context(StrContext::Label("control field data"))
            .parse_next(input)?;
        Ok(Field::Control(ControlField {
            tag: FieldTag(entry.tag.clone()),
            data,
        }))
    } else {
        let indicator = take(2u8)
            .context(StrContext::Label("indicator"))
            .parse_next(input)?;
        let indicator = indicator
            .iter()
            .map(|ch: &u8| *ch as char)
            .collect::<Vec<char>>();
        #[allow(const_item_mutation)]
        SUBFIELD_SEPARATOR
            .context(StrContext::Label("subfield separator"))
            .parse_next(input)?;
        let subfields = separated(1.., parse_subfield(decoder), SUBFIELD_SEPARATOR)
            .context(StrContext::Label("subfields"))
            .parse_next(input)?;
        #[allow(const_item_mutation)]
        FIELD_SEPARATOR
            .context(StrContext::Label("field separator"))
            .parse_next(input)?;
        Ok(Field::Data(DataField {
            indicator,
            tag: FieldTag(entry.tag.clone()),
            subfields,
        }))
    }
}

#[derive(Debug, Error)]
enum DecodeError {
    #[error("UTF-8 error: {0}")]
    Utf(Utf8Error),
    #[error("Unknown char: {0}")]
    Unknown(u8),
    #[error("Invalid pair: base `{0}` with combining `{1}`")]
    InvalidPair(char, char),
}

trait TextDecoder {
    fn decode<'a>(&self, text: &'a [u8]) -> Result<Cow<'a, str>, DecodeError>;
}

// Blanket impl for boxes
impl<T> TextDecoder for Box<T>
where
    T: TextDecoder + Sized,
{
    fn decode<'a>(&self, text: &'a [u8]) -> Result<Cow<'a, str>, DecodeError> {
        self.as_ref().decode(text)
    }
}

fn parse_subfield<'a>(
    decoder: &dyn TextDecoder,
) -> impl Parser<&'a [u8], Subfield, ContextError> + '_ {
    move |input: &mut &[u8]| {
        seq! {
            Subfield{
                tag: take(1usize).verify_map(|data: &[u8]| {
                        char::from_u32(data[0] as u32).map(|ch| SubfieldTag(ch))
                    }).context(StrContext::Label("tag")),
                data: take_while(0.., |tok| tok != SUBFIELD_SEPARATOR && tok != FIELD_SEPARATOR).and_then(decode_text(decoder)).map(|cow| cow.to_string()),
            }
        }
        .context(StrContext::Label("subfield"))
        .parse_next(input)
    }
}

fn decode_text<'a>(
    decoder: &dyn TextDecoder,
) -> impl Parser<&'a [u8], Cow<'a, str>, ContextError> + '_ {
    move |input: &mut &'a [u8]| match decoder.decode(input) {
        Ok(data) => return Ok(data),
        Err(e) => Err(ErrMode::Cut(
            ContextError::from_external_error(input, winnow::error::ErrorKind::Fail, e)
                .add_context(input, &input.checkpoint(), StrContext::Label("text")),
        )),
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct FieldTag([char; 3]);

impl Display for FieldTag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char(self.0[0])?;
        f.write_char(self.0[1])?;
        f.write_char(self.0[2])
    }
}

#[derive(Debug, PartialEq, Eq)]
pub enum Field {
    Control(ControlField),
    Data(DataField),
}

impl Field {
    pub fn control(&self) -> Option<&ControlField> {
        match self {
            Field::Control(control_field) => Some(control_field),
            Field::Data(_) => None,
        }
    }

    pub fn data(&self) -> Option<&DataField> {
        match self {
            Field::Control(_) => None,
            Field::Data(data_field) => Some(data_field),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ControlField {
    pub tag: FieldTag,
    pub data: String,
}

#[derive(Debug, PartialEq, Eq)]
pub struct DataField {
    pub tag: FieldTag,
    pub indicator: Vec<char>,
    pub subfields: Vec<Subfield>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct SubfieldTag(char);

impl Display for SubfieldTag {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_char(self.0)
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Subfield {
    pub tag: SubfieldTag,
    pub data: String,
}

struct Utf8Decoder {}

impl TextDecoder for Utf8Decoder {
    fn decode<'a>(&self, text: &'a [u8]) -> Result<Cow<'a, str>, DecodeError> {
        str::from_utf8(&text)
            .map(|s| Cow::Borrowed(s))
            .map_err(|e| DecodeError::Utf(e))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn it_parses_a_leader() {
        let input = &b"01649cam  22003855a 4500";
        let leader = Leader::parse_next(&mut &input[..]).unwrap();
        assert_eq!(leader.record_length, 1649);
        assert_eq!(leader.status, Status::Corrected);
        assert_eq!(leader.coding_scheme, CodingScheme::Marc8);
        assert_eq!(leader.data_base_address, 385);
    }

    #[test]
    fn it_parses_a_directory() {
        let input = &b"001000800000005001700008006001900025007001500044008004100059020001800100020002600118035001200144040001300156082002000169100003100189245022900220246004600449260003500495300006800530336002700598337003000625338003900655500001500694500001100709500004000720504002300760504002900783521000900812588006000821650005200881655003100933700002000964776018200984856009701166\x1E";
        assert_eq!(input.len(), 30 * 12 + 1);
        let entries = parse_directory_entries(&mut &input[..]).unwrap();
        assert_eq!(30, entries.len());
        let first = entries.first().unwrap();
        assert_eq!(&['0', '0', '1'], &first.tag);
        assert_eq!(8, first.field_length);
        assert_eq!(0, first.starting_pos);
        let last = entries.last().unwrap();
        assert_eq!((&['8', '5', '6']), &last.tag);
        assert_eq!(97, last.field_length);
        assert_eq!(1166, last.starting_pos);
    }

    #[test]
    fn it_parses_a_control_field() {
        let input = &b"B301882\0x1E";
        let entry = DirectoryEntry {
            tag: ['0', '0', '1'],
            field_length: 8,
            starting_pos: 0,
        };
        assert_eq!(
            Field::Control(ControlField {
                tag: FieldTag(['0', '0', '1']),
                data: String::from("B301882")
            }),
            parse_field(&Utf8Decoder {}, &entry, &mut &input[..]).unwrap()
        );
    }

    #[test]
    fn it_parses_a_single_record() {
        let whole_file = include_bytes!("../samples/marc8_multiple.mrc");
        let record = Record::parse_next(&mut &whole_file[0..1649]).unwrap();
        assert_eq!(CodingScheme::Marc8, record.leader.coding_scheme);
        assert_eq!(
            &Field::Data(DataField {
                tag: FieldTag(['8', '5', '6']),
                indicator: vec!['4', '0'],
                subfields: vec![
                    Subfield {
                        tag: SubfieldTag('z'),
                        data: "Accès par BIBLIUS.CA (format ePub)".to_string()
                    },
                    Subfield {
                        tag: SubfieldTag('u'),
                        data: "https://cssdm.biblius.ca/explore/products/9782896479764".to_string()
                    }
                ],
            }),
            record.fields.last().unwrap()
        )
    }

    #[test]
    fn it_parses_a_entire_set() {
        let whole_file = include_bytes!("../samples/marc8_multiple.mrc");
        let records = parse_records(whole_file).unwrap();

        assert_eq!(109, records.len())
    }
}
