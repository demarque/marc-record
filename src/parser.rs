use std::borrow::Cow;

use winnow::{
    ascii::digit1,
    combinator::{fail, repeat, separated, seq},
    error::{AddContext, ContextError, ErrMode, FromExternalError, StrContext, StrContextValue},
    stream::Stream,
    token::{any, one_of, take, take_while},
    PResult, Parser,
};

use crate::{
    BibliographicalLevel, CatalogingForm, CodingScheme, ControlField, ControlType, DataField,
    DirectoryEntry, EncodingLevel, Field, FieldTag, Leader, MultipartResourceRecordLevel, Record,
    RecordType, Status, Subfield, SubfieldTag, TextDecoder, FIELD_SEPARATOR, RECORD_SEPARATOR,
    SUBFIELD_SEPARATOR,
};

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
        crate::SUBFIELD_SEPARATOR
            .context(StrContext::Label("subfield separator"))
            .parse_next(input)?;
        let subfields = separated(1.., parse_subfield(decoder), SUBFIELD_SEPARATOR)
            .context(StrContext::Label("subfields"))
            .parse_next(input)?;
        #[allow(const_item_mutation)]
        crate::FIELD_SEPARATOR
            .context(StrContext::Label("field separator"))
            .parse_next(input)?;
        Ok(Field::Data(DataField {
            indicator,
            tag: FieldTag(entry.tag.clone()),
            subfields,
        }))
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

fn parse_coding_scheme(input: &mut &[u8]) -> PResult<CodingScheme> {
    // SAFETY: the `as char` is safe because we know it's one of the limited values
    let ch = one_of([' ', 'a']).parse_next(input).map_err(ErrMode::cut)? as char;

    match ch {
        ' ' => Ok(CodingScheme::Marc8),
        'a' => Ok(CodingScheme::Ucs),
        _ => fail(input),
    }
}

fn parse_status(input: &mut &[u8]) -> PResult<Status> {
    // SAFETY: the `as char` is safe because we know it's one of the limited values
    let ch = one_of(['a', 'c', 'd', 'n', 'p'])
        .parse_next(input)
        .map_err(ErrMode::cut)? as char;

    match ch {
        'a' => Ok(Status::IncreaseInEncoding),
        'c' => Ok(Status::Corrected),
        'd' => Ok(Status::Deleted),
        'n' => Ok(Status::New),
        'p' => Ok(Status::IncreaseFromPrepublication),
        _ => fail(input),
    }
}

fn parse_record_type(input: &mut &[u8]) -> PResult<RecordType> {
    // SAFETY: the `as char` is safe because we know it's one of the limited values
    let ch = one_of([
        'a', 'c', 'd', 'e', 'f', 'g', 'i', 'j', 'k', 'm', 'o', 'p', 'r', 't',
    ])
    .parse_next(input)
    .map_err(ErrMode::cut)? as char;

    match ch {
        'a' => Ok(RecordType::LanguageMaterial),
        'c' => Ok(RecordType::NotatedMusic),
        'd' => Ok(RecordType::ManuscriptNotatedMusic),
        'e' => Ok(RecordType::CartographicMaterial),
        'f' => Ok(RecordType::ManuscriptCartographicMaterial),
        'g' => Ok(RecordType::ProjectedMedium),
        'i' => Ok(RecordType::NonmusicalSoundRecording),
        'j' => Ok(RecordType::MusicalSoundRecording),
        'k' => Ok(RecordType::TwoDimensionalNonprojectableGraphic),
        'm' => Ok(RecordType::ComputerFile),
        'o' => Ok(RecordType::Kit),
        'p' => Ok(RecordType::MixedMaterials),
        'r' => Ok(RecordType::ThreeDimensionalArtifact),
        't' => Ok(RecordType::ManuscriptLanguageMaterial),
        _ => fail(input),
    }
}

fn parse_control_type(input: &mut &[u8]) -> PResult<ControlType> {
    // SAFETY: the `as char` is safe because we know it's one of the limited values
    let ch = one_of([' ', 'a']).parse_next(input)? as char;
    match ch {
        ' ' => Ok(ControlType::Unspecified),
        'a' => Ok(ControlType::Archival),
        _ => fail(input),
    }
}

fn parse_bibliographical_level(input: &mut &[u8]) -> PResult<BibliographicalLevel> {
    // SAFETY: the `as char` is safe because we know it's one of the limited values
    let ch = one_of(['a', 'b', 'c', 'd', 'i', 'm', 's', ' ']).parse_next(input)? as char;

    match ch {
        'a' => Ok(BibliographicalLevel::MonographicComponentPart),
        'b' => Ok(BibliographicalLevel::SerialComponentPart),
        'c' => Ok(BibliographicalLevel::Collection),
        'd' => Ok(BibliographicalLevel::Subunit),
        'i' => Ok(BibliographicalLevel::IntegratingResource),
        'm' => Ok(BibliographicalLevel::Monograph),
        's' => Ok(BibliographicalLevel::Serial),
        ' ' => Ok(BibliographicalLevel::Unknown),
        _ => fail(input),
    }
}

fn parse_encoding_level(input: &mut &[u8]) -> PResult<EncodingLevel> {
    // SAFETY: the `as char` is safe because we know it's one of the limited values
    let ch = one_of([
        ' ', '1', '2', '3', '4', '5', '7', '8', 'u', 'z', 'I', 'k', 'K', 'M',
        // Found in the wild, value unknown
        'a', 'i', 'J', 'C', 'L', '|', 'X', '0', 'T',
    ])
    .context(StrContext::Label("encoding level"))
    .parse_next(input)
    .map_err(ErrMode::cut)? as char;

    match ch {
        ' ' => Ok(EncodingLevel::Full),
        '1' => Ok(EncodingLevel::FullMaterialNotExamined),
        '2' => Ok(EncodingLevel::LessThanFullMaterialNotExamined),
        '3' => Ok(EncodingLevel::Abbreviated),
        '4' => Ok(EncodingLevel::Core),
        '5' => Ok(EncodingLevel::Partial),
        '7' => Ok(EncodingLevel::Minimal),
        '8' => Ok(EncodingLevel::Prepublication),
        'u' => Ok(EncodingLevel::Unknown),
        'z' => Ok(EncodingLevel::NotApplicable),
        'i' | 'I' => Ok(EncodingLevel::ObsoleteFull),
        'k' | 'K' => Ok(EncodingLevel::ObsoleteMinimal),
        'M' => Ok(EncodingLevel::AddedFromBatch),
        '0' | 'a' | 'J' | 'L' | 'C' | '|' | 'X' | 'T' => Ok(EncodingLevel::Unknown),
        _ => fail(input),
    }
}

fn parse_descriptive_cataloging_form(input: &mut &[u8]) -> PResult<CatalogingForm> {
    // SAFETY: the `as char` is safe because we know it's one of the limited values
    let ch = one_of([
        ' ', 'a', 'c', 'C', 'i', 'n', 'u', // Found in the wild, value unknown
        's', '|', 'd', 'q', 'I',
    ])
    .parse_next(input)
    .map_err(ErrMode::cut)? as char;

    match ch {
        ' ' => Ok(CatalogingForm::NonIsbd),
        'a' => Ok(CatalogingForm::Aacr2),
        'c' | 'C' => Ok(CatalogingForm::IsbdPunctuationOmitted),
        'i' | 'I' => Ok(CatalogingForm::IsbdPunctuationIncluded),
        'n' => Ok(CatalogingForm::NonIsbdPunctuationOmitted),
        'u' | 's' | '|' | 'd' | 'q' => Ok(CatalogingForm::Unknown),

        _ => fail(input),
    }
}

fn parse_multipart_resource_record_level(
    input: &mut &[u8],
) -> PResult<MultipartResourceRecordLevel> {
    // SAFETY: the `as char` is safe because we know it's one of the limited values
    let ch = one_of([' ', 'a', 'b', 'c'])
        .parse_next(input)
        .map_err(ErrMode::cut)? as char;

    match ch {
        ' ' => Ok(MultipartResourceRecordLevel::NotApplicable),
        'a' => Ok(MultipartResourceRecordLevel::Set),
        'b' => Ok(MultipartResourceRecordLevel::PartWithIndependentTitle),
        'c' => Ok(MultipartResourceRecordLevel::PartWithDependentTitle),

        _ => fail(input),
    }
}

fn parse_fixed_num<const I: usize>(input: &mut &[u8]) -> PResult<u16> {
    take(I).and_then(digit1).parse_to().parse_next(input)
}

pub fn parse_leader(input: &mut &[u8]) -> PResult<Leader> {
    let length = parse_fixed_num::<5>
        .context(StrContext::Label("record length"))
        .context(StrContext::Expected(StrContextValue::Description(
            "integer length",
        )))
        .parse_next(input)?;
    let status = parse_status
        .context(StrContext::Label("status"))
        .parse_next(input)?;
    let record_type = parse_record_type
        .context(StrContext::Label("record type"))
        .parse_next(input)?;
    let bibliographical_level = parse_bibliographical_level
        .context(StrContext::Label("bibliographical level"))
        .parse_next(input)?;
    let control_type = parse_control_type
        .context(StrContext::Label("control type"))
        .parse_next(input)?;
    let coding_scheme = parse_coding_scheme
        .context(StrContext::Label("coding scheme"))
        .parse_next(input)?;
    '2'.context(StrContext::Label("indicator count"))
        .parse_next(input)?; // Indicator count
    '2'.context(StrContext::Label("subfield code count"))
        .parse_next(input)?; // Subfield code count
    let data_base_address = parse_fixed_num::<5>
        .context(StrContext::Label("data base address"))
        .parse_next(input)?;
    let encoding_level = parse_encoding_level
        .context(StrContext::Label("encoding level"))
        .parse_next(input)?;
    let descriptive_cataloging_form = parse_descriptive_cataloging_form
        .context(StrContext::Label("descriptive cataloging form"))
        .parse_next(input)?;
    let multipart_resource_record_level = parse_multipart_resource_record_level
        .context(StrContext::Label("multipart resource record level"))
        .parse_next(input)?;
    '4'.parse_next(input)?;
    '5'.parse_next(input)?;
    '0'.parse_next(input)?;
    '0'.parse_next(input)?;

    Ok(Leader {
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

pub fn parse_record(input: &mut &[u8]) -> PResult<Record> {
    let leader = parse_leader
        .context(StrContext::Label("leader"))
        .parse_next(input)?;
    let directory = parse_directory_entries
        .context(StrContext::Label("directory"))
        .parse_next(input)
        .map_err(ErrMode::cut)?;
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

#[cfg(test)]
mod tests {
    use crate::{parse_records, Utf8Decoder};

    use super::*;

    #[test]
    fn it_parses_a_leader() {
        let input = &b"01649cam  22003855a 4500";
        let leader = parse_leader(&mut &input[..]).unwrap();
        assert_eq!(leader.record_length, 1649);
        assert_eq!(leader.status, Status::Corrected);
        assert_eq!(leader.coding_scheme, CodingScheme::Marc8);
        assert_eq!(leader.data_base_address, 385);
    }

    #[test]
    fn it_parses_another_leader() {
        let input = &b"01631ca  a2200409a  4500";
        let leader = parse_leader(&mut &input[..]).unwrap();
        assert_eq!(leader.record_length, 1631);
        assert_eq!(leader.status, Status::Corrected);
        assert_eq!(leader.coding_scheme, CodingScheme::Ucs);
        assert_eq!(leader.data_base_address, 409);
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
        let record = parse_record(&mut &whole_file[0..1649]).unwrap();
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
