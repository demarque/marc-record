//! This crates provides means to parse MARC21 records. It supports normal MARC21 records
//! using either MARC-8 (for latin languages) or Unicode and tries to transform as much as
//! possible into strings. It doesn't interpret the field data much, so lookup from tag numbers will be required
//!
//! Info about the format can be found here: https://www.loc.gov/marc/bibliographic/
//!
//! The general structure of a MARC record is as follows:
//!
//! A file can contain many MARC records. Each records has the following parts:
//! - a leader: a header that contains info about the structure of the record;
//! - a directory: an index of the various fields;
//! - fields, which can either be control fields or data fields
//!
//! All the fields have an identifying tag.
//!
//! Control fields simply contain ASCII data.
//!
//! Each data field can have a 2-character set of indicators, for which some meaning can be derived.
//!
//! They also contain a list of subfields which are identified by a single ASCII character.
//!
//! The only entrypoint to the library is the parse_records function:
//! ```rust
//! use marc_record::parse_records;
//!
//! let binary_data = include_bytes!("../samples/marc8_multiple.mrc");
//! let records = parse_records(binary_data).unwrap();
//! assert_eq!(records.len(), 109);
//! ```

// Support for the MARC-8 encoding format
pub mod marc8;
// Parsing rules
mod parser;

use core::str;
use std::{
    borrow::Cow,
    fmt::{Display, Write},
    str::Utf8Error,
};

use marc8::Marc8Decoder;
use thiserror::Error;
use winnow::{
    combinator::repeat,
    error::{ContextError, ParseError, StrContext},
    Parser,
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
    Ok(repeat(
        0..,
        parser::parse_record.context(StrContext::Label("record")),
    )
    .parse(data)?)
}

// Represents a parsing error
#[derive(thiserror::Error, Debug)]
pub enum Error {
    #[error("Failed to parse: {} at byte offset `{}`", reason, offset)]
    ParseFailed { reason: String, offset: usize },
}

impl From<ParseError<&[u8], ContextError>> for Error {
    fn from(value: ParseError<&[u8], ContextError>) -> Self {
        Self::ParseFailed {
            reason: value.inner().to_string(),
            offset: value.offset(),
        }
    }
}

/// A MARC record describes a content or piece of content using a series of fields.
/// Fields are identified by a three-digit ASCII code (e.g. `001`) and contain
/// either control or field data. Control fields are identified by being in the 000-099 range
/// while data fields are all the others.
/// Control fields contain a single piece of information
pub struct Record {
    /// The leader is MARC's way of naming the header info
    pub leader: Leader,
    /// The actual control and data fields
    pub fields: Vec<Field>,
}

/// The leader is MARC's equivalent of a header. It contains internal bookkeeping info about the record
/// as well as some information of interest to the applications reading it.
#[derive(Debug)]
pub struct Leader {
    /// Length in bytes of the record
    pub record_length: u16,

    /// The status of the information, e.g. whether the sender signals it is a new entry or a correction
    pub status: Status,

    /// The type of content described by the record, will alter which fields *should* be provided.
    pub record_type: RecordType,

    /// Mostly used to tell whether this is for a single item or a collection
    pub bibliographical_level: BibliographicalLevel,

    pub control_type: ControlType,

    /// Tells which encoding was used for the text of the variable data fields.
    pub coding_scheme: CodingScheme,

    /// Byte index of the variable field data, after the leader and the dictionary
    pub data_base_address: u16,

    /// Represents something like the quality of the data about the entry
    pub encoding_level: EncodingLevel,
    pub descriptive_cataloging_form: CatalogingForm,
    pub multipart_resource_record_level: MultipartResourceRecordLevel,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Status {
    IncreaseInEncoding,
    Corrected,
    Deleted,
    New,
    IncreaseFromPrepublication,
}

#[derive(Debug, PartialEq, Eq)]
pub enum RecordType {
    LanguageMaterial,
    NotatedMusic,
    ManuscriptNotatedMusic,
    CartographicMaterial,
    ManuscriptCartographicMaterial,
    ProjectedMedium,
    NonmusicalSoundRecording,
    MusicalSoundRecording,
    TwoDimensionalNonprojectableGraphic,
    ComputerFile,
    Kit,
    MixedMaterials,
    ThreeDimensionalArtifact,
    ManuscriptLanguageMaterial,
}

#[derive(Debug, PartialEq, Eq)]
pub enum BibliographicalLevel {
    MonographicComponentPart,
    SerialComponentPart,
    Collection,
    Subunit,
    IntegratingResource,
    Monograph,
    Serial,
    Unknown,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ControlType {
    Unspecified,
    Archival,
}

#[derive(Debug, PartialEq, Eq)]
pub enum CodingScheme {
    /// The MARC-8 Encoding scheme
    Marc8,
    /// Unicode code points encoded as UTF-8
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

#[derive(Debug, PartialEq, Eq)]
pub enum EncodingLevel {
    Full,
    FullMaterialNotExamined,
    LessThanFullMaterialNotExamined,
    Abbreviated,
    Core,
    Partial,
    Minimal,
    Prepublication,
    Unknown,
    NotApplicable,
    ObsoleteFull,
    ObsoleteMinimal,
    AddedFromBatch,
}

#[derive(Debug, PartialEq, Eq)]
pub enum CatalogingForm {
    NonIsbd,
    Aacr2,
    IsbdPunctuationOmitted,
    IsbdPunctuationIncluded,
    NonIsbdPunctuationOmitted,
    Unknown,
}

#[derive(Debug, PartialEq, Eq)]
pub enum MultipartResourceRecordLevel {
    NotApplicable,
    Set,
    PartWithIndependentTitle,
    PartwithDependentTitle,
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

#[derive(Debug, Error)]
enum DecodeError {
    #[error("UTF-8 error: {0}")]
    Utf(Utf8Error),
    #[error("Unknown char: {0}")]
    Unknown(u8),
    #[error("Invalid pair: base `{0}` with combining `{1}`")]
    InvalidPair(char, char),
    #[error("Invalid character sequence")]
    InvalidSequence,
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
    /// A quick accessor to get the control field variant if the field is actually one.
    pub fn control(&self) -> Option<&ControlField> {
        match self {
            Field::Control(control_field) => Some(control_field),
            Field::Data(_) => None,
        }
    }

    /// A quick accessor to get the data field variant if the field is actually one.
    pub fn data(&self) -> Option<&DataField> {
        match self {
            Field::Control(_) => None,
            Field::Data(data_field) => Some(data_field),
        }
    }
}

/// The first fields of a MARC record represent control data. Unlike
/// the variable data fields, they are simple blob of ASCII content, although
/// some of them are encoded with a specific scheme (for example, some of them are pipe-separated values)
#[derive(Debug, PartialEq, Eq)]
pub struct ControlField {
    pub tag: FieldTag,
    pub data: String,
}

/// One of the variable data fields representing the bulk of the information
/// found in the MARC record. The tag along with the indicators (of which there are typically 2) help
/// figure out the specific meaning of the data found within the subfields or blocks of content.
#[derive(Debug, PartialEq, Eq)]
pub struct DataField {
    pub tag: FieldTag,
    pub indicator: Vec<char>,
    pub subfields: Vec<Subfield>,
}

/// A type representing the function of a block of content
/// within a variable data field. Typically a single character.
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
