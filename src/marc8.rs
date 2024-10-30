//! MARC-8 support for MARC records

use core::str;

use unicode_normalization::char::compose;

use crate::{DecodeError, TextDecoder};

/// A MARC-8 decoder for latin language text.
///
/// Info about decoding MARC-8 are scarce, but some sources were:
///
/// - https://en.wikipedia.org/wiki/MARC-8
/// - https://en.wikipedia.org/wiki/ANSEL
///
pub struct Marc8Decoder {}

impl TextDecoder for Marc8Decoder {
    /// Tries to decode and transform text into valid UTF-8
    ///
    /// If the text is entirely ASCII, the result will simply be a reference to the original string
    fn decode<'a>(&self, text: &'a [u8]) -> Result<std::borrow::Cow<'a, str>, DecodeError> {
        // Check if all we received in practice is ASCII, which means that we can just the array as is.
        // This could be optimized using SIMD
        if text.iter().all(|ch| *ch <= 127) {
            return Ok(std::borrow::Cow::Borrowed(
                str::from_utf8(text).map_err(|e| DecodeError::Utf(e))?,
            ));
        }

        let mut out = String::with_capacity(text.len());
        let mut it = text.into_iter();

        let mut combining_buffer: Vec<char> = Vec::new();
        while let Some(ch) = it.next() {
            // The basic set is the same as ASCII and thus uses the same unicode code points
            if *ch <= 127 {
                if combining_buffer.is_empty() {
                    out.push(char::from_u32(*ch as u32).unwrap());
                    continue;
                }

                let mut base =
                    char::from_u32(*ch as u32).expect("we already checked it was below 128");
                // we had one or more combining characters stacked, consume them and try to form a valid unicode sequence

                let mut combining_it = combining_buffer.iter();
                while let Some(combining) = combining_it.next() {
                    if let Some(new) = compose(base, *combining) {
                        base = new;
                    } else {
                        return Err(DecodeError::InvalidPair(base, *combining));
                    }
                }

                out.push(base);
                combining_buffer.clear();
                continue;
            }

            if *ch >= 0xA1 && *ch <= 0xC8 {
                match ch {
                    0xA1 => out.push('\u{0141}'),
                    0xA2 => out.push('\u{00D8}'),
                    0xA3 => out.push('\u{0110}'),
                    0xA4 => out.push('\u{00DE}'),
                    0xA5 => out.push('\u{00C6}'),
                    0xA6 => out.push('\u{0152}'),
                    0xA7 => out.push('\u{02B9}'),
                    0xA8 => out.push('\u{00B7}'),
                    0xA9 => out.push('\u{266D}'),
                    0xAA => out.push('\u{00AE}'),
                    0xAB => out.push('\u{00B1}'),
                    0xAC => out.push('\u{01A0}'),
                    0xAD => out.push('\u{01AF}'),
                    0xAE => out.push('\u{02BC}'),
                    0xB0 => out.push('\u{02BB}'),
                    0xB1 => out.push('\u{0142}'),
                    0xB2 => out.push('\u{00F8}'),
                    0xB3 => out.push('\u{0111}'),
                    0xB4 => out.push('\u{00FE}'),
                    0xB5 => out.push('\u{00E6}'),
                    0xB6 => out.push('\u{0153}'),
                    0xB7 => out.push('\u{02BA}'),
                    0xB8 => out.push('\u{0131}'),
                    0xB9 => out.push('\u{00A3}'),
                    0xBA => out.push('\u{00F0}'),
                    0xBC => out.push('\u{01A1}'),
                    0xBD => out.push('\u{01B0}'),
                    0xC0 => out.push('\u{00B0}'),
                    0xC1 => out.push('\u{2113}'),
                    0xC2 => out.push('\u{2117}'),
                    0xC3 => out.push('\u{00A9}'),
                    0xC4 => out.push('\u{266F}'),
                    0xC5 => out.push('\u{00BF}'),
                    0xC6 => out.push('\u{00A1}'),
                    0xC7 => out.push('\u{00DF}'), // Not clear if it's supped to be lowercase of uppercase, I put lower
                    0xC8 => out.push('\u{20AC}'), // Euro sign
                    _ => return Err(DecodeError::Unknown(*ch)),
                };
                continue;
            }

            // Combinining characters
            if *ch >= 0xE0 && *ch <= 0xFE {
                // let Some((_, n)) = it.next() else {
                //     return Err(DecodeError::Unknown(*ch));
                // };

                // let Some(base_char) = char::from_u32(*n as u32) else {
                //     return Err(DecodeError::Unknown(*ch));
                // };

                let combining = match ch {
                    0xE0 => '\u{0309}',
                    0xE1 => '\u{0300}',
                    0xE2 => '\u{0301}',
                    0xE3 => '\u{0302}',
                    0xE4 => '\u{0303}',
                    0xE5 => '\u{0304}',
                    0xE6 => '\u{0306}',
                    0xE7 => '\u{0307}',
                    0xE8 => '\u{0308}',
                    0xE9 => '\u{030C}',
                    0xEA => '\u{030A}',
                    0xEB => '\u{FE20}',
                    0xEC => '\u{FE21}',
                    0xED => '\u{0315}',
                    0xEE => '\u{030B}',
                    0xEF => '\u{0310}',
                    0xF0 => '\u{0327}',
                    0xF1 => '\u{0328}',
                    0xF2 => '\u{0323}',
                    0xF3 => '\u{0324}',
                    0xF4 => '\u{0325}',
                    0xF5 => '\u{0333}',
                    0xF6 => '\u{0332}',
                    0xF7 => '\u{0326}',
                    0xF8 => '\u{031C}',
                    0xF9 => '\u{032E}',
                    0xFA => '\u{FE22}',
                    0xFB => '\u{FE23}',
                    0xFE => '\u{0313}',
                    _ => return Err(DecodeError::Unknown(*ch)),
                };

                combining_buffer.push(combining);

                continue;
            }

            return Err(DecodeError::Unknown(*ch));
        }

        // If we're at the end of the string and we were working on a combining sequence, something went wrong
        if !combining_buffer.is_empty() {
            return Err(DecodeError::InvalidSequence);
        }

        return Ok(std::borrow::Cow::Owned(out));
    }
}
