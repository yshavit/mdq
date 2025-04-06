use crate::query::strings::{ParsedString, ParsedStringMode};
use crate::query::{DetachedSpan, Pair, ParseError};
use crate::select::{AnyVariant, Matcher};
use regex::Regex;

impl TryFrom<Option<Pair<'_>>> for Matcher {
    type Error = ParseError;

    fn try_from(pair: Option<Pair>) -> Result<Self, Self::Error> {
        let Some(pair) = pair else {
            return Ok(Self::Any(AnyVariant::Implicit));
        };
        let span = DetachedSpan::from(&pair);
        let parsed_string: ParsedString = pair.into_inner().try_into()?;
        if parsed_string.is_equivalent_to_asterisk() {
            let any_variant = if parsed_string.explicit_wildcard {
                AnyVariant::Explicit
            } else {
                AnyVariant::Implicit
            };
            return Ok(Self::Any(any_variant));
        }
        let matcher = match parsed_string.mode {
            ParsedStringMode::CaseSensitive => Self::Text {
                case_sensitive: true,
                anchor_start: parsed_string.anchor_start,
                text: parsed_string.text,
                anchor_end: parsed_string.anchor_end,
            },
            ParsedStringMode::CaseInsensitive => Self::Text {
                case_sensitive: false,
                anchor_start: parsed_string.anchor_start,
                text: parsed_string.text,
                anchor_end: parsed_string.anchor_end,
            },
            ParsedStringMode::Regex => {
                let re = Regex::new(&parsed_string.text).map_err(|e| ParseError::Other(span.into(), e.to_string()))?;
                Self::Regex(re)
            }
        };
        Ok(matcher)
    }
}
