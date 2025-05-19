use crate::query::strings::{ParsedString, ParsedStringMode};
use crate::query::{DetachedSpan, InnerParseError, Pair};
use crate::select::{MatchReplace, Matcher, Regex};
use fancy_regex::Error;

impl MatchReplace {
    pub(crate) fn try_from(pair: Option<Pair>) -> Result<Self, InnerParseError> {
        let Some(pair) = pair else {
            return Ok(Self {
                matcher: Matcher::Any { explicit: false },
                replacement: None,
            });
        };
        let span = DetachedSpan::from(&pair);
        let parsed_string = ParsedString::new_from_pairs(pair.into_inner())?;
        if parsed_string.is_equivalent_to_asterisk() {
            return Ok(Self {
                matcher: Matcher::Any {
                    explicit: parsed_string.explicit_wildcard,
                },
                replacement: None,
            });
        }
        let matcher = match parsed_string.mode {
            ParsedStringMode::CaseSensitive => Matcher::Text {
                case_sensitive: true,
                anchor_start: parsed_string.anchor_start,
                text: parsed_string.text,
                anchor_end: parsed_string.anchor_end,
            },
            ParsedStringMode::CaseInsensitive => Matcher::Text {
                case_sensitive: false,
                anchor_start: parsed_string.anchor_start,
                text: parsed_string.text,
                anchor_end: parsed_string.anchor_end,
            },
            ParsedStringMode::Regex => {
                let re = fancy_regex::Regex::new(&parsed_string.text).map_err(|e| {
                    match e {
                        Error::ParseError(pos, err) => {
                            let mut re_span = span;
                            re_span.start += pos + 1; // +1 for the regex's opening slash
                            re_span.end = re_span.start;
                            InnerParseError::Other(re_span, format!("regex parse error: {err}"))
                        }
                        err => {
                            // not expected, but we'll handle it anyway
                            InnerParseError::Other(span, err.to_string())
                        }
                    }
                })?;
                Matcher::Regex(Regex { re })
            }
        };
        Ok(Self {
            matcher,
            replacement: parsed_string.replace_string,
        })
    }
}
