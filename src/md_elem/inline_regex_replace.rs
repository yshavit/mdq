use crate::md_elem::flat_inlines::{FlattenedText, RangeReplacementError};
use crate::md_elem::tree::elem::Inline;
use std::error::Error;
use std::fmt::{Display, Formatter};

#[derive(Clone, Debug)]
pub(crate) enum RegexReplaceError {
    InvalidRegex { pattern: String, error: String },
    ReplacementError(RangeReplacementError),
}

impl Display for RegexReplaceError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RegexReplaceError::InvalidRegex { pattern, error } => write!(f, "invalid regex {pattern:?}: {error}"),
            RegexReplaceError::ReplacementError(RangeReplacementError::InternalError(e)) => {
                write!(f, "internal error: {e}")
            }
            RegexReplaceError::ReplacementError(RangeReplacementError::AtomicityViolation) => {
                write!(f, "replacement crosses atomic boundary")
            }
        }
    }
}

impl Error for RegexReplaceError {}

#[derive(Debug)]
pub(crate) struct InlineReplacements {
    pub(crate) inlines: Vec<Inline>,
    pub(crate) matched_any: bool,
}

/// Applies regex search and replace to a vector of inline elements.
///
/// This flattens the inlines, applies the regex replacement, and reconstructs
/// the tree structure. Returns an error if the regex would cross formatting
/// boundaries that cannot be represented (like links or unsupported content).
pub(crate) fn regex_replace_inlines(
    inlines: impl IntoIterator<Item = Inline>,
    pattern: &fancy_regex::Regex,
    replacement: &str,
) -> Result<InlineReplacements, RegexReplaceError> {
    // TODO should I have this take an owned Vec<Inline>? If I do, then if there are no matches I can just return the
    //  original inlines, and thus save on the unflatten step.
    let mut flattened = FlattenedText::from_inlines(inlines);

    let mut replaced_string = String::new();
    let flattened_text = flattened.text.to_string();
    let mut matched_any = false;
    for capture in pattern.captures_iter(&flattened_text) {
        matched_any = true;
        let capture = capture.map_err(|e| RegexReplaceError::InvalidRegex {
            pattern: pattern.as_str().to_string(),
            error: format!("{e}"),
        })?;
        let capture_match = capture.get(0).expect("unwrap of capture's 0-group");
        replaced_string.clear();
        capture.expand(replacement, &mut replaced_string);
        let capture_range = capture_match.start()..capture_match.end();
        flattened
            .replace_range(capture_range, &replaced_string)
            .map_err(RegexReplaceError::ReplacementError)?;
    }

    // 7. Reconstruct the inlines
    let unflattened = flattened.unflatten().map_err(RegexReplaceError::ReplacementError)?;
    Ok(InlineReplacements {
        matched_any,
        inlines: unflattened,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::md_elem::tree_test_utils::inlines;

    #[test]
    fn simple_replacement() {
        let inlines = inlines!["hello world"];
        let pattern = fancy_regex::Regex::new(r"world").unwrap();
        let result = regex_replace_inlines(inlines, &pattern, "rust").unwrap();

        assert_eq!(result.inlines, inlines!["hello rust"]);
        assert!(result.matched_any);
    }

    #[test]
    fn simple_replacement_to_same() {
        let inlines = inlines!["hello world"];
        let pattern = fancy_regex::Regex::new(r"world").unwrap();
        let result = regex_replace_inlines(inlines, &pattern, "world").unwrap();

        assert_eq!(result.inlines, inlines!["hello world"]); // same as original
        assert!(result.matched_any);
    }

    #[test]
    fn no_match_returns_original() {
        let inlines = inlines!["hello world"];
        let pattern = fancy_regex::Regex::new(r"foo").unwrap();
        let result = regex_replace_inlines(inlines.clone(), &pattern, "bar").unwrap();

        assert_eq!(result.inlines, inlines);
        assert!(!result.matched_any);
    }

    #[test]
    fn replacement_with_formatting() {
        let inlines = inlines!["before ", em["emphasized"], " after"];
        let pattern = fancy_regex::Regex::new(r"emphasized").unwrap();
        let result = regex_replace_inlines(inlines, &pattern, "replaced").unwrap();

        let expected = inlines!["before ", em["replaced"], " after"];
        assert_eq!(result.inlines, expected);
    }

    #[test]
    fn replacement_across_formatting() {
        let inlines = inlines!["before ", em["emphasized"], " after"];

        let pattern = fancy_regex::Regex::new(r"ore emphasized af").unwrap();
        let result = regex_replace_inlines(inlines, &pattern, "oo").unwrap();

        // When replacement spans formatting boundaries, formatting should be removed
        let expected = inlines!["befooter"];
        assert_eq!(result.inlines, expected);
    }

    #[test]
    fn capture_groups() {
        let inlines = inlines!["hello world"];
        let pattern = fancy_regex::Regex::new(r"(\w+) (\w+)").unwrap();
        let result = regex_replace_inlines(inlines, &pattern, "$2 $1").unwrap();

        assert_eq!(result.inlines, inlines!["world hello"]);
    }

    #[test]
    fn multiple_matches() {
        let inlines = inlines!["foo bar foo baz"];
        let pattern = fancy_regex::Regex::new(r"foo").unwrap();
        let result = regex_replace_inlines(inlines, &pattern, "qux").unwrap();

        assert_eq!(result.inlines, inlines!["qux bar qux baz"]);
        assert!(result.matched_any);
    }

    #[test]
    fn unsupported_content_error() {
        let inlines = inlines!["before ", link["link text"]("https://example.com"), " after"];

        // This should succeed because the regex doesn't cross the link boundary
        let pattern = fancy_regex::Regex::new(r"before").unwrap();
        let result = regex_replace_inlines(inlines.clone(), &pattern, "pre").unwrap();
        assert_eq!(
            result.inlines,
            inlines!["pre ", link["link text"]("https://example.com"), " after",],
        );

        // This should fail because the regex crosses into the link
        let pattern = fancy_regex::Regex::new(r"ore link").unwrap();
        let result = regex_replace_inlines(inlines, &pattern, "replacement");
        assert!(result.is_err());
    }
}
