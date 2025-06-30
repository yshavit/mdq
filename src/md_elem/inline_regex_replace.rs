use crate::md_elem::flat_inlines::{FlattenedText, FormattingType, RegexReplaceError, FlattenError};
use crate::md_elem::tree::elem::Inline;

impl From<FlattenError> for RegexReplaceError {
    fn from(_: FlattenError) -> Self {
        RegexReplaceError {}
    }
}

/// Applies regex search and replace to a vector of inline elements.
///
/// This flattens the inlines, applies the regex replacement, and reconstructs
/// the tree structure. Returns an error if the regex would cross formatting
/// boundaries that cannot be represented (like links or unsupported content).
pub(crate) fn regex_replace_inlines(
    inlines: &[Inline],
    pattern: &fancy_regex::Regex,
    replacement: &str
) -> Result<Vec<Inline>, RegexReplaceError> {
    // 1. Flatten the inlines
    let mut flattened = FlattenedText::from_inlines(inlines)?;

    // 2. Find all regex matches and collect match info to avoid borrowing issues
    let matches: Vec<_> = pattern.find_iter(&flattened.text)
        .collect::<Result<Vec<_>, _>>()
        .map_err(|_| RegexReplaceError {})?;

    // Collect match ranges and replacement texts before modifying flattened
    let mut match_info: Vec<(std::ops::Range<usize>, String)> = Vec::new();
    for mat in &matches {
        let match_text = &flattened.text[mat.start()..mat.end()];
        let replacement_text = pattern.replace(match_text, replacement).into_owned();
        match_info.push((mat.start()..mat.end(), replacement_text));
    }

    // 3. Check if any matches cross unsupported formatting boundaries
    for (range, _) in &match_info {
        let match_start = range.start;
        let match_end = range.end;

        for event in &flattened.formatting_events {
            if matches!(event.formatting, FormattingType::Unsupported) {
                let event_start = event.start_pos;
                let event_end = event.start_pos + event.length;

                // Check if the match overlaps with this unsupported event
                if match_start < event_end && match_end > event_start {
                    return Err(RegexReplaceError {});
                }
            }
        }
    }

    // 4. If no matches found, return the original inlines unchanged
    if match_info.is_empty() {
        return Ok(inlines.to_vec());
    }

    // 5. Apply range replacements to update formatting events
    // We need to apply them in reverse order to maintain correct positions
    for (range, replacement_text) in match_info.iter().rev() {
        flattened.replace_range(range.clone(), replacement_text)
            .map_err(|_| RegexReplaceError {})?;
    }

    // 6. Remove any remaining unsupported events (these weren't affected by replacements)
    flattened.formatting_events.retain(|event| {
        !matches!(event.formatting, FormattingType::Unsupported)
    });

    // 7. Reconstruct the inlines
    flattened.unflatten()
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::md_elem::tree_test_utils::inlines;

    #[test]
    fn simple_replacement() {
        let inlines = inlines!["hello world"];
        let pattern = fancy_regex::Regex::new(r"world").unwrap();
        let result = regex_replace_inlines(&inlines, &pattern, "rust").unwrap();

        assert_eq!(result, inlines!["hello rust"]);
    }

    #[test]
    fn no_match_returns_original() {
        let inlines = inlines!["hello world"];
        let pattern = fancy_regex::Regex::new(r"foo").unwrap();
        let result = regex_replace_inlines(&inlines, &pattern, "bar").unwrap();

        assert_eq!(result, inlines);
    }

    #[test]
    fn replacement_with_formatting() {
        let inlines = inlines![
            "before ",
            em["emphasized"],
            " after"
        ];
        let pattern = fancy_regex::Regex::new(r"emphasized").unwrap();
        let result = regex_replace_inlines(&inlines, &pattern, "replaced").unwrap();

        let expected = inlines![
            "before ",
            em["replaced"],
            " after"
        ];
        assert_eq!(result, expected);
    }

    #[test]
    fn replacement_across_formatting() {
        let inlines = inlines![
            "before ",
            em["emphasized"],
            " after"
        ];

        let pattern = fancy_regex::Regex::new(r"ore emphasized af").unwrap();
        let result = regex_replace_inlines(&inlines, &pattern, "oo").unwrap();

        // When replacement spans formatting boundaries, formatting should be removed
        let expected = inlines!["befooter"];
        assert_eq!(result, expected);
    }

    #[test]
    fn capture_groups() {
        let inlines = inlines!["hello world"];
        let pattern = fancy_regex::Regex::new(r"(\w+) (\w+)").unwrap();
        let result = regex_replace_inlines(&inlines, &pattern, "$2 $1").unwrap();

        assert_eq!(result, inlines!["world hello"]);
    }

    #[test]
    fn multiple_matches() {
        let inlines = inlines!["foo bar foo baz"];
        let pattern = fancy_regex::Regex::new(r"foo").unwrap();
        let result = regex_replace_inlines(&inlines, &pattern, "qux").unwrap();

        assert_eq!(result, inlines!["qux bar qux baz"]);
    }

    #[test]
    fn unsupported_content_error() {


        let inlines = inlines![
            "before ",
            link["link text"] "https://example.com",
            " after"
        ];

        // Debug what the flattened representation looks like
        let flattened = FlattenedText::from_inlines(&inlines).unwrap();
        println!("Text: {:?}", flattened.text);
        println!("Events: {:?}", flattened.formatting_events);

        // This should succeed because the regex doesn't cross the link boundary
        let pattern = fancy_regex::Regex::new(r"before").unwrap();
        let result = regex_replace_inlines(&inlines, &pattern, "after");
        println!("Result: {:?}", result);
        assert!(result.is_ok());

        // This should fail because the regex crosses into the link
        let pattern = fancy_regex::Regex::new(r"ore link").unwrap();
        let result = regex_replace_inlines(&inlines, &pattern, "replacement");
        assert!(result.is_err());
    }
}
