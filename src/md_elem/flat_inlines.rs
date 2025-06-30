use crate::md_elem::tree::elem::{Inline, SpanVariant, Text, TextVariant};

/// The type of formatting to apply to a range of text.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum FormattingType {
    /// Standard span formatting (emphasis, strong, delete)
    Span(SpanVariant),
    /// Unsupported content that cannot be processed by regex replacement
    Unsupported,
}

/// A flattened representation of inline markdown that separates plain text from formatting.
///
/// This allows regex operations to work on the plain text while preserving formatting
/// information that can be reapplied after the text transformation.
#[derive(Debug, Clone, PartialEq)]
pub struct FlattenedText {
    /// The plain text content with all formatting removed
    pub text: String,
    /// Formatting events that describe where formatting should be applied to the text
    pub formatting_events: Vec<FormattingEvent>,
    /// Cumulative offset from range replacements (for tracking coordinate mapping)
    offset: isize,
    /// The end position of the last replacement (in original coordinates) to enforce ordering
    last_replacement_end: usize,
}

/// Describes a span of formatting that should be applied to a range of characters.
///
/// Events are applied in order, so earlier events become outer spans in nested formatting.
#[derive(Debug, Clone, PartialEq)]
pub struct FormattingEvent {
    /// Starting character position in the flattened text (inclusive)
    pub start_pos: usize,
    /// The number of characters this formatting applies to
    pub length: usize,
    /// The type of formatting to apply
    pub formatting: FormattingType,
}

/// Error that occurs when trying to flatten inlines that contain non-flattenable content.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct FlattenError {
    // TODO: Add specific error variants and details
}

/// Error that occurs during regex replacement operations.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct RegexReplaceError {
    // TODO: Add specific error variants and details
}

/// Error that occurs during range replacement operations.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct RangeReplacementError {
    // TODO: Add specific error variants and details
}

impl FlattenedText {
    /// Creates a flattened representation from a slice of inline elements.
    ///
    /// This extracts all plain text content and creates formatting events that describe
    /// where spans should be applied. Links, images, and non-plain text variants will
    /// cause a FlattenError.
    pub(crate) fn from_inlines(inlines: &[Inline]) -> Result<Self, FlattenError> {
        let mut text = String::new();
        let mut formatting_events = Vec::new();

        flatten_inlines_recursive(inlines, &mut text, &mut formatting_events)?;

        Ok(FlattenedText {
            text,
            formatting_events,
            offset: 0,
            last_replacement_end: 0,
        })
    }

    /// Reconstructs inline elements from the flattened representation.
    ///
    /// This applies the formatting events to the text to rebuild the original
    /// tree structure. Events marked as `FormattingType::Unsupported` will cause
    /// a reconstruction error since we cannot recreate the original atomic elements.
    pub(crate) fn unflatten(&self) -> Result<Vec<Inline>, RegexReplaceError> {
        // Check for unsupported formatting
        for event in &self.formatting_events {
            if matches!(event.formatting, FormattingType::Unsupported) {
                return Err(RegexReplaceError {});
            }
        }

        unflatten_recursive(&self.text, &self.formatting_events, 0, self.text.len())
    }

    /// Replaces a range of text using original coordinates.
    ///
    /// The range must be non-overlapping and in increasing order relative to previous
    /// calls to this method. This updates both the text and adjusts formatting events.
    pub(crate) fn replace_range(
        &mut self,
        original_range: std::ops::Range<usize>,
        replacement: &str,
    ) -> Result<(), RangeReplacementError> {
        // Validate that ranges are non-overlapping and in increasing order
        // by checking that the current range start is not before the end of the last replacement
        if original_range.start < self.last_replacement_end {
            return Err(RangeReplacementError {});
        }

        let current_start = (original_range.start as isize + self.offset) as usize;
        let current_end = (original_range.end as isize + self.offset) as usize;

        // Validate range is within bounds and not going backwards
        if current_end > self.text.len() || current_start > self.text.len() {
            return Err(RangeReplacementError {});
        }

        // Replace the text
        self.text.replace_range(current_start..current_end, replacement);

        // Calculate the size change
        let original_len = original_range.end - original_range.start;
        let size_change = replacement.len() as isize - original_len as isize;

        // Update formatting events based on how they overlap with the replacement
        let mut events_to_keep = Vec::new();

        for event in &self.formatting_events {
            let event_start = event.start_pos;
            let event_end = event.start_pos + event.length;

            if event_end <= original_range.start {
                // Event is completely before the replacement - keep unchanged
                events_to_keep.push(event.clone());
            } else if event_start >= original_range.end {
                // Event is completely after the replacement - shift by size change
                let mut new_event = event.clone();
                new_event.start_pos = (event_start as isize + size_change) as usize;
                events_to_keep.push(new_event);
            } else if event_start >= original_range.start && event_end <= original_range.end {
                // Event is completely within the replacement - remove it
                // (Don't add to events_to_keep)
            } else if event_start < original_range.start && event_end > original_range.end {
                // Event spans the entire replacement - adjust length
                let mut new_event = event.clone();
                new_event.length = (event.length as isize + size_change) as usize;
                events_to_keep.push(new_event);
            } else if event_start < original_range.start && event_end <= original_range.end {
                // Event starts before replacement and ends within it - truncate
                let mut new_event = event.clone();
                new_event.length = original_range.start - event_start;
                events_to_keep.push(new_event);
            } else if event_start >= original_range.start && event_end > original_range.end {
                // Event starts within replacement and ends after it - shift and truncate
                let mut new_event = event.clone();
                let chars_removed_from_event = original_range.end - event_start;
                new_event.start_pos = (original_range.start as isize + replacement.len() as isize) as usize;
                new_event.length = event.length - chars_removed_from_event;
                events_to_keep.push(new_event);
            }
        }

        self.formatting_events = events_to_keep;

        // Update the cumulative offset
        self.offset += size_change;

        // Update the last replacement end position
        self.last_replacement_end = original_range.end;

        Ok(())
    }
}

/// Recursively flattens inlines, building up the text and formatting events.
fn flatten_inlines_recursive(
    inlines: &[Inline],
    text: &mut String,
    formatting_events: &mut Vec<FormattingEvent>,
) -> Result<(), FlattenError> {
    for inline in inlines {
        match inline {
            Inline::Text(Text {
                variant: TextVariant::Plain,
                value,
            }) => {
                text.push_str(value);
            }
            Inline::Text(non_plain_text) => {
                // Non-plain text (code, math, html) - treat as unsupported
                let start_pos = text.len();
                text.push_str(&non_plain_text.value);
                let length = non_plain_text.value.len();

                if length > 0 {
                    formatting_events.push(FormattingEvent {
                        start_pos,
                        length,
                        formatting: FormattingType::Unsupported,
                    });
                }
            }
            Inline::Span(span) => {
                let start_pos = text.len();

                // Recursively process the span's children
                flatten_inlines_recursive(&span.children, text, formatting_events)?;

                let length = text.len() - start_pos;
                if length > 0 {
                    formatting_events.push(FormattingEvent {
                        start_pos,
                        length,
                        formatting: FormattingType::Span(span.variant),
                    });
                }
            }
            Inline::Link(_) | Inline::Image(_) | Inline::Footnote(_) => {
                // These are atomic - extract their text content and mark as unsupported
                let start_pos = text.len();
                let content = extract_text_content(inline);
                text.push_str(&content);
                let length = content.len();

                if length > 0 {
                    formatting_events.push(FormattingEvent {
                        start_pos,
                        length,
                        formatting: FormattingType::Unsupported,
                    });
                }
            }
        }
    }

    Ok(())
}

/// Recursively reconstructs inline elements from flattened text and formatting events.
fn unflatten_recursive(
    text: &str,
    events: &[FormattingEvent],
    start: usize,
    end: usize,
) -> Result<Vec<Inline>, RegexReplaceError> {
    use crate::md_elem::tree::elem::Span;

    let mut result = Vec::new();
    let mut pos = start;

    // Find events that start at our current position and fit within our range
    let mut applicable_events: Vec<_> = events
        .iter()
        .filter(|event| event.start_pos >= start && event.start_pos < end && event.start_pos + event.length <= end)
        .collect();

    // Sort by start position, then by length (longer spans first for proper nesting)
    applicable_events.sort_by_key(|event| (event.start_pos, std::cmp::Reverse(event.length)));

    let mut processed_ranges = Vec::new();

    for event in applicable_events {
        let event_start = event.start_pos;
        let event_end = event.start_pos + event.length;

        // Skip if this range has already been processed by a parent span
        if processed_ranges
            .iter()
            .any(|(start, end)| event_start >= *start && event_end <= *end)
        {
            continue;
        }

        // Add any plain text before this event
        if pos < event_start {
            let plain_text = &text[pos..event_start];
            if !plain_text.is_empty() {
                result.push(Inline::Text(Text {
                    variant: TextVariant::Plain,
                    value: plain_text.to_string(),
                }));
            }
        }

        // Create the span for this event
        if let FormattingType::Span(span_variant) = event.formatting {
            // Find child events that are completely contained within this span
            let child_events: Vec<_> = events
                .iter()
                .filter(|child| {
                    child.start_pos >= event_start && child.start_pos + child.length <= event_end && child != &event
                    // Don't include self
                })
                .cloned()
                .collect();

            // Recursively process the content inside this span
            let children = unflatten_recursive(text, &child_events, event_start, event_end)?;
            result.push(Inline::Span(Span {
                variant: span_variant,
                children,
            }));

            processed_ranges.push((event_start, event_end));
        }

        pos = event_end;
    }

    // Add any remaining plain text
    if pos < end {
        let plain_text = &text[pos..end];
        if !plain_text.is_empty() {
            result.push(Inline::Text(Text {
                variant: TextVariant::Plain,
                value: plain_text.to_string(),
            }));
        }
    }

    Ok(result)
}

/// Extracts the text content from atomic inline elements (links, images, footnotes).
fn extract_text_content(inline: &Inline) -> String {
    // TODO do I need this? There's existing functionality for it already elsewhere
    use crate::md_elem::tree::elem::{FootnoteId, Image, Link};

    match inline {
        Inline::Link(Link::Standard(link)) => {
            // For standard links, extract text from display content
            let mut text = String::new();
            let mut events = Vec::new();
            // We ignore errors here since we're just extracting text
            let _ = flatten_inlines_recursive(&link.display, &mut text, &mut events);
            text
        }
        Inline::Link(Link::Autolink(autolink)) => {
            // For autolinks, the URL is the display text
            autolink.url.clone()
        }
        Inline::Image(Image { alt, .. }) => {
            // For images, use the alt text
            alt.clone()
        }
        Inline::Footnote(FootnoteId { id }) => {
            // For footnotes, use the ID (without the caret)
            format!("[^{}]", id)
        }
        _ => String::new(), // Should not happen given our match pattern
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::md_elem::tree_test_utils::inlines;

    mod flatten {
        use super::*;

        #[test]
        fn plain_text_only() {
            let inlines = inlines!["hello world"];
            let result = FlattenedText::from_inlines(&inlines).unwrap();

            assert_eq!(result.text, "hello world");
            assert_eq!(result.formatting_events, vec![]);
        }

        #[test]
        fn simple_emphasis() {
            let inlines = inlines!["before ", em["emphasized"], " after"];
            let result = FlattenedText::from_inlines(&inlines).unwrap();

            assert_eq!(result.text, "before emphasized after");
            assert_eq!(
                result.formatting_events,
                vec![FormattingEvent {
                    start_pos: 7,
                    length: 10,
                    formatting: FormattingType::Span(SpanVariant::Emphasis),
                }]
            );
        }

        #[test]
        fn nested_formatting() {
            // _**text**_
            let inlines = inlines![em[strong["text"]]];
            let result = FlattenedText::from_inlines(&inlines).unwrap();

            assert_eq!(result.text, "text");
            assert_eq!(
                result.formatting_events,
                vec![
                    FormattingEvent {
                        start_pos: 0,
                        length: 4,
                        formatting: FormattingType::Span(SpanVariant::Strong),
                    },
                    FormattingEvent {
                        start_pos: 0,
                        length: 4,
                        formatting: FormattingType::Span(SpanVariant::Emphasis),
                    }
                ]
            );
        }

        #[test]
        fn link_as_unsupported() {
            let inlines = inlines!["before ", link["link text"] "https://example.com", " after"];
            let result = FlattenedText::from_inlines(&inlines).unwrap();

            assert_eq!(result.text, "before link text after");
            assert_eq!(
                result.formatting_events,
                vec![FormattingEvent {
                    start_pos: 7,
                    length: 9,
                    formatting: FormattingType::Unsupported,
                }]
            );
        }
    }

    mod unflatten {
        use super::*;

        #[test]
        fn plain_text_only() {
            let flattened = FlattenedText {
                text: "hello world".to_string(),
                formatting_events: vec![],
                offset: 0,
                last_replacement_end: 0,
            };
            let result = flattened.unflatten().unwrap();

            assert_eq!(result, inlines!["hello world"]);
        }

        #[test]
        fn simple_emphasis() {
            let flattened = FlattenedText {
                text: "before emphasized after".to_string(),
                formatting_events: vec![FormattingEvent {
                    start_pos: 7,
                    length: 10,
                    formatting: FormattingType::Span(SpanVariant::Emphasis),
                }],
                offset: 0,
                last_replacement_end: 0,
            };
            let result = flattened.unflatten().unwrap();

            let expected = inlines!["before ", em["emphasized"], " after"];
            assert_eq!(result, expected);
        }

        #[test]
        fn nested_formatting() {
            let flattened = FlattenedText {
                text: "text".to_string(),
                formatting_events: vec![
                    FormattingEvent {
                        start_pos: 0,
                        length: 4,
                        formatting: FormattingType::Span(SpanVariant::Emphasis),
                    },
                    FormattingEvent {
                        start_pos: 0,
                        length: 4,
                        formatting: FormattingType::Span(SpanVariant::Strong),
                    },
                ],
                offset: 0,
                last_replacement_end: 0,
            };
            let result = flattened.unflatten().unwrap();

            // Should reconstruct as _**text**_
            let expected = inlines![em[strong["text"]]];
            assert_eq!(result, expected);
        }

        #[test]
        fn unsupported_formatting_error() {
            let flattened = FlattenedText {
                text: "before link text after".to_string(),
                formatting_events: vec![FormattingEvent {
                    start_pos: 7,
                    length: 9,
                    formatting: FormattingType::Unsupported,
                }],
                offset: 0,
                last_replacement_end: 0,
            };

            assert!(flattened.unflatten().is_err());
        }
    }

    mod roundtrip {
        use super::*;

        #[test]
        fn identity_property() {
            let original = inlines![
                "before ",
                em[
                    "emphasis with ",
                    strong["nested strong"],
                    " text"
                ],
                " after"
            ];

            let flattened = FlattenedText::from_inlines(&original).unwrap();
            let reconstructed = flattened.unflatten().unwrap();

            assert_eq!(original, reconstructed);
        }
    }

    mod replace_range {
        use super::*;

        #[test]
        fn simple_replacement() {
            let mut flattened = FlattenedText {
                text: "hello world".to_string(),
                formatting_events: vec![],
                offset: 0,
                last_replacement_end: 0,
            };

            flattened.replace_range(6..11, "rust").unwrap();

            assert_eq!(flattened.text, "hello rust");
            assert_eq!(flattened.offset, -1); // "world" (5 chars) -> "rust" (4 chars)
        }

        #[test]
        fn replacement_with_formatting_events() {
            let mut flattened = FlattenedText {
                text: "before emphasized after".to_string(),
                formatting_events: vec![
                    FormattingEvent {
                        start_pos: 7,
                        length: 10,
                        formatting: FormattingType::Span(SpanVariant::Emphasis),
                    },
                    FormattingEvent {
                        start_pos: 18,
                        length: 5,
                        formatting: FormattingType::Span(SpanVariant::Strong),
                    },
                ],
                offset: 0,
                last_replacement_end: 0,
            };

            // Replace "before " with "after "
            flattened.replace_range(0..7, "after ").unwrap();

            assert_eq!(flattened.text, "after emphasized after");
            // Events should shift by -1 since "before " (7 chars) -> "after " (6 chars)
            assert_eq!(flattened.formatting_events[0].start_pos, 6);
            assert_eq!(flattened.formatting_events[1].start_pos, 17);
        }

        #[test]
        fn multiple_replacements() {
            let mut flattened = FlattenedText {
                //     ⁰123456789¹12
                text: "one two three".to_string(),
                formatting_events: vec![FormattingEvent {
                    start_pos: 8,
                    length: 5,
                    formatting: FormattingType::Span(SpanVariant::Emphasis),
                }],
                offset: 0,
                last_replacement_end: 0,
            };

            // Replace "one" with "1"
            flattened.replace_range(0..3, "1").unwrap();
            assert_eq!(flattened.text, "1 two three");
            assert_eq!(flattened.offset, -2);

            // Replace "two" with "2" (using original coordinates)
            flattened.replace_range(4..7, "2").unwrap();
            assert_eq!(flattened.text, "1 2 three");
            assert_eq!(flattened.offset, -4);

            // The formatting event should have shifted to position 5
            assert_eq!(flattened.formatting_events[0].start_pos, 5);
        }

        #[test]
        fn multiple_replacements_out_of_order() {
            let mut flattened = FlattenedText {
                //     ⁰123456789¹12
                text: "one two three".to_string(),
                formatting_events: vec![FormattingEvent {
                    start_pos: 8,
                    length: 5,
                    formatting: FormattingType::Span(SpanVariant::Emphasis),
                }],
                offset: 0,
                last_replacement_end: 0,
            };
            // Replace "two" with "2" (using original coordinates)
            flattened.replace_range(4..7, "2").unwrap();
            assert_eq!(flattened.text, "one 2 three");
            assert_eq!(flattened.offset, -2);

            // Replace "one" with "1". This isn't allowed, since it's replacing backwards.
            // (This restriction is what lets us keep the simple offset, rather than an arbitrary set of location
            // mappings.)
            let err = flattened.replace_range(0..3, "1");
            assert_eq!(err, Err(RangeReplacementError {}));
        }

        #[test]
        fn replacement_shortens_string() {
            let mut flattened = FlattenedText {
                text: "one two three".to_string(),
                formatting_events: vec![FormattingEvent {
                    start_pos: 8,
                    length: 5,
                    formatting: FormattingType::Span(SpanVariant::Emphasis),
                }],
                offset: 0,
                last_replacement_end: 0,
            };

            // Replace "one" with "1" (3 chars -> 1 char = -2)
            flattened.replace_range(0..3, "1").unwrap();

            assert_eq!(flattened.text, "1 two three");
            assert_eq!(flattened.offset, -2);
            // Event at position 8 should shift by -2 since replacement ends before it
            assert_eq!(flattened.formatting_events[0].start_pos, 6);
        }

        #[test]
        fn replacement_lengthens_string() {
            let mut flattened = FlattenedText {
                text: "one two three".to_string(),
                formatting_events: vec![FormattingEvent {
                    start_pos: 4,
                    length: 3,
                    formatting: FormattingType::Span(SpanVariant::Emphasis),
                }],
                offset: 0,
                last_replacement_end: 0,
            };

            // Replace "one" with "once" (3 chars -> 4 chars = +1)
            flattened.replace_range(0..3, "once").unwrap();

            assert_eq!(flattened.text, "once two three");
            assert_eq!(flattened.offset, 1);
            // Event at position 4 should shift by +1
            assert_eq!(flattened.formatting_events[0].start_pos, 5);
        }

        #[test]
        fn replacement_same_length() {
            let mut flattened = FlattenedText {
                //     ⁰123456789¹12
                text: "one two three".to_string(),
                formatting_events: vec![FormattingEvent {
                    start_pos: 4,
                    length: 3,
                    formatting: FormattingType::Span(SpanVariant::Emphasis),
                }],
                offset: 0,
                last_replacement_end: 0,
            };

            // Replace "one" with "uno" (3 chars -> 3 chars = 0)
            flattened.replace_range(0..3, "uno").unwrap();

            assert_eq!(flattened.text, "uno two three");
            assert_eq!(flattened.offset, 0);
            // Event at position 4 should not shift since size change is 0
            assert_eq!(flattened.formatting_events[0].start_pos, 4);
        }

        // TODO: need to test 0-length formatting, I think? Like "__". Or maybe those can't happen. I should check one
        //  way or another.

        #[test]
        fn replacement_of_full_formatted_span() {
            let mut flattened = FlattenedText {
                //     ⁰123456789¹12
                text: "one two three".to_string(),
                formatting_events: vec![FormattingEvent {
                    start_pos: 4,
                    length: 3,
                    formatting: FormattingType::Span(SpanVariant::Emphasis),
                }],
                offset: 0,
                last_replacement_end: 0,
            };

            // Replace "one" with "uno" (3 chars -> 3 chars = 0)
            flattened.replace_range(4..7, "duo").unwrap();

            assert_eq!(flattened.text, "one duo three");
            assert_eq!(flattened.offset, 0);
            // Event at position 4 should not shift since size change is 0
            assert_eq!(flattened.formatting_events[0].start_pos, 4);
        }

        #[test]
        fn removal_of_full_formatted_span() {
            let mut flattened = FlattenedText {
                //     ⁰123456789¹12
                text: "one two three".to_string(),
                formatting_events: vec![FormattingEvent {
                    start_pos: 4,
                    length: 3,
                    formatting: FormattingType::Span(SpanVariant::Emphasis),
                }],
                offset: 0,
                last_replacement_end: 0,
            };

            // Replace "one" with "uno" (3 chars -> 3 chars = 0)
            flattened.replace_range(4..7, "").unwrap();

            assert_eq!(flattened.text, "one  three");
            assert_eq!(flattened.offset, -3);
            // Event at position 4 should not shift since size change is 0
            assert_eq!(flattened.formatting_events.len(), 0);
        }

        #[test]
        fn replacement_removes_formatting() {
            // "one _two_ three" -> "on@hree" (replace "e two t" with "@")
            let mut flattened = FlattenedText {
                //     ⁰123456789¹12
                text: "one two three".to_string(),
                formatting_events: vec![FormattingEvent {
                    start_pos: 4,
                    length: 3, // "two"
                    formatting: FormattingType::Span(SpanVariant::Emphasis),
                }],
                offset: 0,
                last_replacement_end: 0,
            };

            // Replace "e two t" (positions 2..9) with "@"
            flattened.replace_range(2..9, "@").unwrap();

            assert_eq!(flattened.text, "on@hree");
            assert_eq!(flattened.offset, -6); // 7 chars -> 1 char = -6

            // The formatting event should be removed since it was completely within the replaced range
            // Events that start >= 9 would shift, but our event started at 4, so it gets removed
            // We need to check if the event still exists and has been adjusted
            // Since the event (4..7) was completely contained in the replacement (2..9),
            // it should be removed or have zero length
            assert_eq!(flattened.formatting_events.len(), 0);
        }

        #[test]
        fn replacement_starts_midway_through_formatting() {
            // "one _two_ three" -> "one _t@ee_" (replace "wo thr" with "@")
            let mut flattened = FlattenedText {
                text: "one two three".to_string(),
                formatting_events: vec![FormattingEvent {
                    start_pos: 4,
                    length: 3, // "two"
                    formatting: FormattingType::Span(SpanVariant::Emphasis),
                }],
                offset: 0,
                last_replacement_end: 0,
            };

            // Replace "wo thr" (positions 5..9) with "@"
            flattened.replace_range(5..9, "@").unwrap();

            assert_eq!(flattened.text, "one t@hree");
            assert_eq!(flattened.offset, -3); // 4 chars -> 1 char = -3

            // The formatting event should be truncated to only cover "t" (position 4, length 1)
            assert_eq!(flattened.formatting_events.len(), 1);
            assert_eq!(flattened.formatting_events[0].start_pos, 4);
            assert_eq!(flattened.formatting_events[0].length, 1);
        }

        #[test]
        fn replacement_ends_midway_through_formatting() {
            // "one _two_ three" -> "on@_wo_ three" (replace "e t" with "@")
            let mut flattened = FlattenedText {
                text: "one two three".to_string(),
                formatting_events: vec![FormattingEvent {
                    start_pos: 4,
                    length: 3, // "two"
                    formatting: FormattingType::Span(SpanVariant::Emphasis),
                }],
                offset: 0,
                last_replacement_end: 0,
            };

            // Replace "e t" (positions 2..5) with "@"
            flattened.replace_range(2..5, "@").unwrap();

            assert_eq!(flattened.text, "on@wo three");
            assert_eq!(flattened.offset, -2); // 3 chars -> 1 char = -2

            // The formatting event should be adjusted to start at position 3 (after "@")
            // and cover "wo" (length 2)
            assert_eq!(flattened.formatting_events.len(), 1);
            assert_eq!(flattened.formatting_events[0].start_pos, 3);
            assert_eq!(flattened.formatting_events[0].length, 2);
        }

        #[test]
        fn replacement_spans_multiple_formatting_events() {
            // "one _two_ **three** four" -> "one @ four"
            let mut flattened = FlattenedText {
                //     ⁰123456789¹1234567
                text: "one two three four".to_string(),
                formatting_events: vec![
                    FormattingEvent {
                        start_pos: 4,
                        length: 3, // "two"
                        formatting: FormattingType::Span(SpanVariant::Emphasis),
                    },
                    FormattingEvent {
                        start_pos: 8,
                        length: 5, // "three"
                        formatting: FormattingType::Span(SpanVariant::Strong),
                    },
                ],
                offset: 0,
                last_replacement_end: 0,
            };

            // Replace "two three" (positions 4..13) with "@"
            flattened.replace_range(4..13, "@").unwrap();

            assert_eq!(flattened.text, "one @ four");
            assert_eq!(flattened.offset, -8); // 9 chars -> 1 char = -8

            // Both formatting events should be removed since they were completely within the replaced range
            assert_eq!(flattened.formatting_events.len(), 0);
        }

        #[test]
        fn replacement_splits_formatting_event() {
            // "one _twelve_ three" -> "one _tw@ve_ three" (replace "el" with "@")
            let mut flattened = FlattenedText {
                text: "one twelve three".to_string(),
                formatting_events: vec![FormattingEvent {
                    start_pos: 4,
                    length: 6, // "twelve"
                    formatting: FormattingType::Span(SpanVariant::Emphasis),
                }],
                offset: 0,
                last_replacement_end: 0,
            };

            // Replace "el" (positions 6..8) with "@"
            flattened.replace_range(6..8, "@").unwrap();

            assert_eq!(flattened.text, "one tw@ve three");
            assert_eq!(flattened.offset, -1); // 2 chars -> 1 char = -1

            // The formatting event should be adjusted to cover "tw@ve" (length 5)
            assert_eq!(flattened.formatting_events.len(), 1);
            assert_eq!(flattened.formatting_events[0].start_pos, 4);
            assert_eq!(flattened.formatting_events[0].length, 5);
        }
    }
}
