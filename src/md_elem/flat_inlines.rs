use crate::md_elem::tree::elem::Span;
use crate::md_elem::tree::elem::{Autolink, AutolinkStyle, Image, Link, StandardLink};
use crate::md_elem::tree::elem::{FootnoteId, Inline, LinkDefinition, SpanVariant, Text, TextVariant};
use crate::output::{inlines_to_plain_string, FootnoteToString, InlineToStringOpts};
use std::iter::Peekable;
use std::ops::Range;

/// Atomic formatting types that cannot be crossed by regex operations.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum AtomicFormatting {
    /// Standard link with display text and link definition
    StandardLink(LinkDefinition),
    /// Autolink with style
    AutoLink(AutolinkStyle),
    /// Image with alt text and link definition
    Image(LinkDefinition),
    /// Footnote reference
    Footnote,
    /// Non-plain text (code, math, html)
    Text(TextVariant),
}

/// The type of formatting to apply to a range of text.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum FormattingType {
    /// Standard span formatting (emphasis, strong, delete)
    Span(SpanVariant),
    /// Content that regexes cannot cross into or out of.
    Atomic(AtomicFormatting),
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

/// Error that occurs during range replacement operations.
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum RangeReplacementError {
    InternalError(&'static str),
    AtomicityViolation,
}

impl FlattenedText {
    /// Creates a flattened representation from a slice of inline elements.
    ///
    /// This extracts all plain text content and creates formatting events that describe
    /// where spans should be applied.
    pub(crate) fn from_inlines(inlines: impl IntoIterator<Item = Inline>) -> Self {
        let mut text = String::new();

        let formatting_events = flatten_inlines(inlines, &mut text);

        FlattenedText {
            text,
            formatting_events,
            offset: 0,
            last_replacement_end: 0,
        }
    }

    /// Reconstructs inline elements from the flattened representation.
    ///
    /// This applies the formatting events to the text to rebuild the original
    /// tree structure. Events marked as `FormattingType::Unsupported` will cause
    /// a reconstruction error since we cannot recreate the original atomic elements.
    pub(crate) fn unflatten(self) -> Result<Vec<Inline>, RangeReplacementError> {
        // unflatten_recursive(&self.text, &self.formatting_events, 0, self.text.len())
        let mut events = self.formatting_events.into_iter().peekable();
        let inlines = Self::unflatten_rec_0(&self.text, 0, &mut events);
        if events.peek().is_some() {
            Err(RangeReplacementError::InternalError("some events failed to unflatten"))
        } else {
            Ok(inlines)
        }
    }

    pub(crate) fn unflatten_rec_0<E>(mut text: &str, mut text_offset: usize, events: &mut Peekable<E>) -> Vec<Inline>
    where
        E: Iterator<Item = FormattingEvent>,
    {
        // We're going to basically fold the text up. Given text and events:
        //
        //         /--em---\
        // "hello, world and all"
        //
        // We'll turn "hello, " into a plain text span, and then for the em span we'll recurse down, with "world and"
        // as the string.

        let mut inlines = Vec::new();

        while let Some(peeked_event) = events.peek() {
            let peeked_event_local_offset = peeked_event.start_pos - text_offset;
            if peeked_event_local_offset > 0 {
                // Prefix plain text
                let (plain, tail) = text.split_at(peeked_event_local_offset);
                text = tail;
                text_offset += plain.len();
                inlines.push(Inline::Text(Text {
                    variant: TextVariant::Plain,
                    value: plain.to_string(),
                }));
            } else if peeked_event_local_offset >= text.len() {
                // Events are past this text's range, so pass back up to the caller
                return inlines;
            } else {
                // advance the iterator to discard this event
                let event = events.next().unwrap();
                let (within_span, tail) = text.split_at(event.length);
                text = tail;
                let inline = match event.formatting {
                    FormattingType::Span(variant) => Inline::Span(Span {
                        variant,
                        children: Self::unflatten_rec_0(within_span, text_offset, events),
                    }),
                    FormattingType::Atomic(AtomicFormatting::StandardLink(link)) => {
                        let display =
                            Self::unflatten_rec_0(within_span, text_offset + peeked_event_local_offset, events);
                        Inline::Link(Link::Standard(StandardLink { display, link }))
                    }
                    FormattingType::Atomic(AtomicFormatting::AutoLink(style)) => {
                        Inline::Link(Link::Autolink(Autolink {
                            url: within_span.to_string(),
                            style,
                        }))
                    }
                    FormattingType::Atomic(AtomicFormatting::Image(link)) => Inline::Image(Image {
                        alt: within_span.to_string(),
                        link,
                    }),
                    FormattingType::Atomic(AtomicFormatting::Footnote) => Inline::Footnote(FootnoteId {
                        id: within_span.to_string(),
                    }),
                    FormattingType::Atomic(AtomicFormatting::Text(variant)) => Inline::Text(Text {
                        variant,
                        value: within_span.to_string(),
                    }),
                };
                text_offset += within_span.len();
                inlines.push(inline);
            }
        }

        // The remaining text is plain
        if !text.is_empty() {
            inlines.push(Inline::Text(Text {
                variant: TextVariant::Plain,
                value: text.to_string(),
            }));
        }

        inlines
    }

    /// Replaces a range of text using original coordinates.
    ///
    /// The range must be non-overlapping and in increasing order relative to previous
    /// calls to this method. This updates both the text and adjusts formatting events.
    pub(crate) fn replace_range(
        &mut self,
        original_range: Range<usize>,
        replacement: &str,
    ) -> Result<(), RangeReplacementError> {
        // Validate that ranges are non-overlapping and in increasing order
        // by checking that the current range start is not before the end of the last replacement
        if original_range.start < self.last_replacement_end {
            return Err(RangeReplacementError::InternalError("range replacement went backwards"));
        }

        let current_start = (original_range.start as isize + self.offset) as usize;
        let current_end = (original_range.end as isize + self.offset) as usize;

        // Validate range is within bounds and not going backwards
        if current_end > self.text.len() || current_start > self.text.len() {
            return Err(RangeReplacementError::InternalError("replacement out of range"));
        }

        // Check that we don't cross any atomic events.
        // First, find all atomic events that touch this range.
        let mut relevant_atomic_events = self.formatting_events.iter().filter(|event| {
            let event_end_pos = event.start_pos + event.length;
            let event_pos_overlap = event.start_pos < original_range.end && event_end_pos >= original_range.start;
            event_pos_overlap && matches!(event.formatting, FormattingType::Atomic(_))
        });
        let atomicity_violation = match relevant_atomic_events.next() {
            None => {
                // There are no atomic events, so no atomicity violation.
                false
            }
            Some(first_atomic_event) => {
                // The event overlaps with the replacement range in some way, so make sure that:
                // (a) the replacement range doesn't start before the atomic event
                // (b) the replacement range doesn't end after the atomic event
                // (c) there aren't any other atomic events (since the replacement range would have started before them)
                let atomic_event_end_pos = first_atomic_event.start_pos + first_atomic_event.length;
                if original_range.start < first_atomic_event.start_pos {
                    true // case (a)
                } else if original_range.end > atomic_event_end_pos {
                    true // case(b)
                } else {
                    // case (c)
                    relevant_atomic_events.any(|e| matches!(e.formatting, FormattingType::Atomic(_)))
                }
            }
        };
        drop(relevant_atomic_events);
        if atomicity_violation {
            return Err(RangeReplacementError::AtomicityViolation);
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
                // Event is completely within the replacement
                if event_start == original_range.start && event_end == original_range.end && !replacement.is_empty() {
                    // Event exactly matches the replacement range and replacement is not empty - preserve with new length
                    let mut new_event = event.clone();
                    new_event.length = replacement.len();
                    events_to_keep.push(new_event);
                } else {
                    // Event is within but doesn't exactly match, or replacement is empty - remove it
                    // (Don't add to events_to_keep)
                }
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
fn flatten_inlines(inlines: impl IntoIterator<Item = Inline>, text: &mut String) -> Vec<FormattingEvent> {
    let mut formatting_events = Vec::new();
    for inline in inlines {
        match inline {
            Inline::Text(Text {
                variant: TextVariant::Plain,
                value,
            }) => {
                text.push_str(&value);
            }
            Inline::Text(non_plain_text) => {
                // Non-plain text (code, math, html) - treat as atomic
                let start_pos = text.len();
                text.push_str(&non_plain_text.value);
                let length = non_plain_text.value.len();

                formatting_events.push(FormattingEvent {
                    start_pos,
                    length,
                    formatting: FormattingType::Atomic(AtomicFormatting::Text(non_plain_text.variant)),
                });
            }
            Inline::Span(span) => {
                let start_pos = text.len();

                // Recursively process the span's children, and get the new length
                let mut child_events = flatten_inlines(span.children, text);
                let length = text.len() - start_pos;

                // append the events
                formatting_events.push(FormattingEvent {
                    start_pos,
                    length,
                    formatting: FormattingType::Span(span.variant),
                });
                formatting_events.append(&mut child_events)
            }
            Inline::Link(Link::Standard(link)) => {
                let start_pos = text.len();

                // Recursively process the span's children, and get the new length
                let mut child_events = flatten_inlines(link.display, text);
                let length = text.len() - start_pos;

                // append the events
                formatting_events.push(FormattingEvent {
                    start_pos,
                    length,
                    formatting: FormattingType::Atomic(AtomicFormatting::StandardLink(link.link)),
                });
                formatting_events.append(&mut child_events)
            }
            other @ (Inline::Link(Link::Autolink(_)) | Inline::Image(_) | Inline::Footnote(_)) => {
                // We can't do a regex replace that spans into, within, or out of these. (Doing so would be confusing,
                // since we'd just be doing it on the display text; and the result could be empty, if the replacement
                // range starts outside this inline). Rather than describing a complex situation to the user, we'll just
                // prohibit them.
                let start_pos = text.len();
                let content = inlines_to_plain_string(
                    &[&other],
                    InlineToStringOpts {
                        footnotes: FootnoteToString::OnlyFootnoteId,
                    },
                );
                text.push_str(&content);
                let length = content.len();

                let atomic_formatting = match other {
                    Inline::Link(Link::Standard(ref standard_link)) => {
                        AtomicFormatting::StandardLink(standard_link.link.clone())
                    }
                    Inline::Link(Link::Autolink(ref autolink)) => AtomicFormatting::AutoLink(autolink.style),
                    Inline::Image(ref image) => AtomicFormatting::Image(image.link.clone()),
                    Inline::Footnote(_) => AtomicFormatting::Footnote,
                    _ => unreachable!("other should only be Link, Image, or Footnote"),
                };

                formatting_events.push(FormattingEvent {
                    start_pos,
                    length,
                    formatting: FormattingType::Atomic(atomic_formatting),
                });
            }
        }
    }

    formatting_events
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::md_elem::tree_test_utils::inlines;

    mod flatten {
        use super::*;
        use crate::md_elem::elem::{LinkDefinition, LinkReference};

        #[test]
        fn plain_text_only() {
            let inlines = inlines!["hello world"];
            let result = FlattenedText::from_inlines(inlines);

            assert_eq!(result.text, "hello world");
            assert_eq!(result.formatting_events, vec![]);
        }

        #[test]
        fn simple_emphasis() {
            let inlines = inlines!["before ", em["emphasized"], " after"];
            let result = FlattenedText::from_inlines(inlines);

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
            let result = FlattenedText::from_inlines(inlines);

            assert_eq!(result.text, "text");
            assert_eq!(
                result.formatting_events,
                vec![
                    FormattingEvent {
                        start_pos: 0,
                        length: 4,
                        formatting: FormattingType::Span(SpanVariant::Emphasis),
                    },
                    FormattingEvent {
                        start_pos: 0,
                        length: 4,
                        formatting: FormattingType::Span(SpanVariant::Strong),
                    }
                ]
            );
        }

        #[test]
        fn link() {
            let inlines = inlines!["before ", link["link text"]("https://example.com"), " after"];
            let result = FlattenedText::from_inlines(inlines);

            assert_eq!(result.text, "before link text after");
            assert_eq!(
                result.formatting_events,
                vec![FormattingEvent {
                    start_pos: 7,
                    length: 9,
                    formatting: FormattingType::Atomic(AtomicFormatting::StandardLink(LinkDefinition {
                        url: "https://example.com".to_string(),
                        title: None,
                        reference: LinkReference::Inline,
                    }))
                }]
            );
        }

        #[test]
        fn link_with_no_display_text() {
            let inlines = inlines!["before ", link[""]("https://example.com"), " after"];
            let result = FlattenedText::from_inlines(inlines);

            //                       ₀123456789₁12
            assert_eq!(result.text, "before  after");
            assert_eq!(
                result.formatting_events,
                vec![FormattingEvent {
                    start_pos: 7,
                    length: 0,
                    formatting: FormattingType::Atomic(AtomicFormatting::StandardLink(LinkDefinition {
                        url: "https://example.com".to_string(),
                        title: None,
                        reference: LinkReference::Inline,
                    }))
                }]
            );
        }

        #[test]
        fn footnote() {
            let inlines = inlines![footnote["1"]];
            let result = FlattenedText::from_inlines(inlines);
            assert_eq!(result.text, "1");
            assert_eq!(
                result.formatting_events,
                vec![FormattingEvent {
                    start_pos: 0,
                    length: 1,
                    formatting: FormattingType::Atomic(AtomicFormatting::Footnote)
                }]
            );
        }
    }

    mod unflatten {
        use super::*;
        use crate::md_elem::tree::elem::{LinkDefinition, LinkReference};

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
        fn link_with_plain_test() {
            let flattened = FlattenedText {
                //     ₀123456789₁12
                text: "example link".to_string(),
                formatting_events: vec![FormattingEvent {
                    start_pos: 0,
                    length: 12,
                    formatting: FormattingType::Atomic(AtomicFormatting::StandardLink(LinkDefinition {
                        url: "https://example.com".to_string(),
                        title: None,
                        reference: LinkReference::Inline,
                    })),
                }],
                offset: 0,
                last_replacement_end: 0,
            };

            // The unflatten function ignores atomic elements and just returns plain text
            let result = flattened.unflatten().unwrap();
            assert_eq!(result, inlines![link["example link"]("https://example.com")]);
        }

        #[test]
        fn link_with_formatted_text() {
            let flattened = FlattenedText {
                //     ₀123456789₁12
                text: "example link".to_string(),
                formatting_events: vec![
                    FormattingEvent {
                        start_pos: 0,
                        length: 12,
                        formatting: FormattingType::Atomic(AtomicFormatting::StandardLink(LinkDefinition {
                            url: "https://example.com".to_string(),
                            title: None,
                            reference: LinkReference::Inline,
                        })),
                    },
                    FormattingEvent {
                        start_pos: 8,
                        length: 4,
                        formatting: FormattingType::Span(SpanVariant::Emphasis),
                    },
                ],
                offset: 0,
                last_replacement_end: 0,
            };

            // The unflatten function ignores atomic elements and just returns plain text
            let result = flattened.unflatten().unwrap();
            assert_eq!(result, inlines![link["example ", em["link"]]("https://example.com")]);
        }

        #[test]
        fn link_with_empty_display() {
            let flattened = FlattenedText {
                //     ₀123456789₁12
                text: "before  after".to_string(),
                formatting_events: vec![FormattingEvent {
                    start_pos: 7,
                    length: 0,
                    formatting: FormattingType::Atomic(AtomicFormatting::StandardLink(LinkDefinition {
                        url: "https://example.com".to_string(),
                        title: None,
                        reference: LinkReference::Inline,
                    })),
                }],
                offset: 0,
                last_replacement_end: 0,
            };

            // The unflatten function ignores atomic elements and just returns plain text
            let result = flattened.unflatten().unwrap();
            assert_eq!(result, inlines!["before ", link[]("https://example.com"), " after"]);
        }

        #[test]
        fn image() {
            let flattened = FlattenedText {
                text: "example link".to_string(),
                formatting_events: vec![FormattingEvent {
                    start_pos: 0,
                    length: 12,
                    formatting: FormattingType::Atomic(AtomicFormatting::Image(LinkDefinition {
                        url: "https://example.com/image.png".to_string(),
                        title: None,
                        reference: LinkReference::Inline,
                    })),
                }],
                offset: 0,
                last_replacement_end: 0,
            };

            // The unflatten function ignores atomic elements and just returns plain text
            let result = flattened.unflatten().unwrap();
            assert_eq!(result, inlines![image["example link"]("https://example.com/image.png")]);
        }

        #[test]
        fn footnote() {
            let flattened = FlattenedText {
                text: "1".to_string(),
                formatting_events: vec![FormattingEvent {
                    start_pos: 0,
                    length: 1,
                    formatting: FormattingType::Atomic(AtomicFormatting::Footnote),
                }],
                offset: 0,
                last_replacement_end: 0,
            };

            // The unflatten function ignores atomic elements and just returns plain text
            let result = flattened.unflatten().unwrap();
            assert_eq!(result, inlines![footnote["1"]]);
        }
    }

    mod round_trip {
        use super::*;

        #[test]
        fn identity_property() {
            let original = inlines![
            //   ₀123456
                "before ",
                em[
            //       ₀123456789₁123
                    "emphasis with ",
            //              ₀123456789₁12
                    strong["nested strong"],
            //       ₀1234
                    " text"
                ],
            //   ₀12345
                " after"
            ];

            let flattened = FlattenedText::from_inlines(original.clone());
            let reconstructed = flattened.unflatten().unwrap();

            assert_eq!(original, reconstructed);
        }
    }

    mod replace_range {
        use super::*;
        use crate::md_elem::tree::elem::{LinkDefinition, LinkReference};

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
                //     ₀123456789₁12
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
                //     ₀123456789₁12
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
            assert_eq!(
                err,
                Err(RangeReplacementError::InternalError("range replacement went backwards"))
            );
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
        fn replacement_lengthens_string_from_zero_length_slice() {
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
            flattened.replace_range(0..0, "!").unwrap();

            assert_eq!(flattened.text, "!one two three");
            assert_eq!(flattened.offset, 1);
            // Event at position 4 should shift by +1
            assert_eq!(flattened.formatting_events[0].start_pos, 5);
        }

        #[test]
        fn replacement_same_length() {
            let mut flattened = FlattenedText {
                //     ₀123456789₁12
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

        #[test]
        fn replacement_of_full_formatted_span_with_equal_length() {
            let mut flattened = FlattenedText {
                //     ₀123456789₁12
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
            assert_eq!(flattened.formatting_events[0].length, 3);
        }

        #[test]
        fn replacement_of_full_formatted_span_with_shorter_length() {
            let mut flattened = FlattenedText {
                //     ₀123456789₁12
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
            flattened.replace_range(4..7, "2").unwrap();

            assert_eq!(flattened.text, "one 2 three");
            assert_eq!(flattened.offset, -2);
            // Event at position 4 should not shift since size change is 0
            assert_eq!(flattened.formatting_events[0].start_pos, 4);
            assert_eq!(flattened.formatting_events[0].length, 1);
        }

        #[test]
        fn replacement_of_full_formatted_span_with_longer_length() {
            let mut flattened = FlattenedText {
                //     ₀123456789₁12
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
            flattened.replace_range(4..7, "second").unwrap();

            assert_eq!(flattened.text, "one second three");
            assert_eq!(flattened.offset, 3);
            // Event at position 4 should not shift since size change is 0
            assert_eq!(flattened.formatting_events[0].start_pos, 4);
            assert_eq!(flattened.formatting_events[0].length, 6);
        }

        #[test]
        fn removal_of_full_formatted_span() {
            let mut flattened = FlattenedText {
                //     ₀123456789₁12
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
                //     ₀123456789₁12
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
                //     ₀123456789₁1234567
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

        #[test]
        fn replacement_within_atomic_span() {
            let mut flattened = FlattenedText {
                text: "example link".to_string(),
                formatting_events: vec![FormattingEvent {
                    start_pos: 0,
                    length: 12,
                    formatting: FormattingType::Atomic(AtomicFormatting::StandardLink(LinkDefinition {
                        url: "https://example.com".to_string(),
                        title: None,
                        reference: LinkReference::Inline,
                    })),
                }],
                offset: 0,
                last_replacement_end: 0,
            };

            // Replace "amp" with "@" within the atomic span (positions 2..5)
            let result = flattened.replace_range(2..5, "@");

            // This should succeed since the replacement is entirely within the atomic span
            assert!(result.is_ok());
            assert_eq!(flattened.text, "ex@le link");
            assert_eq!(flattened.formatting_events[0].length, 10); // length adjusted
        }

        #[test]
        fn replacement_into_atomic_span() {
            let mut flattened = FlattenedText {
                //     ₀123456789₁123456789₂1
                text: "before link text after".to_string(),
                formatting_events: vec![
                    FormattingEvent {
                        start_pos: 0,
                        length: 6,
                        formatting: FormattingType::Span(SpanVariant::Emphasis),
                    },
                    FormattingEvent {
                        start_pos: 7,
                        length: 9,
                        formatting: FormattingType::Atomic(AtomicFormatting::StandardLink(LinkDefinition {
                            url: "https://example.com".to_string(),
                            title: None,
                            reference: LinkReference::Inline,
                        })),
                    },
                ],
                offset: 0,
                last_replacement_end: 0,
            };

            // Replace "ore lin" (positions 3..10) which crosses from normal span into atomic span
            let result = flattened.replace_range(3..10, "@");
            assert_eq!(result, Err(RangeReplacementError::AtomicityViolation));
            assert_eq!(flattened.text, "before link text after");
        }

        #[test]
        fn replacement_out_of_atomic_span() {
            let mut flattened = FlattenedText {
                text: "before link text after".to_string(),
                formatting_events: vec![
                    FormattingEvent {
                        start_pos: 7,
                        length: 9,
                        formatting: FormattingType::Atomic(AtomicFormatting::StandardLink(LinkDefinition {
                            url: "https://example.com".to_string(),
                            title: None,
                            reference: LinkReference::Inline,
                        })),
                    },
                    FormattingEvent {
                        start_pos: 17,
                        length: 5,
                        formatting: FormattingType::Span(SpanVariant::Strong),
                    },
                ],
                offset: 0,
                last_replacement_end: 0,
            };

            // Replace "xt af" (positions 13..18) which crosses from atomic span into normal span
            let result = flattened.replace_range(13..18, "@");
            assert_eq!(result, Err(RangeReplacementError::AtomicityViolation));
            assert_eq!(flattened.text, "before link text after");
        }

        #[test]
        fn replacement_across_atomic_spans() {
            let mut flattened = FlattenedText {
                //     ₀123456789₁12456789
                text: "link text image alt".to_string(),
                formatting_events: vec![
                    FormattingEvent {
                        start_pos: 0,
                        length: 9,
                        formatting: FormattingType::Atomic(AtomicFormatting::StandardLink(LinkDefinition {
                            url: "https://example.com".to_string(),
                            title: None,
                            reference: LinkReference::Inline,
                        })),
                    },
                    FormattingEvent {
                        start_pos: 10,
                        length: 9,
                        formatting: FormattingType::Atomic(AtomicFormatting::Image(LinkDefinition {
                            url: "https://example.com/image.png".to_string(),
                            title: None,
                            reference: LinkReference::Inline,
                        })),
                    },
                ],
                offset: 0,
                last_replacement_end: 0,
            };

            // Replace "xt image al" (positions 7..17) which crosses from one atomic span into another
            let result = flattened.replace_range(7..17, "_");
            assert_eq!(result, Err(RangeReplacementError::AtomicityViolation));
            assert_eq!(flattened.text, "link text image alt");
        }

        /// Checks the atomic spans restrictions, specifically focusing on fence post conditions.
        mod atomicity_fencepost_checks {
            use super::*;
            use crate::md_elem::elem::{LinkDefinition, LinkReference};

            #[test]
            fn goes_until_right_before() {
                let result = test_data(Tc {
                    //    "₀1234567"
                    text: "a LINK b",
                    atomic_start: 2,
                    atomic_len: 4,
                })
                .replace_range(0..2, "");
                assert_eq!(result, Ok(()))
            }

            #[test]
            fn goes_until_right_into() {
                let result = test_data(Tc {
                    //    "₀1234567"
                    text: "a LINK b",
                    atomic_start: 2,
                    atomic_len: 4,
                })
                .replace_range(0..3, "");
                assert_eq!(result, Err(RangeReplacementError::AtomicityViolation))
            }

            #[test]
            fn starts_right_before_goes_to_mid() {
                let result = test_data(Tc {
                    //    "₀1234567"
                    text: "a LINK b",
                    atomic_start: 2,
                    atomic_len: 4,
                })
                .replace_range(1..5, "");
                assert_eq!(result, Err(RangeReplacementError::AtomicityViolation))
            }

            #[test]
            fn starts_right_at_goes_to_mid() {
                let result = test_data(Tc {
                    //    "₀1234567"
                    text: "a LINK b",
                    atomic_start: 2,
                    atomic_len: 4,
                })
                .replace_range(2..5, "");
                assert_eq!(result, Ok(()))
            }

            #[test]
            fn starts_mid_goes_to_end() {
                let result = test_data(Tc {
                    //    "₀1234567"
                    text: "a LINK b",
                    atomic_start: 2,
                    atomic_len: 4,
                })
                .replace_range(4..6, "");
                assert_eq!(result, Ok(()))
            }

            #[test]
            fn starts_mid_goes_to_right_after() {
                let result = test_data(Tc {
                    //    "₀1234567"
                    text: "a LINK b",
                    atomic_start: 2,
                    atomic_len: 4,
                })
                .replace_range(4..7, "");
                assert_eq!(result, Err(RangeReplacementError::AtomicityViolation))
            }

            struct Tc {
                text: &'static str,
                atomic_start: usize,
                atomic_len: usize,
            }

            fn test_data(tc: Tc) -> FlattenedText {
                FlattenedText {
                    text: tc.text.to_string(),
                    formatting_events: vec![FormattingEvent {
                        start_pos: tc.atomic_start,
                        length: tc.atomic_len,
                        formatting: FormattingType::Atomic(AtomicFormatting::StandardLink(LinkDefinition {
                            url: "https://example.com".to_string(),
                            title: None,
                            reference: LinkReference::Inline,
                        })),
                    }],
                    offset: 0,
                    last_replacement_end: 0,
                }
            }
        }
    }
}
