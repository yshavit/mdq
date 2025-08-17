use crate::md_elem::tree::elem::Span;
use crate::md_elem::tree::elem::{Autolink, AutolinkStyle, Image, Link, StandardLink};
use crate::md_elem::tree::elem::{FootnoteId, Inline, LinkDefinition, SpanVariant, Text, TextVariant};
use crate::output::{inlines_to_plain_string, FootnoteToString, InlineToStringOpts};
use std::cmp::Ordering;
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
pub(crate) struct FlattenedText {
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
pub(crate) struct FormattingEvent {
    /// Starting character position in the flattened text (inclusive)
    pub start_pos: usize,
    /// The number of characters this formatting applies to
    pub length: usize,
    /// The type of formatting to apply
    pub formatting: FormattingType,
}

/// Error that occurs during range replacement operations.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum RangeReplacementError {
    InternalError(String),
    AtomicityViolation,
}

/// Represents how a position relates to a boundary point
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BoundaryPosition {
    /// Position is before the boundary
    Before,
    /// Position is exactly at the boundary
    At,
    /// Position is after the boundary
    After,
}

impl BoundaryPosition {
    fn classify(position: usize, boundary: usize) -> Self {
        match position.cmp(&boundary) {
            Ordering::Less => Self::Before,
            Ordering::Equal => Self::At,
            Ordering::Greater => Self::After,
        }
    }
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
        // TODO this should only return the Vec, not a Result. If there are trailing events, that should be a panic.
        let mut events = self.formatting_events.into_iter().peekable();
        let inlines = Self::unflatten_rec_0(&self.text, 0, &mut events);
        if events.peek().is_some() {
            let remaining_count = events.count();
            Err(RangeReplacementError::InternalError(format!(
                "failed to unflatten: {} events remaining",
                remaining_count
            )))
        } else {
            Ok(inlines)
        }
    }

    /// Reconstructs inline elements by recursively processing text slices and formatting events.
    ///
    /// ## The Core Problem: Global Events vs Local Text Slices
    ///
    /// Formatting events always have coordinates relative to the ORIGINAL flattened text.
    /// But during recursion, we work with text SLICES that are substrings of the original.
    /// The `text_start_offset` parameter bridges this gap by telling us where our current
    /// text slice starts in the original global coordinate space.
    ///
    /// ## Example: Why We Need text_start_offset
    ///
    /// Original text: `"hello _world_ end"`
    /// Flattened: `text="hello world end", events=[{start_pos: 6, length: 5, formatting: Emphasis}]`
    ///
    /// 1. Initial call: `unflatten_rec_0("hello world end", text_start_offset=0)`
    ///    - Event at global pos 6 means "split at position 6 in the original text"
    ///    - Since text_start_offset=0, local position = 6-0 = 6 ✓
    ///    - Extract slice "world" and recursively process it
    ///
    /// 2. Recursive call: `unflatten_rec_0("world", text_start_offset=6)`
    ///    - The text slice is just "world" (5 chars)
    ///    - But events still reference the ORIGINAL coordinates!
    ///    - If we see an event at global pos 8, local position = 8-6 = 2 ✓
    ///    - Without text_start_offset, we'd think pos 8 means "split at char 8 of 'world'" → PANIC!
    ///
    /// ## How Recursion Works
    ///
    /// Each recursive call processes a text slice that corresponds to the content inside
    /// a formatting span. The text_start_offset ensures that global event coordinates
    /// are correctly translated to local positions within each text slice.
    ///
    /// This prevents the "byte index X is out of bounds" panics that occurred when
    /// the original algorithm lost track of the coordinate mapping between global
    /// event positions and local text slice boundaries.
    fn unflatten_rec_0<E>(text: &str, text_start_offset: usize, events: &mut Peekable<E>) -> Vec<Inline>
    where
        E: Iterator<Item = FormattingEvent>,
    {
        let mut inlines = Vec::new();
        let mut current_pos = 0; // Position within the current text slice
        let text_end_offset = text_start_offset + text.len();

        while let Some(peeked_event) = events.peek() {
            // Skip events that are completely before our text range
            if peeked_event.start_pos + peeked_event.length <= text_start_offset {
                events.next();
                continue;
            }

            // Stop processing if event starts after our text range
            if peeked_event.start_pos >= text_end_offset {
                break;
            }

            // Calculate where this event starts within our current text slice
            let event_start_in_slice = peeked_event.start_pos - text_start_offset;

            // Add any plain text before this event
            if event_start_in_slice > current_pos {
                let plain_text = &text[current_pos..event_start_in_slice];
                if !plain_text.is_empty() {
                    inlines.push(Inline::Text(Text {
                        variant: TextVariant::Plain,
                        value: plain_text.to_string(),
                    }));
                }
                current_pos = event_start_in_slice;
            }

            // Process the event
            let event = events.next().unwrap();
            let event_end_in_slice = (event.start_pos + event.length) - text_start_offset;
            let event_end_in_slice = event_end_in_slice.min(text.len());

            if event_end_in_slice >= current_pos {
                let event_text = &text[current_pos..event_end_in_slice];
                let inline = match event.formatting {
                    FormattingType::Span(variant) => Inline::Span(Span {
                        variant,
                        children: Self::unflatten_rec_0(event_text, text_start_offset + current_pos, events),
                    }),
                    FormattingType::Atomic(AtomicFormatting::StandardLink(link)) => {
                        let display = Self::unflatten_rec_0(event_text, text_start_offset + current_pos, events);
                        Inline::Link(Link::Standard(StandardLink { display, link }))
                    }
                    FormattingType::Atomic(AtomicFormatting::AutoLink(style)) => {
                        Inline::Link(Link::Autolink(Autolink {
                            url: event_text.to_string(),
                            style,
                        }))
                    }
                    FormattingType::Atomic(AtomicFormatting::Image(link)) => Inline::Image(Image {
                        alt: event_text.to_string(),
                        link,
                    }),
                    FormattingType::Atomic(AtomicFormatting::Footnote) => Inline::Footnote(FootnoteId {
                        id: event_text.to_string(),
                    }),
                    FormattingType::Atomic(AtomicFormatting::Text(variant)) => Inline::Text(Text {
                        variant,
                        value: event_text.to_string(),
                    }),
                };
                inlines.push(inline);
                current_pos = event_end_in_slice;
            }
        }

        // Add any remaining plain text
        if current_pos < text.len() {
            let remaining_text = &text[current_pos..];
            inlines.push(Inline::Text(Text {
                variant: TextVariant::Plain,
                value: remaining_text.to_string(),
            }));
        }

        inlines
    }

    /// Replaces a range of text using original coordinates.
    ///
    /// The range must be non-overlapping and in increasing order relative to previous calls to this method. This
    /// updates both the text and adjusts formatting events.
    pub(crate) fn replace_range(
        &mut self,
        replacement_range: Range<usize>,
        replacement: &str,
    ) -> Result<(), RangeReplacementError> {
        // Validate that ranges are non-overlapping and in increasing order
        // by checking that the current range start is not before the end of the last replacement
        if replacement_range.start < self.last_replacement_end {
            return Err(RangeReplacementError::InternalError(format!(
                "range replacement went backwards: new start {} < last end {}",
                replacement_range.start, self.last_replacement_end
            )));
        }

        let current_start = (replacement_range.start as isize + self.offset) as usize;
        let current_end = (replacement_range.end as isize + self.offset) as usize;

        // Validate range is within bounds and not going backwards
        if current_end > self.text.len() || current_start > self.text.len() {
            return Err(RangeReplacementError::InternalError(format!(
                "replacement range {}..{} exceeds text length {}",
                current_start,
                current_end,
                self.text.len()
            )));
        }

        // Check that we don't cross any atomic events.
        // First, find all atomic events that touch this range.
        let mut relevant_atomic_events = self.formatting_events.iter().filter(|event| {
            let event_end_pos = event.start_pos + event.length;
            let event_pos_overlap = event.start_pos < replacement_range.end && event_end_pos >= replacement_range.start;
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
                #[allow(clippy::if_same_then_else)]
                if replacement_range.start < first_atomic_event.start_pos {
                    true // case (a)
                } else if replacement_range.end > atomic_event_end_pos {
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
        let original_len = replacement_range.end - replacement_range.start;
        let size_change = replacement.len() as isize - original_len as isize;

        // Update formatting events based on how they overlap with the replacement
        let mut events_to_keep = Vec::new();

        for mut event in self.formatting_events.drain(..) {
            let event_start = event.start_pos;
            let event_end = event.start_pos + event.length;

            // Now we need to adjust the events to account for the replacement range being snipped out and replaced with
            // the new text.
            // The general semantics are that the new text should have the formatting of the start of the range.
            // Formatting before or after the replacement range should be trimmed to avoid the replacement range.
            // We accomplish this with a series of matches.
            match BoundaryPosition::classify(event_start, replacement_range.start) {
                BoundaryPosition::Before => {
                    // The event starts before the replacement range.
                    match BoundaryPosition::classify(event_end, replacement_range.start) {
                        BoundaryPosition::Before | BoundaryPosition::At => {
                            // The event is entirely before the replacement, so we can just keep it as-is.
                            events_to_keep.push(event);
                        }
                        BoundaryPosition::After => {
                            // The event started before the replacement but crosses into the replacement. Does it go all
                            // the way across it?
                            event.length = match BoundaryPosition::classify(event_end, replacement_range.end) {
                                BoundaryPosition::Before => {
                                    // Event ends before the replacement does, so we just need to adjust its length such
                                    // that it now ends where the replacement_range starts.
                                    replacement_range.start - event_start
                                }
                                BoundaryPosition::At | BoundaryPosition::After => {
                                    // Event spans all the way across the replacement, so we just need to shorten its
                                    // length by the replacement delta.
                                    (event.length as isize + size_change) as usize
                                }
                            };
                            events_to_keep.push(event);
                        }
                    }
                }
                BoundaryPosition::At => {
                    // Event starts exactly at replacement start
                    match BoundaryPosition::classify(event_end, replacement_range.end) {
                        BoundaryPosition::At | BoundaryPosition::Before => {
                            // If the replacement is empty, we can just discard this formatting event. Otherwise, the
                            // event's new length is just the replacement text's length.
                            if !replacement.is_empty() {
                                // Preserve with new length
                                event.length = replacement.len();
                                events_to_keep.push(event);
                            }
                        }
                        BoundaryPosition::After => {
                            // Event starts at replacement start, ends after. We can keep it, we just need to adjust
                            // its length to account for the replacement range changing in length.
                            event.length = (event.length as isize + size_change) as usize;
                            events_to_keep.push(event);
                        }
                    }
                }
                BoundaryPosition::After => {
                    // Event starts after replacement start
                    match BoundaryPosition::classify(event_start, replacement_range.end) {
                        BoundaryPosition::Before => {
                            // Event starts within replacement. When does it end?
                            match BoundaryPosition::classify(event_end, replacement_range.end) {
                                BoundaryPosition::At | BoundaryPosition::Before => {
                                    // Event completely within replacement. We want all the text within the replacement
                                    // to be the format of the replacement's start, so this inner formatting can just
                                    // go away.
                                }
                                BoundaryPosition::After => {
                                    // Event starts within, ends after replacement. We basically want to remove the
                                    // first part of the event, such that the event now starts after the replacement.
                                    let chars_removed_from_event = replacement_range.end - event_start;
                                    event.start_pos =
                                        (replacement_range.start as isize + replacement.len() as isize) as usize;
                                    event.length -= chars_removed_from_event;
                                    events_to_keep.push(event);
                                }
                            }
                        }
                        BoundaryPosition::At | BoundaryPosition::After => {
                            // Event is entirely after the replacement; just adjust its start_pos offset to account for
                            // the replacement range resizing.
                            event.start_pos = (event_start as isize + size_change) as usize;
                            events_to_keep.push(event);
                        }
                    }
                }
            }
        }

        self.formatting_events = events_to_keep;

        // Update the cumulative offset
        self.offset += size_change;

        // Update the last replacement end position
        self.last_replacement_end = replacement_range.end;

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
                // TODO if I record text.len() before I recurse, I can just recurse-and-append atomically, meaning
                //  that I don't even need to create a new Vec and return it; I can just pass in the &mut Vec, as I do
                //  for the text. That'll be a bit more efficient.
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
                let content: &'static str = "1"; // TODO
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

        #[test]
        fn multiple_format_spans_with_space_between_them() {
            let original = inlines![em["hello"], " ", em["world"]];

            let flattened = FlattenedText::from_inlines(original.clone());
            let reconstructed = flattened.unflatten().unwrap();

            assert_eq!(reconstructed, original);
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
                Err(RangeReplacementError::InternalError(format!(
                    "range replacement went backwards: new start {} < last end {}",
                    0, 7
                )))
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

            // The em should remain
            assert_eq!(flattened.formatting_events.len(), 1);
            assert_eq!(
                flattened.formatting_events[0],
                FormattingEvent {
                    start_pos: 4,
                    length: 1,
                    formatting: FormattingType::Span(SpanVariant::Emphasis),
                }
            )
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

            /// Tests for boundary conditions when replacements start or end exactly at event borders
            mod boundary_conditions {
                use super::*;

                #[test]
                fn replacement_starts_at_event_start() {
                    // Test case: "**bold text**" -> replace "bold" with "strong"
                    // This should result in "**strong text**", not "strong** text**"
                    let mut flattened = FlattenedText {
                        //     ₀123456789
                        text: "bold text".to_string(),
                        formatting_events: vec![FormattingEvent {
                            start_pos: 0,
                            length: 9, // covers "bold text"
                            formatting: FormattingType::Span(SpanVariant::Strong),
                        }],
                        offset: 0,
                        last_replacement_end: 0,
                    };

                    // Replace "bold" (positions 0..4) with "strong"
                    flattened.replace_range(0..4, "strong").unwrap();

                    assert_eq!(flattened.text, "strong text");
                    assert_eq!(flattened.formatting_events.len(), 1);
                    assert_eq!(flattened.formatting_events[0].start_pos, 0);
                    assert_eq!(flattened.formatting_events[0].length, 11); // "strong text"
                }

                #[test]
                fn replacement_ends_at_event_end() {
                    // Test case: "**text bold**" -> replace "bold" with "strong"
                    // This should result in "**text strong**", not "**text **strong"
                    let mut flattened = FlattenedText {
                        //     ₀123456789
                        text: "text bold".to_string(),
                        formatting_events: vec![FormattingEvent {
                            start_pos: 0,
                            length: 9, // covers "text bold"
                            formatting: FormattingType::Span(SpanVariant::Strong),
                        }],
                        offset: 0,
                        last_replacement_end: 0,
                    };

                    // Replace "bold" (positions 5..9) with "strong"
                    flattened.replace_range(5..9, "strong").unwrap();

                    assert_eq!(flattened.text, "text strong");
                    assert_eq!(flattened.formatting_events.len(), 1);
                    assert_eq!(flattened.formatting_events[0].start_pos, 0);
                    assert_eq!(flattened.formatting_events[0].length, 11); // "text strong"
                }

                #[test]
                fn replacement_starts_at_event_start_with_nested_formatting() {
                    // Test case: "_emphasis and **nested bold**_" -> replace "nested" with "formerly"
                    // This should preserve the nested structure correctly
                    let mut flattened = FlattenedText {
                        //     ₀123456789₁123456789₂123456789₃
                        text: "emphasis and nested bold".to_string(),
                        formatting_events: vec![
                            FormattingEvent {
                                start_pos: 0,
                                length: 24, // covers entire text
                                formatting: FormattingType::Span(SpanVariant::Emphasis),
                            },
                            FormattingEvent {
                                start_pos: 13,
                                length: 11, // covers "nested bold"
                                formatting: FormattingType::Span(SpanVariant::Strong),
                            },
                        ],
                        offset: 0,
                        last_replacement_end: 0,
                    };

                    // Replace "nested" (positions 13..19) with "formerly"
                    flattened.replace_range(13..19, "formerly").unwrap();

                    assert_eq!(flattened.text, "emphasis and formerly bold");
                    assert_eq!(flattened.formatting_events.len(), 2);

                    // Outer emphasis should cover the entire new text
                    assert_eq!(flattened.formatting_events[0].start_pos, 0);
                    assert_eq!(flattened.formatting_events[0].length, 26); // "emphasis and formerly bold"

                    // Inner strong should start at same position but have adjusted length
                    assert_eq!(flattened.formatting_events[1].start_pos, 13);
                    assert_eq!(flattened.formatting_events[1].length, 13); // "formerly bold"
                }
            }
        }
    }
}
