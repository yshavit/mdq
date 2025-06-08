/// Link transformation system for converting between different link formats.
///
/// # Overview
///
/// This module provides a flexible system for transforming links between different
/// formats (inline, reference, etc.) while maintaining proper reference numbering and avoiding
/// conflicts. The system is designed around a delegation pattern that separates the public
/// API from the stateful transformation logic.
///
/// # Architecture
///
/// ## Core Components
/// - [`LinkTransform`]: Public enum defining transformation strategies
/// - [`LinkTransformer`]: Crate-public type for transformations; trivially delegates to [`LinkTransformerStrategy`].
/// - [`LinkTransformerStrategy`]: Internal enum implementing the different transformation strategies
/// - [`LinkTransformation`]: Temporary holder for preprocessing data
/// - [`ReferenceAssigner`]: Stateful numbering system for reference links
///
/// ## Data Flow
/// ```text
/// LinkTransform (user choice)
///       ↓
/// LinkTransformer::from() → LinkTransformerStrategy (with ReferenceAssigner if needed)
///       ↓
/// For each link:
///   LinkTransformation::new() → extract preprocessing data
///       ↓
///   LinkTransformation::apply() → perform transformation using strategy
/// ```
///
/// The two-step process (new → apply) exists because Rust's borrowing rules prevent:
/// ```text
/// let result = transformer.transform(&mut self, link)
/// //           ^^^^^^^^^^^            ^^^^
/// //           first borrow           second borrow
/// ```
///
/// By extracting data in `new()` and applying in `apply()`, we can release the borrow
///
use crate::md_elem::elem::*;
use crate::md_elem::*;
use crate::output::fmt_md_inlines::{InlineElemOptions, LinkLike, MdInlinesWriter};
use crate::util::output::Output;
use clap::ValueEnum;
use std::borrow::Cow;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::ops::Deref;

/// Whether to render links as inline, reference form, or keep them as they were.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default, ValueEnum)]
#[non_exhaustive]
pub enum LinkTransform {
    /// Keep links as they were in the original
    Keep,

    /// Turn all links into inlined form: `[link text](https://example.com)`
    Inline,

    /// Turn all links into reference form: `[link text][1]`
    ///
    /// Links that weren't already in reference form will be auto-assigned a reference id. Links that were in reference
    /// form will have the link number be reordered.
    Reference,

    #[default]
    /// Keep `[full][123]`, `[collapsed][]`, and `[shortcut]` links unchanged, but replace
    /// `[inlined](https://example.com)` links.
    ///
    /// The current implementation will turn them into full-style links with incrementing numbers, but this may
    /// change in future versions.
    NeverInline,
}

/// The crate-public orchestrator for link transformations.
//
// # Internal documentation for just this file:
//
// This struct just provides a crate-public interface to the real logic, which is all in LinkTransformerStrategy.
pub(crate) struct LinkTransformer {
    delegate: LinkTransformerStrategy,
}

/// Represents the display text of a link, which can be either plain text or formatted inlines.
///
/// This enum handles the two ways link text can be represented in the markdown AST:
/// - Simple text strings (most common case)
/// - Complex inline formatting like `[**bold** text](url)`
///
/// Used by [`ReferenceAssigner`] when it needs to convert link labels to strings for reference IDs
/// (e.g., `[text][]` → `[text][text]`).
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub(crate) enum LinkLabel<'md> {
    /// Plain text label, potentially borrowed from the original markdown
    Text(Cow<'md, str>),
    /// Complex label with inline formatting (emphasis, strong, etc.)
    Inline(&'md Vec<Inline>),
}

impl<'md> LinkLabel<'md> {
    /// Converts the label to a plain string for use in reference IDs, such that they can be sorted when writing the
    /// reference definitions.
    ///
    /// - For [`LinkLabel::Text`], this is straightforward string conversion.
    /// - For [`LinkLabel::Inline`], this renders the formatted inlines back to markdown text
    ///   (preserving formatting like `**bold**`).
    pub(crate) fn get_sort_string(&self, ctx: &'md MdContext) -> String {
        // There may be a way to Cow this so that we don't have to copy the ::Text string, but I can't find it.
        match self {
            LinkLabel::Text(s) => s.to_string(),
            LinkLabel::Inline(inlines) => {
                let mut inline_writer = MdInlinesWriter::new(
                    ctx,
                    InlineElemOptions {
                        link_format: LinkTransform::Keep,
                        renumber_footnotes: false,
                    },
                );
                inlines_to_string(&mut inline_writer, inlines)
            }
        }
    }
}

impl From<LinkTransform> for LinkTransformer {
    fn from(value: LinkTransform) -> Self {
        let delegate = match value {
            LinkTransform::Keep => LinkTransformerStrategy::Keep,
            LinkTransform::Inline => LinkTransformerStrategy::Inline,
            LinkTransform::Reference => LinkTransformerStrategy::Reference(ReferenceAssigner::new()),
            LinkTransform::NeverInline => LinkTransformerStrategy::NeverInline(ReferenceAssigner::new()),
        };
        Self { delegate }
    }
}

/// Strategy implementations for different link transformation approaches.
enum LinkTransformerStrategy {
    Keep,
    Inline,
    Reference(ReferenceAssigner),
    NeverInline(ReferenceAssigner),
}

/// Temporary holder for preprocessing data needed during link transformation.
///
/// This struct exists to work around Rust's borrowing rules by separating the
/// data extraction phase from the transformation phase. It holds any preprocessing
/// data that needs to be computed before the actual transformation can occur.
///
/// # Borrowing Workaround
/// Without this pattern, we'd need to do:
/// ```text
/// let result = transformer.transform(&mut self, link)
/// //           ^^^^^^^^^^^            ^^^^
/// //           first borrow           second borrow (ERROR!)
/// ```
///
/// Instead, we can do:
/// ```text
/// let transformation = LinkTransformation::new(variant, writer, link); // extract data
/// let result = transformation.apply(&mut transformer, link_ref);       // use data
/// ```
pub(crate) struct LinkTransformation<'md> {
    /// Text extracted from collapsed/shortcut links for use as reference IDs.
    /// Only populated for [`LinkTransform::Reference`] and [`LinkTransform::NeverInline`] when processing
    /// [`LinkReference::Collapsed`] or [`LinkReference::Shortcut`].
    link_text: Option<Cow<'md, str>>,
}

impl<'md> LinkTransformation<'md> {
    /// Extracts preprocessing data needed for the given transformation strategy.
    pub(crate) fn new<L>(transform: LinkTransform, inline_writer: &mut MdInlinesWriter<'md>, item: L) -> Self
    where
        L: LinkLike<'md> + Copy,
    {
        let link_text = match transform {
            LinkTransform::Keep | LinkTransform::Inline => None,
            LinkTransform::Reference | LinkTransform::NeverInline => {
                let (_, label, definition) = item.link_info();
                match &definition.reference {
                    LinkReference::Inline | LinkReference::Full(_) => None,
                    LinkReference::Collapsed | LinkReference::Shortcut => {
                        let text = match label {
                            LinkLabel::Text(text) => text,
                            LinkLabel::Inline(text) => Cow::Owned(inlines_to_string(inline_writer, text)),
                        };
                        Some(text)
                    }
                }
            }
        };
        Self { link_text }
    }

    /// Applies the transformation using the preprocessed data and transformer state.
    // We could in principle return a Cow<'md, LinkReference>, and save some clones in the assigner.
    // To do that, fmt_md_inlines.rs would need to adjust to hold Cows instead of LinkLabels directly. For now, not
    // a high priority.
    pub(crate) fn apply(self, transformer: &mut LinkTransformer, link: &'md LinkReference) -> LinkReference {
        match &mut transformer.delegate {
            LinkTransformerStrategy::Keep => Cow::Borrowed(link),
            LinkTransformerStrategy::Inline => Cow::Owned(LinkReference::Inline),
            LinkTransformerStrategy::Reference(assigner) => assigner.assign(self, link),
            LinkTransformerStrategy::NeverInline(assigner) => match &link {
                LinkReference::Inline => assigner.assign_new(),
                LinkReference::Full(prev) => assigner.assign_if_numeric(prev).unwrap_or(Cow::Borrowed(link)),
                a @ LinkReference::Collapsed | a @ LinkReference::Shortcut => {
                    let text_cow = self.link_text.unwrap();
                    assigner
                        .assign_if_numeric(text_cow.deref())
                        .unwrap_or(Cow::Borrowed(*a))
                }
            },
        }
        .into_owned()
    }
}

impl LinkTransformer {
    /// Returns the transformation variant this transformer was created with.
    pub(crate) fn transform_variant(&self) -> LinkTransform {
        match self.delegate {
            LinkTransformerStrategy::Keep => LinkTransform::Keep,
            LinkTransformerStrategy::Inline => LinkTransform::Inline,
            LinkTransformerStrategy::Reference(_) => LinkTransform::Reference,
            LinkTransformerStrategy::NeverInline(_) => LinkTransform::NeverInline,
        }
    }
}

/// Manages reference numbering for link transformations that need auto-assigned IDs.
struct ReferenceAssigner {
    /// The next number to assign to a new reference.
    ///
    /// Let's not worry about overflow. The minimum size for each link is 5 bytes (`[][1]`), so u64 of those is about
    ///  80 exabytes of markdown -- and it would be even bigger than that, since the digits get bigger. It's just not a
    /// case I'm too worried about right now.
    next_index: u64,

    /// Mappings from old numeric reference IDs to their new assigned numbers.
    ///
    /// This ensures that multiple references to the same ID get consistently
    /// renumbered. We store these as Strings (not ints) so that we don't need to worry
    /// about overflow when parsing very large numbers.
    reorderings: HashMap<String, u64>,
}

impl ReferenceAssigner {
    fn new() -> Self {
        Self {
            next_index: 1,
            reorderings: HashMap::with_capacity(16), // arbitrary
        }
    }

    /// Assigns a reference ID for the given link using the Reference transform strategy.
    ///
    /// This method implements the full reference transformation logic:
    /// - **Inline links**: Get a new auto-assigned number
    /// - **Full references**: Renumber if numeric, otherwise keep as-is
    /// - **Collapsed/Shortcut**: Use the link text as reference ID, renumbering if it's numeric
    ///
    /// # Reference ID Logic
    /// For collapsed/shortcut links, the link text becomes the reference ID:
    /// - `[example][]` → `[example][example]` (text is "example")
    /// - `[123][]` → `[123][1]` (text is "123", gets renumbered)
    /// - `[_123_][]` → `[_123_][_123_]` (text is "_123_", not purely numeric)
    fn assign<'md>(&mut self, state: LinkTransformation<'md>, link: &'md LinkReference) -> Cow<'md, LinkReference> {
        match &link {
            LinkReference::Inline => self.assign_new(),
            LinkReference::Full(prev) => self.assign_if_numeric(prev).unwrap_or(Cow::Borrowed(link)),
            LinkReference::Collapsed | LinkReference::Shortcut => {
                let text_cow = state.link_text.unwrap();
                self.assign_if_numeric(text_cow.deref()).unwrap_or_else(|| {
                    let ref_text_owned = String::from(text_cow.deref());
                    Cow::Owned(LinkReference::Full(ref_text_owned))
                })
            }
        }
    }

    /// Attempts to renumber a reference ID if it's purely numeric.
    fn assign_if_numeric<'md>(&mut self, prev: &str) -> Option<Cow<'md, LinkReference>> {
        if prev.chars().all(|ch| ch.is_numeric()) {
            match self.reorderings.entry(String::from(prev)) {
                Entry::Occupied(map_to) => Some(Cow::Owned(LinkReference::Full(map_to.get().to_string()))),
                Entry::Vacant(e) => {
                    e.insert(self.next_index);
                    Some(self.assign_new())
                }
            }
        } else {
            None
        }
    }

    /// Assigns a new auto-incremented reference number.
    fn assign_new<'md>(&mut self) -> Cow<'md, LinkReference> {
        let idx_str = self.next_index.to_string();
        self.next_index += 1;
        Cow::Owned(LinkReference::Full(idx_str))
    }
}

/// Converts a vector of inline elements back to markdown string format.
///
/// Unlike [crate::output::fmt_plain_str::inlines_to_plain_string], this respects formatting spans
/// like emphasis, strong, etc.
fn inlines_to_string<'md>(inline_writer: &mut MdInlinesWriter<'md>, inlines: &'md Vec<Inline>) -> String {
    let mut string_writer = Output::without_text_wrapping(String::with_capacity(32)); // guess at capacity
    inline_writer.write_line(&mut string_writer, inlines);
    string_writer
        .take_underlying()
        .expect("internal error while parsing collapsed- or shortcut-style link")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::util::utils_for_test::*;

    enum Combo {
        Of(LinkTransform, LinkReference),
    }

    variants_checker!(VARIANTS_CHECKER = Combo {
        Of(LinkTransform::Keep, LinkReference::Inline),
        Of(LinkTransform::Keep, LinkReference::Collapsed),
        Of(LinkTransform::Keep, LinkReference::Full(_)),
        Of(LinkTransform::Keep, LinkReference::Shortcut),

        Of(LinkTransform::Inline, LinkReference::Shortcut),
        Of(LinkTransform::Inline, LinkReference::Collapsed),
        Of(LinkTransform::Inline, LinkReference::Full(_)),
        Of(LinkTransform::Inline, LinkReference::Inline),

        Of(LinkTransform::Reference, LinkReference::Collapsed),
        Of(LinkTransform::Reference, LinkReference::Full(_)),
        Of(LinkTransform::Reference, LinkReference::Inline),
        Of(LinkTransform::Reference, LinkReference::Shortcut),

        Of(LinkTransform::NeverInline, LinkReference::Collapsed),
        Of(LinkTransform::NeverInline, LinkReference::Full(_)),
        Of(LinkTransform::NeverInline, LinkReference::Inline),
        Of(LinkTransform::NeverInline, LinkReference::Shortcut),
    });

    mod keep {
        use super::*;

        #[test]
        fn inline() {
            check_keep(LinkReference::Inline);
        }

        #[test]
        fn collapsed() {
            check_keep(LinkReference::Collapsed);
        }

        #[test]
        fn full() {
            check_keep(LinkReference::Full("5".to_string()));
        }

        #[test]
        fn shortcut() {
            check_keep(LinkReference::Shortcut);
        }

        fn check_keep(link_ref: LinkReference) {
            Given {
                transform: LinkTransform::Keep,
                label: mdq_inline!("doesn't matter"),
                orig_reference: link_ref.clone(),
            }
            .expect(link_ref);
        }
    }

    mod inline {
        use super::*;

        #[test]
        fn inline() {
            // We could in principle have this return a Borrowed Cow, since the input and output are both Inline.
            // But it's not really worth it, given that Inline is just a stateless enum variant and thus as cheap
            // (or potentially even cheaper!) than a pointer.
            check_inline(LinkReference::Inline);
        }

        #[test]
        fn collapsed() {
            check_inline(LinkReference::Collapsed);
        }

        #[test]
        fn full() {
            check_inline(LinkReference::Full("5".to_string()));
        }

        #[test]
        fn shortcut() {
            check_inline(LinkReference::Shortcut);
        }

        fn check_inline(link_ref: LinkReference) {
            Given {
                transform: LinkTransform::Inline,
                label: mdq_inline!("doesn't matter"),
                orig_reference: link_ref,
            }
            .expect(LinkReference::Inline);
        }
    }

    mod reference {
        use super::*;

        #[test]
        fn inline() {
            Given {
                transform: LinkTransform::Reference,
                label: mdq_inline!("doesn't matter"),
                orig_reference: LinkReference::Inline,
            }
            .expect(LinkReference::Full("1".to_string()));
        }

        #[test]
        fn collapsed_label_not_number() {
            Given {
                transform: LinkTransform::Reference,
                label: mdq_inline!("not a number"),
                orig_reference: LinkReference::Collapsed,
            }
            .expect(LinkReference::Full("not a number".to_string()));
        }

        #[test]
        fn collapsed_label_is_number() {
            Given {
                transform: LinkTransform::Reference,
                label: mdq_inline!("321"),
                orig_reference: LinkReference::Collapsed,
            }
            .expect(LinkReference::Full("1".to_string()));
        }

        #[test]
        fn full_ref_id_not_number() {
            Given {
                transform: LinkTransform::Reference,
                label: mdq_inline!("doesn't matter"),
                orig_reference: LinkReference::Full("non-number".to_string()),
            }
            .expect(LinkReference::Full("non-number".to_string()));
        }

        #[test]
        fn full_ref_id_is_number() {
            Given {
                transform: LinkTransform::Reference,
                label: mdq_inline!("doesn't matter"),
                orig_reference: LinkReference::Full("non-number".to_string()),
            }
            .expect(LinkReference::Full("non-number".to_string()));
        }

        #[test]
        fn full_ref_id_is_huge_number() {
            let huge_num_str = format!("{}00000", u128::MAX);
            Given {
                transform: LinkTransform::Reference,
                label: mdq_inline!("doesn't matter"),
                orig_reference: LinkReference::Full(huge_num_str),
            }
            .expect(LinkReference::Full("1".to_string()));
        }

        #[test]
        fn shortcut_label_not_number() {
            Given {
                transform: LinkTransform::Reference,
                label: mdq_inline!("not a number"),
                orig_reference: LinkReference::Shortcut,
            }
            .expect(LinkReference::Full("not a number".to_string()));
        }

        #[test]
        fn shortcut_label_is_number() {
            Given {
                transform: LinkTransform::Reference,
                label: mdq_inline!("321"),
                orig_reference: LinkReference::Shortcut,
            }
            .expect(LinkReference::Full("1".to_string()));
        }

        /// The label isn't even close to a number.
        ///
        /// _c.f._ [shortcut_label_inlines_are_emphasized_number]
        #[test]
        fn shortcut_label_inlines_not_number_like() {
            Given {
                transform: LinkTransform::Reference,
                label: mdq_inline!("hello world"),
                orig_reference: LinkReference::Shortcut,
            }
            .expect(LinkReference::Full("hello world".to_string()));
        }

        /// The label is kind of like a number, except that it's emphasized: `_123_`. This makes it not a number.
        ///
        /// _c.f._ [shortcut_label_inlines_not_number_like]
        #[test]
        fn shortcut_label_inlines_are_emphasized_number() {
            Given {
                transform: LinkTransform::Reference,
                label: Inline::Span(Span {
                    variant: SpanVariant::Emphasis,
                    children: vec![Inline::Text(Text {
                        variant: TextVariant::Plain,
                        value: "123".to_string(),
                    })],
                }),
                orig_reference: LinkReference::Shortcut,
            }
            .expect(LinkReference::Full("_123_".to_string()));
        }
    }

    mod never_inline {
        use super::*;

        #[test]
        fn inline() {
            // We could in principle have this return a Borrowed Cow, since the input and output are both Inline.
            // But it's not really worth it, given that Inline is just a stateless enum variant and thus as cheap
            // (or potentially even cheaper!) than a pointer.
            check_never_inline(LinkReference::Inline, LinkReference::Full("1".to_string()));
        }

        #[test]
        fn collapsed() {
            check_never_inline(LinkReference::Collapsed, LinkReference::Collapsed);
        }

        #[test]
        fn full() {
            check_never_inline(
                LinkReference::Full("5".to_string()),
                LinkReference::Full("1".to_string()),
            );
        }

        #[test]
        fn shortcut() {
            check_never_inline(LinkReference::Shortcut, LinkReference::Shortcut);
        }

        fn check_never_inline(link_ref: LinkReference, expect: LinkReference) {
            Given {
                transform: LinkTransform::NeverInline,
                label: mdq_inline!("doesn't matter"),
                orig_reference: link_ref,
            }
            .expect(expect);
        }
    }

    /// A smoke test basically to ensure that we increment values correctly. We won't test every transformation type,
    /// since the sibling sub-modules already do that.
    #[test]
    fn smoke_test_multi() {
        let mut transformer = LinkTransformer::from(LinkTransform::Reference);
        let ctx = MdContext::empty();
        let mut iw = MdInlinesWriter::new(
            &ctx,
            InlineElemOptions {
                link_format: LinkTransform::Keep,
                renumber_footnotes: false,
            },
        );

        // [alpha](https://example.com) ==> [alpha][1]
        let alpha = make_link("alpha", LinkReference::Inline);
        assert_eq!(
            transform(&mut transformer, &mut iw, &alpha),
            LinkReference::Full("1".to_string())
        );

        // [bravo][1] ==> [bravo][2]
        let bravo = make_link("bravo", LinkReference::Full("1".to_string()));
        assert_eq!(
            transform(&mut transformer, &mut iw, &bravo),
            LinkReference::Full("2".to_string())
        );

        // [charlie][] ==> [charlie][charlie]
        let charlie = make_link("charlie", LinkReference::Shortcut);
        assert_eq!(
            transform(&mut transformer, &mut iw, &charlie),
            LinkReference::Full("charlie".to_string())
        );

        // [delta][delta] ==> [delta][delta]
        let delta = make_link("delta", LinkReference::Full("delta".to_string()));
        assert_eq!(
            transform(&mut transformer, &mut iw, &delta),
            LinkReference::Full("delta".to_string())
        );

        // [789] ==> [789][3]
        let echo = make_link("789", LinkReference::Collapsed);
        assert_eq!(
            transform(&mut transformer, &mut iw, &echo),
            LinkReference::Full("3".to_string())
        );

        // [echo] ==> [echo][echo]
        let echo = make_link("echo", LinkReference::Collapsed);
        assert_eq!(
            transform(&mut transformer, &mut iw, &echo),
            LinkReference::Full("echo".to_string())
        );
    }

    #[test]
    fn smoke_test_multi_with_never_inline() {
        let mut transformer = LinkTransformer::from(LinkTransform::NeverInline);
        let ctx = MdContext::empty();
        let mut iw = MdInlinesWriter::new(
            &ctx,
            InlineElemOptions {
                link_format: LinkTransform::Keep,
                renumber_footnotes: false,
            },
        );

        // Start with a collapse, just for fun. It should be unchanged.
        let alpha = make_link("alpha", LinkReference::Collapsed);
        assert_eq!(transform(&mut transformer, &mut iw, &alpha), LinkReference::Collapsed);

        // Inline should turn into [bravo][1]
        let bravo = make_link("bravo", LinkReference::Inline);
        assert_eq!(
            transform(&mut transformer, &mut iw, &bravo),
            LinkReference::Full("1".to_string())
        );

        // [charlie][2] should be unchanged, *and* take up the "2" slot
        let charlie = make_link("charlie", LinkReference::Full("2".to_string()));
        assert_eq!(
            transform(&mut transformer, &mut iw, &charlie),
            LinkReference::Full("2".to_string())
        );

        // [charlie][1] should be reordered to [3]
        let delta = make_link("charlie", LinkReference::Full("1".to_string()));
        assert_eq!(
            transform(&mut transformer, &mut iw, &delta),
            LinkReference::Full("3".to_string())
        );

        // [4][] should stay as-is, but take up the "4" slot
        let echo = make_link("4", LinkReference::Collapsed);
        assert_eq!(
            transform(&mut transformer, &mut iw, &echo),
            LinkReference::Full("4".to_string())
        );

        // [3] should be renumbered to [3][5] (confusing!)
        let foxtrot = make_link("3", LinkReference::Shortcut);
        assert_eq!(
            transform(&mut transformer, &mut iw, &foxtrot),
            LinkReference::Full("5".to_string())
        );

        // The next inline should turn into [bravo][6]
        let golf = make_link("delta", LinkReference::Inline);
        assert_eq!(
            transform(&mut transformer, &mut iw, &golf),
            LinkReference::Full("6".to_string())
        );
    }

    fn transform<'md>(
        transformer: &mut LinkTransformer,
        iw: &mut MdInlinesWriter<'md>,
        link: &'md Link,
    ) -> LinkReference {
        let actual = match link {
            Link::Standard(standard_link) => {
                LinkTransformation::new(transformer.transform_variant(), iw, standard_link)
                    .apply(transformer, &standard_link.link.reference)
            }
            Link::Autolink(autolink) => {
                panic!("unexpected autolink: {autolink:?}")
            }
        };
        actual
    }

    fn make_link(label: &str, link_ref: LinkReference) -> Link {
        Link::Standard(StandardLink {
            display: vec![Inline::Text(Text {
                variant: TextVariant::Plain,
                value: label.to_string(),
            })],
            link: LinkDefinition {
                url: "https://example.com".to_string(),
                title: None,
                reference: link_ref,
            },
        })
    }

    struct Given {
        transform: LinkTransform,
        label: Inline,
        orig_reference: LinkReference,
    }

    impl Given {
        fn expect(self, expected: LinkReference) {
            let Given {
                transform,
                label,
                orig_reference: reference,
            } = self;
            let mut transformer = LinkTransformer::from(transform);
            let ctx = MdContext::empty();
            let mut iw = MdInlinesWriter::new(
                &ctx,
                InlineElemOptions {
                    link_format: LinkTransform::Keep,
                    renumber_footnotes: false,
                },
            );
            let link = Link::Standard(StandardLink {
                display: vec![label],
                link: LinkDefinition {
                    url: "https://example.com".to_string(),
                    title: None,
                    reference: reference.clone(),
                },
            });

            let actual = self::transform(&mut transformer, &mut iw, &link);

            VARIANTS_CHECKER.see(&Combo::Of(transform, reference.clone()));

            assert_eq!(actual, expected);
        }
    }
}
