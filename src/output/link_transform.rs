use crate::md_elem::elem::*;
use crate::md_elem::*;
use crate::output::find_numbered_links::find_reserved_link_numbers;
use crate::output::fmt_md_inlines::{InlineElemOptions, MdInlinesWriter};
use crate::util::number_assigner::NumberAssigner;
use crate::util::output::Output;
use clap::ValueEnum;
use std::borrow::Cow;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

/// Whether to render links as inline, reference form, or keep them as they were.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default, ValueEnum)]
#[non_exhaustive]
pub enum LinkTransform {
    /// Keep links as they were in the original
    Keep,

    /// Turn all links into inlined form: `[link text](https://example.com)`
    Inline,

    #[default]
    /// Never output `[inline](https://example.com)` links.
    ///
    /// The exact implementation is subject to change. As of this version:
    ///
    /// - `[inline](https://example.com)` links will always be turned into `[full][1]` links, with the link id being
    ///   an auto-assigned number
    /// - `[collapsed][]` and `[shortcut]` links will always be unchanged
    /// - `[full][a]` links will:
    ///    - otherwise, be renumbered if the link id is numeric: `[example][3]` → `[example][1]`
    ///    - otherwise, be left unchanged: `[example][a]` is unchanged
    NeverInline,
}

/// The crate-public orchestrator for link transformations.
//
// Internal documentation for just this file:
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
                    &[], // don't need to see any nodes, since that's only needed for non-Keep link formats
                );
                inlines_to_string(&mut inline_writer, inlines)
            }
        }
    }
}

/// Strategy implementations for different link transformation approaches.
enum LinkTransformerStrategy {
    Keep,
    Inline,
    NeverInline(ReferenceAssigner),
}

impl LinkTransformer {
    pub(crate) fn new(transform: LinkTransform, nodes: &[MdElem], ctx: &MdContext) -> Self {
        let delegate = match transform {
            LinkTransform::Keep => LinkTransformerStrategy::Keep,
            LinkTransform::Inline => LinkTransformerStrategy::Inline,
            LinkTransform::NeverInline => LinkTransformerStrategy::NeverInline(ReferenceAssigner::new(nodes, ctx)),
        };
        Self { delegate }
    }

    pub(crate) fn apply(&mut self, link: &LinkReference) -> LinkReference {
        match &mut self.delegate {
            LinkTransformerStrategy::Keep => Cow::Borrowed(link),
            LinkTransformerStrategy::Inline => Cow::Owned(LinkReference::Inline),
            LinkTransformerStrategy::NeverInline(assigner) => match &link {
                LinkReference::Inline => assigner.assign_new(),
                LinkReference::Full(link_id) => {
                    // For full links (`[foo][a]`), only reassign if the link was a number originally.
                    if is_numeric(link_id) {
                        assigner.reassign(link_id)
                    } else {
                        Cow::Borrowed(link)
                    }
                }
                // LinkReference::Full(prev) => assigner.assign_if_numeric(prev).unwrap_or(Cow::Borrowed(link)),
                LinkReference::Collapsed | LinkReference::Shortcut => {
                    // For collapsed and shortcut links, never renumber! This could turn `[123][]` into `[123][6]`, which
                    // is confusing.
                    Cow::Borrowed(link)
                }
            },
        }
        .into_owned()
    }
}

/// Manages reference numbering for link transformations that need auto-assigned IDs.
struct ReferenceAssigner {
    /// The next number to assign to a new reference.
    ///
    /// Let's not worry about overflow. The minimum size for each link is 5 bytes (`[][1]`), so u64 of those is about
    ///  80 exabytes of markdown -- and it would be even bigger than that, since the digits get bigger. It's just not a
    /// case I'm too worried about right now.
    index_assigner: NumberAssigner,

    /// Mappings from old numeric reference IDs to their new assigned numbers.
    ///
    /// This ensures that multiple references to the same ID get consistently
    /// renumbered. We store these as Strings (not ints) so that we don't need to worry
    /// about overflow when parsing very large numbers.
    reorderings: HashMap<String, u64>,
}

impl ReferenceAssigner {
    fn new(nodes: &[MdElem], ctx: &MdContext) -> Self {
        let reserved_ints = find_reserved_link_numbers(nodes.iter(), ctx);
        let index_assigner = NumberAssigner::new(reserved_ints);

        Self {
            index_assigner,
            reorderings: HashMap::with_capacity(16), // arbitrary
        }
    }

    /// Attempts to renumber a reference ID if it's purely numeric.
    fn reassign<'md>(&mut self, prev: &str) -> Cow<'md, LinkReference> {
        match self.reorderings.entry(String::from(prev)) {
            Entry::Occupied(map_to) => Cow::Owned(LinkReference::Full(map_to.get().to_string())),
            Entry::Vacant(e) => {
                let num = self.index_assigner.next_num();
                e.insert(num);
                Cow::Owned(LinkReference::Full(num.to_string()))
            }
        }
    }

    /// Assigns a new auto-incremented reference number.
    fn assign_new<'md>(&mut self) -> Cow<'md, LinkReference> {
        let idx_str = self.index_assigner.next_num().to_string();
        Cow::Owned(LinkReference::Full(idx_str))
    }
}

/// Converts a vector of inline elements back to markdown string format.
///
/// Unlike [crate::output::fmt_plain_str::inlines_to_plain_string], this respects formatting spans
/// like emphasis, strong, etc.
pub(crate) fn inlines_to_string<'md>(inline_writer: &mut MdInlinesWriter<'md>, inlines: &'md Vec<Inline>) -> String {
    let mut string_writer = Output::without_text_wrapping(String::with_capacity(32)); // guess at capacity
    inline_writer.write_line(&mut string_writer, inlines);
    string_writer
        .take_underlying()
        .expect("internal error while parsing collapsed- or shortcut-style link")
}

fn is_numeric(text: &str) -> bool {
    if text.is_empty() {
        false
    } else if text.as_bytes()[0] == b'0' {
        // "01" for example; don't renumber those
        false
    } else {
        text.as_bytes().iter().all(u8::is_ascii_digit)
    }
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
                LinkReference::Full("1".to_string()), // renumbered
            );
        }

        #[test]
        fn full_with_numeric_display() {
            Given {
                transform: LinkTransform::NeverInline,
                label: mdq_inline!("123"),
                orig_reference: LinkReference::Full("a".to_string()),
            }
            .expect(LinkReference::Full("a".to_string()));
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

    #[test]
    fn smoke_test_multi_with_never_inline() {
        let a_collapsed = make_link("alpha", LinkReference::Collapsed);
        let b_inline = make_link("bravo", LinkReference::Inline);
        let c_full_2 = make_link("charlie", LinkReference::Full("2".to_string()));
        let d_full_1 = make_link("delta", LinkReference::Full("1".to_string()));
        let e_4_collapsed = make_link("4", LinkReference::Collapsed);
        let f_3_shortcut = make_link("3", LinkReference::Shortcut);
        let g_inline = make_link("golf", LinkReference::Inline);

        let as_md_elem: [MdElem; 7] = [
            &a_collapsed,
            &b_inline,
            &c_full_2,
            &d_full_1,
            &e_4_collapsed,
            &f_3_shortcut,
            &g_inline,
        ]
        .map(Link::clone)
        .map(Link::into);

        let ctx = MdContext::empty();
        let mut transformer = LinkTransformer::new(LinkTransform::NeverInline, as_md_elem.as_slice(), &ctx);

        // Start with a collapse, just for fun. It should be unchanged.
        assert_eq!(transform(&mut transformer, &a_collapsed), LinkReference::Collapsed);

        // Inline should turn into [bravo][1]
        assert_eq!(
            transform(&mut transformer, &b_inline),
            LinkReference::Full("1".to_string())
        );

        // [charlie][2] should be unchanged, *and* take up the "2" slot
        assert_eq!(
            transform(&mut transformer, &c_full_2),
            LinkReference::Full("2".to_string())
        );

        // [delta][1] should be reordered to [6], since the existing `[3]` and `[4][]`,  take those slots
        assert_eq!(
            transform(&mut transformer, &d_full_1),
            LinkReference::Full("5".to_string())
        );

        // [4][] should stay as-is, and take up the "4" slot
        assert_eq!(transform(&mut transformer, &e_4_collapsed), LinkReference::Collapsed);

        // [3] should stay as-is, and take up the "3" slot
        assert_eq!(transform(&mut transformer, &f_3_shortcut), LinkReference::Shortcut);

        // [golf](...) should turn into [golf][6]
        assert_eq!(
            transform(&mut transformer, &g_inline),
            LinkReference::Full("6".to_string())
        );
    }

    mod is_numeric {
        use super::*;

        #[test]
        fn only_digits() {
            assert!(is_numeric("123"));
        }

        #[test]
        fn leading_zero() {
            assert!(!is_numeric("012"));
        }

        #[test]
        fn empty() {
            assert!(!is_numeric(""));
        }

        #[test]
        fn non_digits() {
            assert!(!is_numeric("hello"));
        }

        #[test]
        fn unicode_numerics() {
            assert!(!is_numeric("\u{2174}")); // Roman numeral "ⅴ"
        }
    }

    fn transform(transformer: &mut LinkTransformer, link: &Link) -> LinkReference {
        match link {
            Link::Standard(standard_link) => transformer.apply(&standard_link.link.reference),
            Link::Autolink(autolink) => {
                panic!("unexpected autolink: {autolink:?}")
            }
        }
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

            let link = Link::Standard(StandardLink {
                display: vec![label],
                link: LinkDefinition {
                    url: "https://example.com".to_string(),
                    title: None,
                    reference: reference.clone(),
                },
            });

            let link_md_slice: [MdElem; 1] = [link.clone().into()];

            let ctx = MdContext::empty();
            let mut transformer = LinkTransformer::new(transform, &link_md_slice, &ctx);

            let actual = self::transform(&mut transformer, &link);

            VARIANTS_CHECKER.see(&Combo::Of(transform, reference.clone()));

            assert_eq!(actual, expected);
        }
    }
}
