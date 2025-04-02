use crate::fmt_md_inlines::{LinkLike, MdInlinesWriter, MdInlinesWriterOptions};
use crate::output::Output;
use crate::tree::{Inline, LinkReference, MdContext};
use clap::ValueEnum;
use std::borrow::Cow;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::ops::Deref;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
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
}

pub struct LinkTransformer {
    delegate: LinkTransformState,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum LinkLabel<'md> {
    Text(Cow<'md, str>),
    Inline(&'md Vec<Inline>),
}

impl<'md> LinkLabel<'md> {
    pub fn get_sort_string(&self, ctx: &'md MdContext) -> String {
        // There may be a way to Cow this so that we don't have to copy the ::Text string, but I can't find it.
        match self {
            LinkLabel::Text(s) => s.to_string(),
            LinkLabel::Inline(inlines) => {
                let mut inline_writer = MdInlinesWriter::new(
                    ctx,
                    MdInlinesWriterOptions {
                        link_format: LinkTransform::Keep,
                        renumber_footnotes: false,
                    },
                );
                inlines_to_string(&mut inline_writer, *inlines)
            }
        }
    }
}

impl From<LinkTransform> for LinkTransformer {
    fn from(value: LinkTransform) -> Self {
        let delegate = match value {
            LinkTransform::Keep => LinkTransformState::Keep,
            LinkTransform::Inline => LinkTransformState::Inline,
            LinkTransform::Reference => LinkTransformState::Reference(ReferenceAssigner::new()),
        };
        Self { delegate }
    }
}

enum LinkTransformState {
    Keep,
    Inline,
    Reference(ReferenceAssigner),
}

pub struct LinkTransformation<'md> {
    link_text: Option<Cow<'md, str>>,
}

/// A temporary holder to perform link transformations.
///
/// To use:
///
/// ```compile_fail
///     // self: MdInlinesWriter
///     let link_ref = LinkTransformation::new(self.link_transformer.transform_variant(), self, link_like)
///         .apply(&mut self.link_transformer, &link.reference);
/// ```
///
/// We need this because ownership prohibits:
///
/// ```compile_fail
///     let link_ref = self.link_transformer.transform(self, link_like)
///                    ^^^^^^^^^^^^^^^^^^^^^           ^^^^
///                    first borrow                    second borrow
/// ```
///
/// This lets us use the `transform_variant()`'s Copy-ability to release the borrow.
impl<'md> LinkTransformation<'md> {
    pub fn new<L>(transform: LinkTransform, inline_writer: &mut MdInlinesWriter<'md>, item: L) -> Self
    where
        L: LinkLike<'md> + Copy,
    {
        let link_text = match transform {
            LinkTransform::Keep | LinkTransform::Inline => None,
            LinkTransform::Reference => {
                let (_, label, definition) = item.link_info();
                match &definition.reference {
                    LinkReference::Inline | LinkReference::Full(_) => None,
                    LinkReference::Collapsed | LinkReference::Shortcut => {
                        let text = match label {
                            LinkLabel::Text(text) => Cow::from(text),
                            LinkLabel::Inline(text) => Cow::Owned(inlines_to_string(inline_writer, text)),
                        };
                        Some(text)
                    }
                }
            }
        };
        Self { link_text }
    }

    // We could in principle return a Cow<'md, LinkReference>, and save some clones in the assigner.
    // To do that, fmt_md_inlines.rs would need to adjust to hold Cows instead of LinkLabels directly. For now, not
    // a high priority.
    pub fn apply(self, transformer: &mut LinkTransformer, link: &'md LinkReference) -> LinkReference {
        match &mut transformer.delegate {
            LinkTransformState::Keep => Cow::Borrowed(link),
            LinkTransformState::Inline => Cow::Owned(LinkReference::Inline),
            LinkTransformState::Reference(assigner) => assigner.assign(self, link),
        }
        .into_owned()
    }
}

impl LinkTransformer {
    pub fn transform_variant(&self) -> LinkTransform {
        match self.delegate {
            LinkTransformState::Keep => LinkTransform::Keep,
            LinkTransformState::Inline => LinkTransform::Inline,
            LinkTransformState::Reference(_) => LinkTransform::Reference,
        }
    }
}

struct ReferenceAssigner {
    /// Let's not worry about overflow. The minimum size for each link is 5 bytes (`[][1]`), so u64 of those is about
    ///  80 exabytes of markdown -- and it would be even bigger than that, since the digits get bigger. It's just not a
    /// case I'm too worried about right now.
    next_index: u64,
    /// Mappings from old reference id to new. We store these as an Strings (not ints) so that we don't need to worry
    /// about overflow.
    reorderings: HashMap<String, u64>,
}

impl ReferenceAssigner {
    fn new() -> Self {
        Self {
            next_index: 1,
            reorderings: HashMap::with_capacity(16), // arbitrary
        }
    }

    fn assign<'md>(&mut self, state: LinkTransformation<'md>, link: &'md LinkReference) -> Cow<'md, LinkReference> {
        match &link {
            LinkReference::Inline => self.assign_new(),
            LinkReference::Full(prev) => self.assign_if_numeric(prev).unwrap_or_else(|| Cow::Borrowed(link)),
            LinkReference::Collapsed | LinkReference::Shortcut => {
                let text_cow = state.link_text.unwrap();
                self.assign_if_numeric(text_cow.deref()).unwrap_or_else(|| {
                    let ref_text_owned = String::from(text_cow.deref());
                    Cow::Owned(LinkReference::Full(ref_text_owned))
                })
            }
        }
    }

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

    fn assign_new<'md>(&mut self) -> Cow<'md, LinkReference> {
        let idx_str = self.next_index.to_string();
        self.next_index += 1;
        Cow::Owned(LinkReference::Full(idx_str))
    }
}

/// Turns the inlines into a String. Unlike [crate::fmt_str::inlines_to_plain_string], this respects formatting spans
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
    use crate::tree::{Link, LinkDefinition, MdContext, Text, TextVariant};
    use crate::tree_test_utils::*;
    use crate::utils_for_test::*;

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
        use crate::tree::{Formatting, FormattingVariant};

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
                label: Inline::Formatting(Formatting {
                    variant: FormattingVariant::Emphasis,
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

    /// A smoke test basically to ensure that we increment values correctly. We won't test every transformation type,
    /// since the sibling sub-modules already do that.
    #[test]
    fn smoke_test_multi() {
        let mut transformer = LinkTransformer::from(LinkTransform::Reference);
        let ctx = MdContext::empty();
        let mut iw = MdInlinesWriter::new(
            &ctx,
            MdInlinesWriterOptions {
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

    fn transform<'md>(
        mut transformer: &mut LinkTransformer,
        mut iw: &mut MdInlinesWriter<'md>,
        link: &'md Link,
    ) -> LinkReference {
        let actual = LinkTransformation::new(transformer.transform_variant(), &mut iw, link)
            .apply(&mut transformer, &link.link_definition.reference);
        actual
    }

    fn make_link(label: &str, link_ref: LinkReference) -> Link {
        let link = Link {
            text: vec![Inline::Text(Text {
                variant: TextVariant::Plain,
                value: label.to_string(),
            })],
            link_definition: LinkDefinition {
                url: "https://example.com".to_string(),
                title: None,
                reference: link_ref,
            },
        };
        link
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
                MdInlinesWriterOptions {
                    link_format: LinkTransform::Keep,
                    renumber_footnotes: false,
                },
            );
            let link = Link {
                text: vec![label],
                link_definition: LinkDefinition {
                    url: "https://example.com".to_string(),
                    title: None,
                    reference: reference.clone(),
                },
            };

            let actual = self::transform(&mut transformer, &mut iw, &link);

            VARIANTS_CHECKER.see(&Combo::Of(transform, reference.clone()));

            assert_eq!(actual, expected);
        }
    }
}
