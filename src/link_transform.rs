use crate::fmt_md_inlines::{MdInlinesWriter, MdInlinesWriterOptions};
use crate::output::Output;
use crate::tree::{Inline, LinkReference};
use clap::ValueEnum;
use std::borrow::Cow;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::Display;
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
pub enum LinkLabel<'a> {
    Text(Cow<'a, str>),
    Inline(&'a Vec<Inline>),
}

impl<'a> Display for LinkLabel<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LinkLabel::Text(s) => f.write_str(s),
            LinkLabel::Inline(inlines) => {
                let mut inline_writer = MdInlinesWriter::new(MdInlinesWriterOptions {
                    link_canonicalization: LinkTransform::Keep,
                });
                f.write_str(&inlines_to_string(&mut inline_writer, *inlines))
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

impl LinkTransformer {
    pub fn transform<'a>(
        &mut self,
        inline_writer: &mut MdInlinesWriter<'a>,
        link: &'a LinkReference,
        label: &LinkLabel<'a>,
    ) -> Cow<'a, LinkReference> {
        match &mut self.delegate {
            LinkTransformState::Keep => Cow::Borrowed(&link),
            LinkTransformState::Inline => Cow::Owned(LinkReference::Inline),
            LinkTransformState::Reference(assigner) => assigner.assign(inline_writer, link, label),
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

    fn assign<'a>(
        &mut self,
        inline_writer: &mut MdInlinesWriter<'a>,
        link: &'a LinkReference,
        label: &LinkLabel<'a>,
    ) -> Cow<'a, LinkReference> {
        match &link {
            LinkReference::Inline => self.assign_new(),
            LinkReference::Full(prev) => self.assign_if_numeric(prev).unwrap_or_else(|| Cow::Borrowed(link)),
            LinkReference::Collapsed | LinkReference::Shortcut => {
                let text_cow = match label {
                    LinkLabel::Text(text) => Cow::from(text.deref()),
                    LinkLabel::Inline(text) => Cow::Owned(inlines_to_string(inline_writer, text)),
                };
                self.assign_if_numeric(text_cow.deref()).unwrap_or_else(|| {
                    let ref_text_owned = String::from(text_cow.deref());
                    Cow::Owned(LinkReference::Full(ref_text_owned))
                })
            }
        }
    }

    fn assign_if_numeric<'a>(&mut self, prev: &str) -> Option<Cow<'a, LinkReference>> {
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

    fn assign_new<'a>(&mut self) -> Cow<'a, LinkReference> {
        let idx_str = self.next_index.to_string();
        self.next_index += 1;
        Cow::Owned(LinkReference::Full(idx_str))
    }
}

/// Turns the inlines into a String. Unlike [crate::fmt_str::inlines_to_plain_string], this respects formatting spans
/// like emphasis, strong, etc.
fn inlines_to_string<'a>(inline_writer: &mut MdInlinesWriter<'a>, inlines: &'a Vec<Inline>) -> String {
    let mut string_writer = Output::new(String::with_capacity(32)); // guess at capacity
    inline_writer.write_line(&mut string_writer, inlines);
    string_writer
        .take_underlying()
        .expect("internal error while parsing collapsed- or shortcut-style link")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::utils_for_test::*;
    use crate::variants_checker;

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
            check_keep(&LinkReference::Inline);
        }

        #[test]
        fn collapsed() {
            check_keep(&LinkReference::Collapsed);
        }

        #[test]
        fn full() {
            check_keep(&LinkReference::Full("5".to_string()));
        }

        #[test]
        fn shortcut() {
            check_keep(&LinkReference::Shortcut);
        }

        fn check_keep(link_ref: &LinkReference) {
            check_one(
                LinkTransform::Keep,
                link_ref,
                &LinkLabel::Text(Cow::Borrowed("text label")),
                &Cow::Borrowed(link_ref),
            );
        }
    }

    mod inline {
        use super::*;

        #[test]
        fn inline() {
            // We could in principle have this return a Borrowed Cow, since the input and output are both Inline.
            // But it's not really worth it, given that Inline is just a stateless enum variant and thus as cheap
            // (or potentially even cheaper!) than a pointer.
            check_inline(&LinkReference::Inline);
        }

        #[test]
        fn collapsed() {
            check_inline(&LinkReference::Collapsed);
        }

        #[test]
        fn full() {
            check_inline(&LinkReference::Full("5".to_string()));
        }

        #[test]
        fn shortcut() {
            check_inline(&LinkReference::Shortcut);
        }

        fn check_inline(link_ref: &LinkReference) {
            check_one(
                LinkTransform::Inline,
                link_ref,
                &LinkLabel::Text(Cow::Borrowed("text label")),
                &Cow::Owned(LinkReference::Inline),
            );
        }
    }

    mod reference {
        use super::*;
        use crate::tree::{Formatting, FormattingVariant, Text, TextVariant};

        #[test]
        fn inline() {
            check_one(
                LinkTransform::Reference,
                &LinkReference::Inline,
                &LinkLabel::Text(Cow::Borrowed("doesn't matter")),
                &Cow::Owned(LinkReference::Full("1".to_string())),
            )
        }

        #[test]
        fn collapsed_label_not_number() {
            check_one(
                LinkTransform::Reference,
                &LinkReference::Collapsed,
                &LinkLabel::Text(Cow::Borrowed("not a number")),
                &Cow::Owned(LinkReference::Full("not a number".to_string())),
            )
        }

        #[test]
        fn collapsed_label_is_number() {
            check_one(
                LinkTransform::Reference,
                &LinkReference::Collapsed,
                &LinkLabel::Text(Cow::Borrowed("5")),
                &Cow::Owned(LinkReference::Full("1".to_string())), // always count from 1
            )
        }

        #[test]
        fn full_ref_id_not_number() {
            let reference = LinkReference::Full("non-number".to_string());
            let reference_cow: Cow<LinkReference> = Cow::Borrowed(&reference);

            check_one(
                LinkTransform::Reference,
                &reference,
                &LinkLabel::Text(Cow::Borrowed("doesn't matter")),
                &reference_cow,
            )
        }

        #[test]
        fn full_ref_id_is_number() {
            check_one(
                LinkTransform::Reference,
                &LinkReference::Full("321".to_string()), // what number it is doesn't matter
                &LinkLabel::Text(Cow::Borrowed("doesn't matter")),
                &Cow::Owned(LinkReference::Full("1".to_string())),
            )
        }

        #[test]
        fn full_ref_id_is_huge_number() {
            let huge_num_str = format!("{}00000", u128::MAX);
            check_one(
                LinkTransform::Reference,
                &LinkReference::Full(huge_num_str), // what number it is doesn't matter
                &LinkLabel::Text(Cow::Borrowed("doesn't matter")),
                &Cow::Owned(LinkReference::Full("1".to_string())), // always count from 1
            )
        }

        #[test]
        fn shortcut_label_not_number() {
            check_one(
                LinkTransform::Reference,
                &LinkReference::Shortcut,
                &LinkLabel::Text(Cow::Borrowed("not a number")),
                &Cow::Owned(LinkReference::Full("not a number".to_string())),
            )
        }

        #[test]
        fn shortcut_label_is_number() {
            check_one(
                LinkTransform::Reference,
                &LinkReference::Shortcut,
                &LinkLabel::Text(Cow::Borrowed("5")),
                &Cow::Owned(LinkReference::Full("1".to_string())), // always count from 1
            )
        }

        /// The label isn't even close to a number.
        ///
        /// _c.f._ [shortcut_label_inlines_are_emphasized_number]
        #[test]
        fn shortcut_label_inlines_not_number_like() {
            check_one(
                LinkTransform::Reference,
                &LinkReference::Shortcut,
                &LinkLabel::Inline(&vec![Inline::Text(Text {
                    variant: TextVariant::Plain,
                    value: "hello world".to_string(),
                })]),
                &Cow::Owned(LinkReference::Full("hello world".to_string())),
            )
        }

        /// The label is kind of like a number, except that it's emphasized: `_123_`. This makes it not a number.
        ///
        /// _c.f._ [shortcut_label_inlines_not_number_like]
        #[test]
        fn shortcut_label_inlines_are_emphasized_number() {
            check_one(
                LinkTransform::Reference,
                &LinkReference::Shortcut,
                &LinkLabel::Inline(&vec![Inline::Formatting(Formatting {
                    variant: FormattingVariant::Emphasis,
                    children: vec![Inline::Text(Text {
                        variant: TextVariant::Plain,
                        value: "123".to_string(),
                    })],
                })]),
                &Cow::Owned(LinkReference::Full("_123_".to_string())), // note: the emphasis makes it not a number!
            )
        }

        #[test]
        fn shortcut_label_inlines_are_number() {
            check_one(
                LinkTransform::Reference,
                &LinkReference::Shortcut,
                &LinkLabel::Inline(&vec![Inline::Text(Text {
                    variant: TextVariant::Plain,
                    value: "123".to_string(),
                })]),
                &Cow::Owned(LinkReference::Full("1".to_string())),
            )
        }
    }

    /// A smoke test basically to ensure that we increment values correctly. We won't test every transformation type,
    /// since the sibling sub-modules already do that.
    #[test]
    fn smoke_test_multi() {
        let mut transformer = LinkTransformer::from(LinkTransform::Reference);

        // [alpha](https://example.com) ==> [alpha][1]
        assert_eq_cow(
            &transformer.transform(&LinkReference::Inline, &LinkLabel::Text(Cow::Borrowed("alpha"))),
            &Cow::Owned(LinkReference::Full("1".to_string())),
        );

        // [bravo][1] ==> [bravo][2]
        assert_eq_cow(
            &transformer.transform(
                &LinkReference::Full("1".to_string()),
                &LinkLabel::Text(Cow::Borrowed("bravo")),
            ),
            &Cow::Owned(LinkReference::Full("2".to_string())),
        );

        // [charlie][3] ==> [charlie][3]
        // Note that in this case, we could return a Borrowed cow, but we return a new Owned one anyway for simplicity
        assert_eq_cow(
            &transformer.transform(
                &LinkReference::Full("3".to_string()),
                &LinkLabel::Text(Cow::Borrowed("charlie")),
            ),
            &Cow::Owned(LinkReference::Full("3".to_string())),
        );

        // [delta][] ==> [delta][delta]
        // Note that in this case, we could return a Borrowed cow, but we return a new Owned one anyway for simplicity
        assert_eq_cow(
            &transformer.transform(&LinkReference::Collapsed, &LinkLabel::Text(Cow::Borrowed("delta"))),
            &Cow::Owned(LinkReference::Full("delta".to_string())),
        );
    }

    fn check_one(transform: LinkTransform, link_ref: &LinkReference, label: &LinkLabel, expected: &Cow<LinkReference>) {
        let mut transformer = LinkTransformer::from(transform);
        let actual = transformer.transform(&link_ref, &label);

        VARIANTS_CHECKER.see(&Combo::Of(transform, link_ref.clone()));

        assert_eq_cow(&actual, &expected);
    }
}
