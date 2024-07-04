use crate::link_transform::{LinkLabel, LinkTransform, LinkTransformer, SingleTransformationState};
use crate::output::{Output, SimpleWrite};
use crate::tree::{
    Footnote, Formatting, FormattingVariant, Image, Inline, Link, LinkDefinition, LinkReference, MdElem, Text,
    TextVariant,
};
use std::borrow::{Borrow, Cow};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Copy, Clone)]
pub struct MdInlinesWriterOptions {
    pub link_canonicalization: LinkTransform,
}

pub struct MdInlinesWriter<'a> {
    seen_links: HashSet<LinkLabel<'a>>,
    seen_footnotes: HashSet<&'a String>,
    pending_references: PendingReferences<'a>,
    link_transformer: LinkTransformer,
}

struct PendingReferences<'a> {
    pub links: HashMap<LinkLabel<'a>, UrlAndTitle<'a>>,
    pub footnotes: HashMap<&'a String, &'a Vec<MdElem>>,
}

impl<'a> PendingReferences<'a> {
    fn with_capacity(capacity: usize) -> Self {
        Self {
            links: HashMap::with_capacity(capacity),
            footnotes: HashMap::with_capacity(capacity),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub struct UrlAndTitle<'a> {
    pub url: &'a String,
    pub title: &'a Option<String>,
}

#[derive(Debug, Copy, Clone)]
pub enum LinkLikeType {
    Link,
    Image,
}

pub trait LinkLike<'a> {
    fn link_info(&self) -> (LinkLikeType, LinkLabel<'a>, &'a LinkDefinition);
}

impl<'a> LinkLike<'a> for &'a Link {
    fn link_info(&self) -> (LinkLikeType, LinkLabel<'a>, &'a LinkDefinition) {
        (LinkLikeType::Link, LinkLabel::Inline(&self.text), &self.link_definition)
    }
}

impl<'a> LinkLike<'a> for &'a Image {
    fn link_info(&self) -> (LinkLikeType, LinkLabel<'a>, &'a LinkDefinition) {
        (
            LinkLikeType::Image,
            LinkLabel::Text(Cow::Borrowed(&self.alt)),
            &self.link,
        )
    }
}

impl<'a> MdInlinesWriter<'a> {
    pub fn new(options: MdInlinesWriterOptions) -> Self {
        let pending_refs_capacity = 8; // arbitrary guess
        Self {
            seen_links: HashSet::with_capacity(pending_refs_capacity),
            seen_footnotes: HashSet::with_capacity(pending_refs_capacity),
            pending_references: PendingReferences::with_capacity(pending_refs_capacity),
            link_transformer: LinkTransformer::from(options.link_canonicalization),
        }
    }

    pub fn has_pending_links(&self) -> bool {
        !self.pending_references.links.is_empty()
    }

    pub fn has_pending_footnotes(&self) -> bool {
        !self.pending_references.footnotes.is_empty()
    }

    pub fn count_pending_links(&self) -> usize {
        self.pending_references.links.len()
    }

    pub fn count_pending_footnotes(&self) -> usize {
        self.pending_references.footnotes.len()
    }

    pub fn drain_pending_links(&mut self) -> Vec<(LinkLabel<'a>, UrlAndTitle<'a>)> {
        self.pending_references.links.drain().collect()
    }

    pub fn drain_pending_footnotes(&mut self) -> Vec<(&'a String, &'a Vec<MdElem>)> {
        self.pending_references.footnotes.drain().collect()
    }

    pub fn write_line<E, W>(&mut self, out: &mut Output<W>, elems: &'a [E])
    where
        E: Borrow<Inline>,
        W: SimpleWrite,
    {
        for elem in elems {
            self.write_inline_element(out, elem.borrow());
        }
    }

    pub fn write_inline_element<W>(&mut self, out: &mut Output<W>, elem: &'a Inline)
    where
        W: SimpleWrite,
    {
        match elem {
            Inline::Formatting(Formatting { variant, children }) => {
                let surround = match variant {
                    FormattingVariant::Delete => "~~",
                    FormattingVariant::Emphasis => "_",
                    FormattingVariant::Strong => "**",
                };
                out.write_str(surround);
                self.write_line(out, children);
                out.write_str(surround);
            }
            Inline::Text(Text { variant, value }) => {
                let surround = match variant {
                    TextVariant::Plain => "",
                    TextVariant::Code => "`",
                    TextVariant::Math => "$",
                    TextVariant::Html => "",
                };
                out.write_str(surround);
                out.write_str(value);
                out.write_str(surround);
            }
            Inline::Link(link) => self.write_linklike(out, link),
            Inline::Image(image) => self.write_linklike(out, image),
            Inline::Footnote(Footnote { label, text }) => {
                out.write_str("[^");
                out.write_str(label);
                out.write_char(']');
                if self.seen_footnotes.insert(label) {
                    self.pending_references.footnotes.insert(label, text);
                }
            }
        }
    }

    /// Writes the inline portion of the link, which may be the full link if it was originally inlined.
    ///
    /// Examples:
    ///
    /// ```
    /// [an inline link](https://example.com)
    /// [a referenced link][1]
    /// ```
    ///
    /// The `contents` function is what writes e.g. `an inline link` above. It's a function because it may be a recursive
    /// call into [write_line] (for links) or just simple text (for image alts).
    pub fn write_linklike<W, L>(&mut self, out: &mut Output<W>, link_like: L)
    where
        W: SimpleWrite,
        L: LinkLike<'a> + Copy,
    {
        let (link_type, label, link) = link_like.link_info();
        if matches!(link_type, LinkLikeType::Image) {
            out.write_char('!');
        }
        out.write_char('[');
        match &label {
            LinkLabel::Text(text) => out.write_str(text),
            LinkLabel::Inline(text) => self.write_line(out, text),
        }
        out.write_char(']');

        let state = SingleTransformationState::new(self.link_transformer.transform_variant(), self, link_like);
        let link_ref_owned = state.apply(&mut self.link_transformer, &link.reference);

        let reference_to_add = match link_ref_owned {
            LinkReference::Inline => {
                out.write_char('(');
                out.write_str(&link.url);
                self.write_url_title(out, &link.title);
                out.write_char(')');
                None
            }
            LinkReference::Full(identifier) => {
                out.write_char('[');
                out.write_str(&identifier);
                out.write_char(']');
                Some(LinkLabel::Text(Cow::from(identifier)))
            }
            LinkReference::Collapsed => {
                out.write_str("[]");
                Some(label)
            }
            LinkReference::Shortcut => Some(label),
        };

        if let Some(reference_label) = reference_to_add {
            if self.seen_links.insert(reference_label.clone()) {
                self.pending_references.links.insert(
                    reference_label,
                    UrlAndTitle {
                        url: &link.url,
                        title: &link.title,
                    },
                );
                // else warn?
            }
        }
    }

    pub fn write_url_title<W>(&mut self, out: &mut Output<W>, title: &Option<String>)
    where
        W: SimpleWrite,
    {
        let Some(title) = title else { return };
        out.write_char(' ');
        TitleQuote::find_best_strategy(title).escape_to(title, out);
    }
}

enum TitleQuote {
    Double,
    Single,
    Paren,
}

impl TitleQuote {
    pub fn find_best_strategy(text: &str) -> Self {
        [Self::Double, Self::Single, Self::Paren]
            .into_iter()
            .find(|strategy| !strategy.has_conflicts(text))
            .unwrap_or(TitleQuote::Double)
    }

    fn get_surround_chars(&self) -> (char, char) {
        match self {
            TitleQuote::Double => ('"', '"'),
            TitleQuote::Single => ('\'', '\''),
            TitleQuote::Paren => ('(', ')'),
        }
    }

    fn get_conflict_char_fn(surrounds: (char, char)) -> impl Fn(char) -> bool {
        let (open, close) = surrounds;
        move |ch| ch == open || ch == close
    }

    fn has_conflicts(&self, text: &str) -> bool {
        text.chars().any(Self::get_conflict_char_fn(self.get_surround_chars()))
    }

    fn escape_to<W: SimpleWrite>(&self, text: &str, out: &mut Output<W>) {
        let surrounds = self.get_surround_chars();
        let conflict_char_fn = Self::get_conflict_char_fn(surrounds);
        let (open, close) = surrounds;

        out.write_char(open);
        for ch in text.chars() {
            if conflict_char_fn(ch) {
                out.write_char('\\');
            }
            out.write_char(ch);
        }
        out.write_char(close);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::output::Output;

    mod title_quoting {
        use super::*;

        crate::variants_checker!(TITLE_QUOTING_CHECKER = TitleQuote { Double, Single, Paren });

        #[test]
        fn bareword_uses_double() {
            check("foo", "\"foo\"");
        }

        #[test]
        fn has_double_quotes() {
            check("foo\"bar", "'foo\"bar'");
        }

        #[test]
        fn has_double_quotes_and_singles() {
            check("foo'\"bar", "(foo'\"bar)");
        }

        #[test]
        fn has_only_single_quotes() {
            check("foo'bar", "\"foo'bar\"");
        }

        #[test]
        fn has_all_delimiters() {
            check("foo('\")bar", "\"foo('\\\")bar\"");
        }

        fn check(input: &str, expected: &str) {
            let strategy = TitleQuote::find_best_strategy(input);
            TITLE_QUOTING_CHECKER.see(&strategy);

            // +1 to give room for some quotes
            let mut writer = Output::new(String::with_capacity(input.len() + 4));
            strategy.escape_to(input, &mut writer);
            let actual = writer.take_underlying().unwrap();
            assert_eq!(&actual, expected);
        }
    }
}
