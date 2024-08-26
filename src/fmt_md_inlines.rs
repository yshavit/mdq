use crate::footnote_transform::FootnoteTransformer;
use crate::link_transform::{LinkLabel, LinkTransform, LinkTransformation, LinkTransformer};
use crate::output::{Output, SimpleWrite};
use crate::tree::{
    Footnote, Formatting, FormattingVariant, Image, Inline, Link, LinkDefinition, LinkReference, MdElem, Text,
    TextVariant,
};
use serde::Serialize;
use std::borrow::Cow;
use std::cmp::max;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Copy, Clone)]
pub struct MdInlinesWriterOptions {
    pub link_format: LinkTransform,
    pub renumber_footnotes: bool,
}

pub struct MdInlinesWriter<'md> {
    seen_links: HashSet<LinkLabel<'md>>,
    seen_footnotes: HashSet<&'md String>,
    pending_references: PendingReferences<'md>,
    link_transformer: LinkTransformer,
    footnote_transformer: FootnoteTransformer<'md>,
}

struct PendingReferences<'md> {
    pub links: HashMap<LinkLabel<'md>, UrlAndTitle<'md>>,
    pub footnotes: HashMap<&'md String, &'md Vec<MdElem>>,
}

impl<'md> PendingReferences<'md> {
    fn with_capacity(capacity: usize) -> Self {
        Self {
            links: HashMap::with_capacity(capacity),
            footnotes: HashMap::with_capacity(capacity),
        }
    }
}

#[derive(Serialize, Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub struct UrlAndTitle<'md> {
    pub url: &'md String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub title: &'md Option<String>,
}

#[derive(Debug, Copy, Clone)]
pub enum LinkLikeType {
    Link,
    Image,
}

pub trait LinkLike<'md> {
    fn link_info(&self) -> (LinkLikeType, LinkLabel<'md>, &'md LinkDefinition);
}

impl<'md> LinkLike<'md> for &'md Link {
    fn link_info(&self) -> (LinkLikeType, LinkLabel<'md>, &'md LinkDefinition) {
        (LinkLikeType::Link, LinkLabel::Inline(&self.text), &self.link_definition)
    }
}

impl<'md> LinkLike<'md> for &'md Image {
    fn link_info(&self) -> (LinkLikeType, LinkLabel<'md>, &'md LinkDefinition) {
        (
            LinkLikeType::Image,
            LinkLabel::Text(Cow::Borrowed(&self.alt)),
            &self.link,
        )
    }
}

impl<'md> MdInlinesWriter<'md> {
    pub fn new(options: MdInlinesWriterOptions) -> Self {
        let pending_refs_capacity = 8; // arbitrary guess
        Self {
            seen_links: HashSet::with_capacity(pending_refs_capacity),
            seen_footnotes: HashSet::with_capacity(pending_refs_capacity),
            pending_references: PendingReferences::with_capacity(pending_refs_capacity),
            link_transformer: LinkTransformer::from(options.link_format),
            footnote_transformer: FootnoteTransformer::new(options.renumber_footnotes),
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

    pub fn drain_pending_links(&mut self) -> Vec<(LinkLabel<'md>, UrlAndTitle<'md>)> {
        self.pending_references.links.drain().collect()
    }

    pub fn drain_pending_footnotes(&mut self) -> Vec<(String, &'md Vec<MdElem>)> {
        let mut result = Vec::with_capacity(self.pending_references.footnotes.len());
        let mut to_stringer = self.footnote_transformer.new_to_stringer();

        for (k, v) in self.pending_references.footnotes.drain() {
            let transformed_k = to_stringer.transform(k);
            result.push((transformed_k, v))
        }
        result
    }

    pub fn write_line<I, W>(&mut self, out: &mut Output<W>, elems: I)
    where
        I: IntoIterator<Item = &'md Inline>,
        W: SimpleWrite,
    {
        for elem in elems {
            self.write_inline_element(out, elem);
        }
    }

    pub fn write_inline_element<W>(&mut self, out: &mut Output<W>, elem: &'md Inline)
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
                let (surround_ch, surround_space) = match variant {
                    TextVariant::Plain => (Cow::Borrowed(""), false),
                    TextVariant::Math => (Cow::Borrowed("$"), false),
                    TextVariant::Html => (Cow::Borrowed(""), false),
                    TextVariant::Code => {
                        let backticks_info = BackticksInfo::from(value);
                        let surround_ch = if backticks_info.count == 0 {
                            Cow::Borrowed("`")
                        } else {
                            Cow::Owned("`".repeat(backticks_info.count + 1))
                        };
                        (surround_ch, backticks_info.at_either_end)
                    }
                };
                out.write_str(&surround_ch);
                if surround_space {
                    out.write_char(' ');
                }
                out.write_str(value);
                if surround_space {
                    out.write_char(' ');
                }
                out.write_str(&surround_ch);
            }
            Inline::Link(link) => self.write_linklike(out, link),
            Inline::Image(image) => self.write_linklike(out, image),
            Inline::Footnote(Footnote { label, text }) => {
                out.write_str("[^");
                self.footnote_transformer.write(out, label);
                out.write_char(']');
                self.add_footnote(label, text);
            }
        }
    }

    fn add_footnote(&mut self, label: &'md String, text: &'md Vec<MdElem>) {
        if self.seen_footnotes.insert(label) {
            self.pending_references.footnotes.insert(label, text);
        }
        self.find_references_in_footnote_elems(text);
    }

    /// Searches the footnote's text to find any link references and additional footnotes.
    /// Otherwise, by the time we see them it'll be too late to add them to their respective
    /// collections.
    fn find_references_in_footnote_elems(&mut self, text: &'md Vec<MdElem>) {
        for elem in text {
            match elem {
                MdElem::BlockQuote(block) => {
                    self.find_references_in_footnote_elems(&block.body);
                }
                MdElem::List(list) => {
                    for li in &list.items {
                        self.find_references_in_footnote_elems(&li.item);
                    }
                }
                MdElem::Section(section) => {
                    self.find_references_in_footnote_inlines(&section.title);
                    self.find_references_in_footnote_elems(&section.body);
                }
                MdElem::Paragraph(para) => {
                    self.find_references_in_footnote_inlines(&para.body);
                }
                MdElem::Table(table) => {
                    for row in &table.rows {
                        for cell in row {
                            self.find_references_in_footnote_inlines(cell);
                        }
                    }
                }
                MdElem::Inline(inline) => {
                    self.find_references_in_footnote_inlines([inline]); // TODO do I need the array?
                }
                MdElem::CodeBlock(_) | MdElem::Html(_) | MdElem::ThematicBreak => {
                    // nothing
                }
            }
        }
    }

    fn find_references_in_footnote_inlines<I>(&mut self, text: I)
    where
        I: IntoIterator<Item = &'md Inline>,
    {
        for inline in text.into_iter() {
            match inline {
                Inline::Footnote(footnote) => {
                    self.add_footnote(&footnote.label, &footnote.text);
                }
                Inline::Formatting(item) => {
                    self.find_references_in_footnote_inlines(&item.children);
                }
                Inline::Link(link) => {
                    let link_label = match &link.link_definition.reference {
                        LinkReference::Inline => None,
                        LinkReference::Full(reference) => Some(LinkLabel::Text(Cow::Borrowed(reference))),
                        LinkReference::Collapsed | LinkReference::Shortcut => Some(LinkLabel::Inline(&link.text)),
                    };
                    if let Some(label) = link_label {
                        self.add_link_reference(label, &link.link_definition);
                    }
                }
                Inline::Image(_) | Inline::Text(_) => {
                    // nothing
                }
            }
        }
    }

    /// Writes the inline portion of the link, which may be the full link if it was originally inlined.
    ///
    /// Examples:
    ///
    /// ```md
    /// [an inline link](https://example.com)
    /// [a referenced link][1]
    /// ```
    ///
    /// The `contents` function is what writes e.g. `an inline link` above. It's a function because it may be a recursive
    /// call into [write_line] (for links) or just simple text (for image alts).
    pub fn write_linklike<W, L>(&mut self, out: &mut Output<W>, link_like: L)
    where
        W: SimpleWrite,
        L: LinkLike<'md> + Copy,
    {
        let (link_type, label, link) = link_like.link_info();
        if matches!(link_type, LinkLikeType::Image) {
            out.write_char('!');
        }
        out.write_char('[');

        match &label {
            LinkLabel::Text(text) => self.write_link_descriptions(out, text),
            LinkLabel::Inline(text) => {
                // Write to a string, and then dump that to out. This lets us escaping, and will
                // eventually let us handle matched square brackets.
                // Note that it's not really worth it to do the transformation "on the fly":
                // the SimpleWrite trait only writes strings anyway (not chars), so even if we
                // had an intercepting transform, it would still have to work on allocated strings.
                // So we may as well just do it once.
                // This could be output a bit nicer: see #183.
                let mut sub_out = Output::new(String::with_capacity(64));
                self.write_line(&mut sub_out, *text);
                let as_string = sub_out.take_underlying().unwrap();
                self.write_link_descriptions(out, &as_string);
            }
        }

        out.write_char(']');

        let link_ref = LinkTransformation::new(self.link_transformer.transform_variant(), self, link_like)
            .apply(&mut self.link_transformer, &link.reference);
        let reference_to_add = match link_ref {
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
            self.add_link_reference(reference_label, link);
        }
    }

    fn add_link_reference(&mut self, reference_label: LinkLabel<'md>, link: &'md LinkDefinition) {
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

    fn write_link_descriptions<W>(&mut self, out: &mut Output<W>, description: &str)
    where
        W: SimpleWrite,
    {
        description.chars().for_each(|ch| {
            if ch == '[' || ch == ']' {
                out.write_char('\\');
            }
            out.write_char(ch);
        });
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

struct BackticksInfo {
    count: usize,
    at_either_end: bool,
}

impl From<&String> for BackticksInfo {
    fn from(s: &String) -> Self {
        let mut overall_max = 0;
        let mut current_stretch = 0;
        for c in s.chars() {
            match c {
                '`' => current_stretch += 1,
                _ => {
                    if current_stretch > 0 {
                        overall_max = max(current_stretch, overall_max);
                        current_stretch = 0;
                    }
                }
            }
        }
        let count = max(current_stretch, overall_max);
        let at_either_end = s.starts_with('`') || s.ends_with('`');
        Self { count, at_either_end }
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
    use crate::tree::ReadOptions;
    use crate::unwrap;
    use crate::utils_for_test::get_only;

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

    mod inline_code {
        use super::*;

        #[test]
        fn round_trip_no_backticks() {
            round_trip_inline("hello world");
        }

        #[test]
        fn round_trip_one_backtick() {
            round_trip_inline("hello ` world");
        }

        #[test]
        fn round_trip_one_backtick_at_start() {
            round_trip_inline("`hello");
        }

        #[test]
        fn round_trip_one_backtick_at_end() {
            round_trip_inline("hello `");
        }

        #[test]
        fn round_trip_three_backticks() {
            round_trip_inline("hello ``` world");
        }

        #[test]
        fn round_trip_three_backticks_at_end() {
            round_trip_inline("hello `");
        }

        #[test]
        fn round_trip_three_backticks_at_start() {
            round_trip_inline("`hello");
        }

        #[test]
        fn round_trip_surrounding_whitespace() {
            round_trip_inline_to(" hello ", "hello");
        }

        #[test]
        fn round_trip_backtick_and_surrounding_whitespace() {
            round_trip_inline_to(" hello`world ", "hello`world");
        }

        fn round_trip_inline_to(orig: &str, expect: &str) {
            round_trip(
                &Inline::Text(Text {
                    variant: TextVariant::Code,
                    value: orig.to_string(),
                }),
                &Inline::Text(Text {
                    variant: TextVariant::Code,
                    value: expect.to_string(),
                }),
            );
        }

        fn round_trip_inline(inline_str: &str) {
            round_trip_inline_to(inline_str, inline_str);
        }
    }

    mod link_description {
        use super::*;

        #[test]
        fn simple() {
            check_link_description("hello, world", "hello, world");
        }

        #[test]
        fn matched_brackets() {
            check_link_description("link [foo [bar]]", "link \\[foo \\[bar\\]\\]");
        }

        #[test]
        fn unmatched_brackets() {
            check_link_description("link [foo bar", "link \\[foo bar");
        }

        fn check_link_description(input_description: &str, expected: &str) {
            let mut output = Output::new(String::new());
            let mut writer = MdInlinesWriter::new(MdInlinesWriterOptions {
                link_format: LinkTransform::Keep,
                renumber_footnotes: false,
            });
            let link = Inline::Link(Link {
                text: vec![Inline::Text(Text {
                    variant: TextVariant::Plain,
                    value: input_description.to_string(),
                })],
                link_definition: LinkDefinition {
                    url: "https://www.example.com".to_string(),
                    title: None,
                    reference: LinkReference::Inline,
                },
            });
            writer.write_inline_element(&mut output, &link);

            assert_eq!(
                output.take_underlying().unwrap(),
                format!("[{expected}](https://www.example.com)")
            );
        }
    }

    mod img_alt {
        use super::*;

        #[test]
        fn simple() {
            check_img_alt("hello, world", "hello, world");
        }

        #[test]
        fn matched_brackets() {
            check_img_alt("link [foo [bar]]", "link \\[foo \\[bar\\]\\]");
        }

        #[test]
        fn unmatched_brackets() {
            check_img_alt("link [foo bar", "link \\[foo bar");
        }

        fn check_img_alt(input_description: &str, expected: &str) {
            let mut output = Output::new(String::new());
            let mut writer = MdInlinesWriter::new(MdInlinesWriterOptions {
                link_format: LinkTransform::Keep,
                renumber_footnotes: false,
            });
            let link = Inline::Image(Image {
                alt: input_description.to_string(),
                link: LinkDefinition {
                    url: "https://www.example.com".to_string(),
                    title: None,
                    reference: LinkReference::Inline,
                },
            });
            writer.write_inline_element(&mut output, &link);

            assert_eq!(
                output.take_underlying().unwrap(),
                format!("![{expected}](https://www.example.com)")
            );
        }
    }

    /// Not a pure unit test; semi-integ. Checks that writing an inline to markdown and then parsing
    /// that markdown results in the original inline.
    fn round_trip(orig: &Inline, expect: &Inline) {
        let mut output = Output::new(String::new());
        let mut writer = MdInlinesWriter::new(MdInlinesWriterOptions {
            link_format: LinkTransform::Keep,
            renumber_footnotes: false,
        });
        writer.write_inline_element(&mut output, &orig);
        let md_str = output.take_underlying().unwrap();

        let ast = markdown::to_mdast(&md_str, &markdown::ParseOptions::gfm()).unwrap();
        let md_tree = MdElem::read(ast, &ReadOptions::default()).unwrap();

        unwrap!(&md_tree[0], MdElem::Paragraph(p));
        let parsed = get_only(&p.body);
        assert_eq!(parsed, expect);
    }
}
