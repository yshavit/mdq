use crate::md_elem::elem::FrontMatterVariant;
use crate::md_elem::{MdContext, MdDoc, MdElem};
use crate::query::ParseError;
use crate::select::{MatchReplace, Result, SelectorAdapter};

/// The completion state that a [`ListItemMatcher`] looks for.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ListItemTask {
    /// `- [x] foo`
    Selected,
    /// `- [ ] foo`
    Unselected,
    /// `- [?] foo`
    Either,
    /// `- foo`
    None,
}

/// matcher for [`Selector::ListItem`]
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ListItemMatcher {
    /// Whether this matches an ordered list (`1. foo`) or an unordered one (`- foo`).
    pub ordered: bool,
    /// Whether this matches a task list (`- [ ] foo`), and if so, what completion state matches.
    ///
    /// Tasks are typically unordered, but may also be ordered (`1. [ ] foo`).
    pub task: ListItemTask,
    pub matcher: MatchReplace,
}

/// matcher for [`Selector::Section`]
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SectionMatcher {
    pub title: MatchReplace,
}

/// matcher for both [`Selector::Link`] and [`Selector::Image`]
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct LinklikeMatcher {
    pub display_matcher: MatchReplace,
    pub url_matcher: MatchReplace,
}

/// matcher for [`Selector::BlockQuote`]
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct BlockQuoteMatcher {
    pub text: MatchReplace,
}

/// matcher for [`Selector::Html`]
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct HtmlMatcher {
    pub html: MatchReplace,
}

/// matcher for [`Selector::Paragraph`]
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct ParagraphMatcher {
    pub text: MatchReplace,
}

/// matcher for [`Selector::CodeBlock`]
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CodeBlockMatcher {
    pub language: MatchReplace,
    pub contents: MatchReplace,
}

/// matcher for [`Selector::FrontMatter`]
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FrontMatterMatcher {
    pub variant: Option<FrontMatterVariant>,
    pub text: MatchReplace,
}

/// matcher for [`Selector::Table`]
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct TableMatcher {
    pub headers: MatchReplace,
    pub rows: MatchReplace,
}

/// The in-memory equivalent of mdq's selector query string.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[non_exhaustive]
pub enum Selector {
    /// `foo | bar`
    Chain(Vec<Self>),
    /// `# section title`
    Section(SectionMatcher),
    /// `1. ordered` or `- unordered` lists, or `- [ ] tasks`
    ListItem(ListItemMatcher),
    /// `[some](https://example.com/url)`
    Link(LinklikeMatcher),
    /// `![alt](https://example.com/image.png)`
    Image(LinklikeMatcher),
    /// `> block quote`
    BlockQuote(BlockQuoteMatcher),
    /// ` ```language contents `
    CodeBlock(CodeBlockMatcher),
    /// `+++ front matter`
    FrontMatter(FrontMatterMatcher),
    /// `</> html-tags`
    Html(HtmlMatcher),
    /// `P: paragraph text`
    Paragraph(ParagraphMatcher),
    /// `:-: headers :-: rows`
    Table(TableMatcher),
}

impl Selector {
    /// Filter (and possibly manipulate) [`MdElem`]s in the provided [`MdDoc`] according to this selector.
    ///
    /// For each element of the `nodes` argument, if that element matches this selector, it will be returned in the
    /// result. Otherwise, this method will recurse into that node's children and match against them, and so on. This
    /// also means that each element may turn into multiple elements in the result, if multiple of its children match.
    /// If an element _and_ its children (or other descendants) match, the result will only include that parent.
    ///
    /// This may return an empty `Vec`. That's not an error per se; it just means that none of the elements matched.
    ///
    /// The result also includes an [`MdContext`] that you can use with [`MdWriter`](crate::output::MdWriter).
    pub fn find_nodes(self, doc: MdDoc) -> Result<(Vec<MdElem>, MdContext)> {
        let MdDoc { ctx, roots } = doc;
        let result_elems = SelectorAdapter::from(self).find_nodes(&ctx, vec![MdElem::Doc(roots)])?;
        Ok((result_elems, ctx))
    }
}

impl TryFrom<&'_ str> for Selector {
    type Error = ParseError;

    fn try_from(value: &'_ str) -> std::result::Result<Self, Self::Error> {
        Selector::try_parse(value).map_err(ParseError::new)
    }
}

impl TryFrom<&'_ String> for Selector {
    type Error = ParseError;

    fn try_from(value: &'_ String) -> std::result::Result<Self, Self::Error> {
        Selector::try_from(value.as_str())
    }
}
