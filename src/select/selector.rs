use crate::md_elem::{MdContext, MdElem};
use crate::query::ParseError;
use crate::select::{Matcher, SelectorAdapter};

/// The completion state that a [`ListItemMatcher`] looks for.
#[derive(Eq, PartialEq, Debug, Copy, Clone)]
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
#[derive(Eq, PartialEq, Debug)]
pub struct ListItemMatcher {
    /// Whether this matches an ordered list (`1. foo`) or an unordered one (`- foo`).
    pub ordered: bool,
    /// Whether this matches a task list (`- [ ] foo`), and if so, what completion state matches.
    ///
    /// Tasks are typically unordered, but may also be ordered (`1. [ ] foo`).
    pub task: ListItemTask,
    pub matcher: Matcher,
}

/// matcher for [`Selector::Section`]
#[derive(Eq, PartialEq, Debug)]
pub struct SectionMatcher {
    pub title: Matcher,
}

/// matcher for both [`Selector::Link`] and [`Selector::Image`]
#[derive(Eq, PartialEq, Debug, Clone)]
pub struct LinklikeMatcher {
    pub display_matcher: Matcher,
    pub url_matcher: Matcher,
}

/// matcher for [`Selector::BlockQuote`]
#[derive(Eq, PartialEq, Debug)]
pub struct BlockQuoteMatcher {
    pub text: Matcher,
}

/// matcher for [`Selector::Html`]
#[derive(Eq, PartialEq, Debug)]
pub struct HtmlMatcher {
    pub html: Matcher,
}

/// matcher for [`Selector::Paragraph`]
#[derive(Eq, PartialEq, Debug)]
pub struct ParagraphMatcher {
    pub text: Matcher,
}

/// matcher for [`Selector::CodeBlock`]
#[derive(Eq, PartialEq, Debug)]
pub struct CodeBlockMatcher {
    pub language: Matcher,
    pub contents: Matcher,
}

/// matcher for [`Selector::Table`]
#[derive(Eq, PartialEq, Debug)]
pub struct TableMatcher {
    pub headers: Matcher,
    pub rows: Matcher,
}

/// The in-memory equivalent of mdq's selector query string.
#[derive(Eq, PartialEq, Debug)]
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
    /// `</> html-tags`
    Html(HtmlMatcher),
    /// `P: paragraph text`
    Paragraph(ParagraphMatcher),
    /// `:-: headers :-: rows`
    Table(TableMatcher),
}

impl Selector {
    /// Filter and manipulate the provided `MdElem`s according to this selector.
    pub fn find_nodes(self, ctx: &MdContext, nodes: Vec<MdElem>) -> Vec<MdElem> {
        SelectorAdapter::from(self).find_nodes(ctx, nodes)
    }
}

impl TryFrom<&'_ str> for Selector {
    type Error = ParseError;

    fn try_from(value: &'_ str) -> Result<Self, Self::Error> {
        Selector::try_parse(value)
    }
}

impl TryFrom<&'_ String> for Selector {
    type Error = ParseError;

    fn try_from(value: &'_ String) -> Result<Self, Self::Error> {
        Selector::try_parse(value)
    }
}
