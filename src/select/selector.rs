use crate::query::ParseError;
use crate::select::Matcher;

#[derive(Eq, PartialEq, Debug, Copy, Clone)]
pub enum ListItemTask {
    Selected,
    Unselected,
    Either,
    None,
}

#[derive(Eq, PartialEq, Debug)]
pub struct ListItemMatcher {
    pub ordered: bool,
    pub task: ListItemTask,
    pub matcher: Matcher,
}

#[derive(Eq, PartialEq, Debug)]
pub struct SectionMatcher {
    pub title: Matcher,
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub struct LinklikeMatcher {
    pub display_matcher: Matcher,
    pub url_matcher: Matcher,
}

#[derive(Eq, PartialEq, Debug)]
pub struct BlockQuoteMatcher {
    pub text: Matcher,
}

#[derive(Eq, PartialEq, Debug)]
pub struct HtmlMatcher {
    pub html: Matcher,
}

#[derive(Eq, PartialEq, Debug)]
pub struct ParagraphMatcher {
    pub text: Matcher,
}

#[derive(Eq, PartialEq, Debug)]
pub struct CodeBlockMatcher {
    pub language: Matcher,
    pub contents: Matcher,
}

#[derive(Eq, PartialEq, Debug)]
pub struct TableMatcher {
    pub column: Matcher,
    pub row: Matcher,
}

#[derive(Eq, PartialEq, Debug)]
pub enum Selector {
    Chain(Vec<Self>),
    Section(SectionMatcher),
    ListItem(ListItemMatcher),
    Link(LinklikeMatcher),
    Image(LinklikeMatcher),
    BlockQuote(BlockQuoteMatcher),
    CodeBlock(CodeBlockMatcher),
    Html(HtmlMatcher),
    Paragraph(ParagraphMatcher),
    Table(TableMatcher),
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
