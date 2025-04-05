use crate::query::matcher::Matcher;

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

#[derive(Eq, PartialEq, Debug, Clone)]
pub struct LinklikeMatcher {
    pub display_matcher: Matcher,
    pub url_matcher: Matcher,
}

#[derive(Eq, PartialEq, Debug)]
pub struct CodeBlockMatcher {
    pub language: Matcher,
    pub contents: Matcher,
}

#[derive(Eq, PartialEq, Debug)]
pub struct TableSliceMatcher {
    pub column: Matcher,
    pub row: Matcher,
}

#[derive(Eq, PartialEq, Debug)]
pub struct SelectorChain {
    pub selectors: Vec<Selector>,
}

#[derive(Eq, PartialEq, Debug)]
pub enum Selector {
    Section(Matcher),
    ListItem(ListItemMatcher),
    Link(LinklikeMatcher),
    Image(LinklikeMatcher),
    BlockQuote(Matcher),
    CodeBlock(CodeBlockMatcher),
    Html(Matcher),
    Paragraph(Matcher),
    TableSlice(TableSliceMatcher),
}
