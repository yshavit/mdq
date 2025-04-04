use crate::select::matchers::{CodeBlockMatcher, LinklikeMatcher, ListItemMatcher, Matcher, TableSliceMatcher};

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
