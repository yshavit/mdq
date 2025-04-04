use crate::query::{Pairs, Query};
use crate::select::matchers::{CodeBlockMatcher, LinklikeMatcher, ListItemMatcher, Matcher, TableSliceMatcher};
use crate::select::{ParseError, SelectorAdapter};
use crate::tree::MdContext;
use crate::tree_ref::MdElemRef;

#[derive(Eq, PartialEq, Debug)]
pub struct SelectorChain {
    pub selectors: Vec<Selector>,
}

#[derive(Clone, Eq, PartialEq, Debug)]
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

impl Selector {
    pub fn parse(text: &str) -> Result<Vec<Self>, ParseError> {
        // TODo most of this impl should be in query
        let parsed: Pairs = Query::parse(text).map_err(|err| ParseError::from(err))?;
        let parsed_selectors = SelectorChain::try_from(parsed).map_err(|e| ParseError::from(e))?;
        Ok(parsed_selectors.selectors.into_iter().map(|s| Self::from(s)).collect())
    }

    pub fn find_nodes<'md>(&self, ctx: &'md MdContext, nodes: Vec<MdElemRef<'md>>) -> Vec<MdElemRef<'md>> {
        // TODo most of this impl should be in adapter, maybe?
        let mut result = Vec::with_capacity(8); // arbitrary guess
        let mut search_context = crate::select::adapter::SearchContext::new(ctx);
        for node in nodes {
            SelectorAdapter::build_output(self, &mut result, &mut search_context, node);
        }
        result
    }
}
