use crate::matcher::StringMatcher;
use crate::parsing_iter::ParsingIterator;
use crate::select::base::Selector;
use crate::select::{ParseResult, SelectResult};
use crate::tree::Link;
use crate::tree_ref::MdElemRef;

#[derive(Debug, PartialEq)]
pub struct LinkSelector {
    display_matcher: StringMatcher,
    url_matcher: StringMatcher,
}

impl LinkSelector {
    pub fn read(iter: &mut ParsingIterator) -> ParseResult<Self> {
        let display_matcher = StringMatcher::read(iter, ']')?;
        iter.require_str("](")?;
        let url_matcher = StringMatcher::read(iter, ')')?;
        iter.require_char(')')?;
        Ok(Self {
            display_matcher,
            url_matcher,
        })
    }
}

impl<'a> Selector<'a, &'a Link> for LinkSelector {
    fn matches(&self, item: &'a Link) -> bool {
        self.display_matcher.matches_inlines(&item.text) && self.url_matcher.matches(&item.link_definition.url)
    }

    fn pick(&self, item: &'a Link) -> SelectResult<'a> {
        SelectResult::One(MdElemRef::Link(item))
    }
}
