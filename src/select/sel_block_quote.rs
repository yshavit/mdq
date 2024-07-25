use crate::matcher::StringMatcher;
use crate::parsing_iter::ParsingIterator;
use crate::select::{ParseResult, Selector, SELECTOR_SEPARATOR};
use crate::tree::BlockQuote;

#[derive(Debug, PartialEq)]
pub struct BlockQuoteSelector {
    matcher: StringMatcher,
}

impl BlockQuoteSelector {
    pub fn read(iter: &mut ParsingIterator) -> ParseResult<Self> {
        iter.require_whitespace_or(SELECTOR_SEPARATOR, ">")?;
        let matcher = StringMatcher::read(iter, SELECTOR_SEPARATOR)?;
        Ok(Self { matcher })
    }
}

impl<'a> Selector<'a, &'a BlockQuote> for BlockQuoteSelector {
    fn matches(&self, block_quote: &'a BlockQuote) -> bool {
        self.matcher.matches_any(&block_quote.body)
    }
}
