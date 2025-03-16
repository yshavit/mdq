use crate::matcher::StringMatcher;
use crate::select::match_selector::MatchSelector;
use crate::tree::BlockQuote;

#[derive(Debug, PartialEq)]
pub struct BlockQuoteSelector {
    matcher: StringMatcher,
}

impl MatchSelector<&BlockQuote> for BlockQuoteSelector {
    fn matches(&self, block_quote: &BlockQuote) -> bool {
        self.matcher.matches_any(&block_quote.body)
    }
}
