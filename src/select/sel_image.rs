use crate::parsing_iter::ParsingIterator;
use crate::select::match_selector::MatchSelector;
use crate::select::sel_link::LinkMatchers;
use crate::select::ParseResult;
use crate::tree::Image;

#[derive(Debug, PartialEq)]
pub struct ImageSelector {
    matchers: LinkMatchers,
}

impl ImageSelector {
    pub fn read(iter: &mut ParsingIterator) -> ParseResult<Self> {
        let matchers = LinkMatchers::read(iter)?;
        Ok(Self { matchers })
    }
}

impl MatchSelector<&Image> for ImageSelector {
    fn matches(&self, item: &Image) -> bool {
        self.matchers.display_matcher.matches(&item.alt) && self.matchers.url_matcher.matches(&item.link.url)
    }
}
