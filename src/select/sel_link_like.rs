use crate::matcher::StringMatcher;
use crate::query::selectors::LinklikeMatcher;
use crate::select::match_selector::MatchSelector;
use crate::tree::{Image, Link};

#[derive(Debug, PartialEq)]
pub struct LinkSelector {
    matchers: LinkMatchers,
}

impl From<LinklikeMatcher> for LinkSelector {
    fn from(value: LinklikeMatcher) -> Self {
        todo!()
    }
}

impl MatchSelector<&Link> for LinkSelector {
    fn matches(&self, item: &Link) -> bool {
        self.matchers.display_matcher.matches_inlines(&item.text)
            && self.matchers.url_matcher.matches(&item.link_definition.url)
    }
}

#[derive(Debug, PartialEq)]
pub struct ImageSelector {
    matchers: LinkMatchers,
}

impl From<LinklikeMatcher> for ImageSelector {
    fn from(value: LinklikeMatcher) -> Self {
        todo!()
    }
}

impl MatchSelector<&Image> for ImageSelector {
    fn matches(&self, item: &Image) -> bool {
        self.matchers.display_matcher.matches(&item.alt) && self.matchers.url_matcher.matches(&item.link.url)
    }
}

#[derive(Debug, PartialEq)]
pub struct LinkMatchers {
    pub display_matcher: StringMatcher,
    pub url_matcher: StringMatcher,
}
