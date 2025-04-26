use crate::md_elem::elem::*;
use crate::select::match_selector::MatchSelector;
use crate::select::string_matcher::StringMatcher;
use crate::select::LinklikeMatcher;

#[derive(Debug, PartialEq)]
pub struct LinkSelector {
    matchers: LinkMatchers,
}

impl From<LinklikeMatcher> for LinkSelector {
    fn from(value: LinklikeMatcher) -> Self {
        Self { matchers: value.into() }
    }
}

impl MatchSelector<Link> for LinkSelector {
    fn matches(&self, item: &Link) -> bool {
        self.matchers.display_matcher.matches_inlines(&item.display)
            && self.matchers.url_matcher.matches(&item.link.url)
    }
}

#[derive(Debug, PartialEq)]
pub struct ImageSelector {
    matchers: LinkMatchers,
}

impl From<LinklikeMatcher> for ImageSelector {
    fn from(value: LinklikeMatcher) -> Self {
        Self { matchers: value.into() }
    }
}

impl MatchSelector<Image> for ImageSelector {
    fn matches(&self, item: &Image) -> bool {
        self.matchers.display_matcher.matches(&item.alt) && self.matchers.url_matcher.matches(&item.link.url)
    }
}

#[derive(Debug, PartialEq)]
pub struct LinkMatchers {
    pub display_matcher: StringMatcher,
    pub url_matcher: StringMatcher,
}

impl From<LinklikeMatcher> for LinkMatchers {
    fn from(value: LinklikeMatcher) -> Self {
        Self {
            display_matcher: value.display_matcher.into(),
            url_matcher: value.url_matcher.into(),
        }
    }
}
