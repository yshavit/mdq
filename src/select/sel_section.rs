use crate::matcher::StringMatcher;
use crate::md_elem::elem::*;
use crate::select::match_selector::MatchSelector;
use crate::select::Matcher;

#[derive(Debug, PartialEq)]
pub struct SectionSelector {
    matcher: StringMatcher,
}

impl From<Matcher> for SectionSelector {
    fn from(value: Matcher) -> Self {
        Self { matcher: value.into() }
    }
}

impl MatchSelector<&Section> for SectionSelector {
    fn matches(&self, section: &Section) -> bool {
        self.matcher.matches_inlines(&section.title)
    }
}
