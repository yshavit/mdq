use crate::md_elem::elem::*;
use crate::select::match_selector::MatchSelector;
use crate::select::string_matcher::StringMatcher;
use crate::select::SectionMatcher;

#[derive(Debug, PartialEq)]
pub struct SectionSelector {
    matcher: StringMatcher,
}

impl From<SectionMatcher> for SectionSelector {
    fn from(value: SectionMatcher) -> Self {
        Self {
            matcher: value.title.into(),
        }
    }
}

impl MatchSelector<Section> for SectionSelector {
    fn matches(&self, section: &Section) -> bool {
        self.matcher.matches_inlines(&section.title)
    }
}
