use crate::matcher::StringMatcher;
use crate::select::match_selector::MatchSelector;
use crate::tree::Section;

#[derive(Debug, PartialEq)]
pub struct SectionSelector {
    matcher: StringMatcher,
}

impl MatchSelector<&Section> for SectionSelector {
    fn matches(&self, section: &Section) -> bool {
        self.matcher.matches_inlines(&section.title)
    }
}
