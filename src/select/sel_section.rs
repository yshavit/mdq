use crate::matcher::StringMatcher;
use crate::parsing_iter::ParsingIterator;
use crate::select::match_selector::MatchSelector;
use crate::select::{ParseResult, SELECTOR_SEPARATOR};
use crate::tree::Section;

#[derive(Debug, PartialEq)]
pub struct SectionSelector {
    matcher: StringMatcher,
}

impl SectionSelector {
    pub fn read(iter: &mut ParsingIterator) -> ParseResult<Self> {
        iter.require_whitespace_or(SELECTOR_SEPARATOR, "#")?;
        let matcher = StringMatcher::read(iter, SELECTOR_SEPARATOR)?;
        Ok(Self { matcher })
    }
}

impl<'a> MatchSelector<'a, &'a Section> for SectionSelector {
    fn matches(&self, section: &'a Section) -> bool {
        self.matcher.matches_inlines(&section.title)
    }
}
