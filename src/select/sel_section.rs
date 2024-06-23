use crate::matcher::StringMatcher;
use crate::parsing_iter::ParsingIterator;
use crate::select::base::Selector;
use crate::select::{ParseResult, SelectResult, SELECTOR_SEPARATOR};
use crate::tree::Section;

#[derive(Debug, PartialEq)]
pub struct SectionSelector {
    matcher: StringMatcher,
}

impl SectionSelector {
    pub fn read(iter: &mut ParsingIterator) -> ParseResult<Self> {
        let matcher = StringMatcher::read(iter, SELECTOR_SEPARATOR)?;
        Ok(Self { matcher })
    }
}

impl<'a> Selector<'a, &'a Section> for SectionSelector {
    fn matches(&self, section: &'a Section) -> bool {
        self.matcher.matches_inlines(&section.title)
    }

    fn pick(&self, item: &'a Section) -> SelectResult<'a> {
        SelectResult::Multi(&item.body)
    }
}
