use crate::fmt_str::inlines_to_plain_string;
use crate::matcher::StringMatcher;
use crate::parsing_iter::ParsingIterator;
use crate::select::base::Selector;
use crate::select::{ParseResult, SelectResult};
use crate::tree::Section;

#[derive(Debug, PartialEq)]
pub struct SectionSelector {
    matcher: StringMatcher,
}

impl SectionSelector {
    pub fn read(chars: &mut ParsingIterator) -> ParseResult<SectionSelector> {
        let matcher = StringMatcher::parse_matcher(chars)?;
        Ok(Self { matcher })
    }
}

impl<'a> Selector<'a, &'a Section> for SectionSelector {
    fn matches(&self, section: &'a Section) -> bool {
        let header_text = inlines_to_plain_string(&section.title);
        self.matcher.matches(&header_text)
    }

    fn pick(&self, item: &'a Section) -> SelectResult<'a> {
        SelectResult::Multi(&item.body)
    }
}
