use crate::fmt_str::inlines_to_plain_string;
use crate::matcher::Matcher;
use crate::parsing_iter::ParsingIterator;
use crate::select::base::Selector;
use crate::select::util::require_whitespace;
use crate::select::{ParseResult, SelectResult};
use crate::tree::Section;

#[derive(Debug, PartialEq)]
pub struct SectionSelector {
    matcher: Matcher,
}

impl SectionSelector {
    pub fn read<C: Iterator<Item = char>>(chars: &mut ParsingIterator<C>) -> ParseResult<SectionSelector> {
        require_whitespace(chars, "Section specifier")?;

        let matcher = Matcher::parse_matcher(chars)?;
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
