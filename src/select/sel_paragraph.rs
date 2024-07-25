use crate::matcher::StringMatcher;
use crate::parsing_iter::ParsingIterator;
use crate::select::base::Selector;
use crate::select::{ParseResult, SELECTOR_SEPARATOR};
use crate::tree::Paragraph;

#[derive(Debug, PartialEq)]
pub struct ParagraphSelector {
    matcher: StringMatcher,
}

impl ParagraphSelector {
    pub fn read(iter: &mut ParsingIterator) -> ParseResult<Self> {
        iter.require_char(':')?;
        iter.require_whitespace_or(SELECTOR_SEPARATOR, "P:")?;
        let matcher = StringMatcher::read(iter, SELECTOR_SEPARATOR)?;
        Ok(Self { matcher })
    }
}

impl<'a> Selector<'a, &'a Paragraph> for ParagraphSelector {
    fn matches(&self, paragraph: &'a Paragraph) -> bool {
        self.matcher.matches_inlines(&paragraph.body)
    }
}
