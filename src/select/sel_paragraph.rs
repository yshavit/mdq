use crate::matcher::StringMatcher;
use crate::parsing_iter::ParsingIterator;
use crate::select::match_selector::MatchSelector;
use crate::select::{ParseResult, SELECTOR_SEPARATOR};
use crate::tree::Paragraph;

#[derive(Debug, PartialEq)]
pub struct ParagraphSelector {
    matcher: StringMatcher,
}

impl MatchSelector<&Paragraph> for ParagraphSelector {
    fn matches(&self, paragraph: &Paragraph) -> bool {
        self.matcher.matches_inlines(&paragraph.body)
    }
}
