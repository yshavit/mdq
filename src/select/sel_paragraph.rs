use crate::matcher::StringMatcher;
use crate::select::match_selector::MatchSelector;
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
