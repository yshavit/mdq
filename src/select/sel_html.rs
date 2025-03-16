use crate::matcher::StringMatcher;
use crate::select::match_selector::MatchSelector;
use crate::tree_ref::HtmlRef;

#[derive(Debug, PartialEq)]
pub struct HtmlSelector {
    matcher: StringMatcher,
}

impl MatchSelector<HtmlRef<'_>> for HtmlSelector {
    fn matches(&self, html: HtmlRef) -> bool {
        self.matcher.matches(html.0)
    }
}
