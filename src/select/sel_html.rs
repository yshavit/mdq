use crate::matcher::StringMatcher;
use crate::parsing_iter::ParsingIterator;
use crate::select::base::Selector;
use crate::select::{ParseResult, SELECTOR_SEPARATOR};
use crate::tree_ref::HtmlRef;

#[derive(Debug, PartialEq)]
pub struct HtmlSelector {
    matcher: StringMatcher,
}

impl HtmlSelector {
    pub fn read(iter: &mut ParsingIterator) -> ParseResult<Self> {
        iter.require_str("/>")?;
        let matcher = StringMatcher::read(iter, SELECTOR_SEPARATOR)?;
        Ok(Self { matcher })
    }
}

impl<'a> Selector<'a, HtmlRef<'a>> for HtmlSelector {
    fn matches(&self, html: HtmlRef<'a>) -> bool {
        self.matcher.matches(html.0)
    }
}
