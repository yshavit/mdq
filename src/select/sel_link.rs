use crate::matcher::StringMatcher;
use crate::parsing_iter::ParsingIterator;
use crate::select::base::Selector;
use crate::select::ParseResult;
use crate::tree::Link;

#[derive(Debug, PartialEq)]
pub struct LinkSelector {
    matchers: LinkMatchers,
}

impl LinkSelector {
    pub fn read(iter: &mut ParsingIterator) -> ParseResult<Self> {
        let matchers = LinkMatchers::read(iter)?;
        Ok(Self { matchers })
    }
}

impl<'a> Selector<'a, &'a Link> for LinkSelector {
    fn matches(&self, item: &'a Link) -> bool {
        self.matchers.display_matcher.matches_inlines(&item.text)
            && self.matchers.url_matcher.matches(&item.link_definition.url)
    }
}

#[derive(Debug, PartialEq)]
pub struct LinkMatchers {
    pub display_matcher: StringMatcher,
    pub url_matcher: StringMatcher,
}

impl LinkMatchers {
    pub fn read(iter: &mut ParsingIterator) -> ParseResult<Self> {
        let display_matcher = StringMatcher::read(iter, ']')?;
        iter.require_str("](")?;
        let url_matcher = StringMatcher::read(iter, ')')?;
        iter.require_char(')')?;
        Ok(Self {
            display_matcher,
            url_matcher,
        })
    }
}
