use crate::parsing_iter::ParsingIterator;
use crate::select::base::Selector;
use crate::select::sel_link::LinkMatchers;
use crate::select::ParseResult;
use crate::tree::Image;
use crate::tree_ref::MdElemRef;

#[derive(Debug, PartialEq)]
pub struct ImageSelector {
    matchers: LinkMatchers,
}

impl ImageSelector {
    pub fn read(iter: &mut ParsingIterator) -> ParseResult<Self> {
        let matchers = LinkMatchers::read(iter)?;
        Ok(Self { matchers })
    }
}

impl<'a> Selector<'a, &'a Image> for ImageSelector {
    fn matches(&self, item: &'a Image) -> bool {
        self.matchers.display_matcher.matches(&item.alt) && self.matchers.url_matcher.matches(&item.link.url)
    }

    fn pick(&self, item: &'a Image) -> MdElemRef<'a> {
        item.into()
    }
}
