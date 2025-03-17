use crate::matcher::StringMatcher;
use crate::query::selectors::Matcher;
use crate::select::match_selector::MatchSelector;
use crate::tree::{BlockQuote, Paragraph, Section};
use crate::tree_ref::HtmlRef;
use paste::paste;

macro_rules! single_matcher_adapter {
    { $name:ident $match_fn:ident $tree_struct_field:ident } => {
        paste! {
            #[derive(Debug, PartialEq)]
            pub struct [<$name Selector>] {
                matcher: StringMatcher,
            }

            impl MatchSelector<&$name> for [<$name Selector>] {
                fn matches(&self, matcher: &$name) -> bool {
                    self.matcher.$match_fn(&matcher.$tree_struct_field)
                }
            }

            impl From<Matcher> for [<$name Selector>] {
                fn from(value: Matcher) -> Self {
                    Self { matcher: value.into() }
                }
            }
        }
    };
}

single_matcher_adapter! { Section matches_any body }
single_matcher_adapter! { BlockQuote matches_any body }
single_matcher_adapter! { Paragraph matches_inlines body }

#[derive(Debug, PartialEq)]
pub struct HtmlSelector {
    matcher: StringMatcher,
}

impl From<Matcher> for HtmlSelector {
    fn from(value: Matcher) -> Self {
        Self { matcher: value.into() }
    }
}

impl MatchSelector<HtmlRef<'_>> for HtmlSelector {
    fn matches(&self, html: HtmlRef) -> bool {
        self.matcher.matches(html.0)
    }
}
