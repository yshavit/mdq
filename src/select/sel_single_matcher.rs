use crate::md_elem::elem::*;
use crate::select::match_selector::MatchSelector;
use crate::select::string_matcher::StringMatcher;
use crate::select::{BlockQuoteMatcher, FrontMatterMatcher, HtmlMatcher, ParagraphMatcher, SectionMatcher};
use paste::paste;

macro_rules! single_matcher_adapter {
    { $name:ident {$matcher_field:ident} $match_fn:ident $tree_struct_field:ident } => {
        paste! {
            #[derive(Debug, PartialEq)]
            pub struct [<$name Selector>] {
                matcher: StringMatcher,
            }

            impl MatchSelector<$name> for [<$name Selector>] {
                fn matches(&self, matcher: &$name) -> bool {
                    self.matcher.$match_fn(&matcher.$tree_struct_field)
                }
            }

            impl From< [<$name Matcher>] > for [<$name Selector>] {
                fn from(value: [<$name Matcher>]) -> Self {
                    Self { matcher: value.$matcher_field.into() }
                }
            }
        }
    };
}

single_matcher_adapter! { Section {title} matches_any body }
single_matcher_adapter! { BlockQuote {text} matches_any body }
single_matcher_adapter! { Paragraph {text} matches_inlines body }

#[derive(Debug, PartialEq)]
pub struct HtmlSelector {
    matcher: StringMatcher,
}

impl From<HtmlMatcher> for HtmlSelector {
    fn from(value: HtmlMatcher) -> Self {
        Self {
            matcher: value.html.into(),
        }
    }
}

impl MatchSelector<BlockHtml> for HtmlSelector {
    fn matches(&self, html: &BlockHtml) -> bool {
        self.matcher.matches(&html.value)
    }
}

#[derive(Debug, PartialEq)]
pub struct FrontMatterSelector {
    variant: Option<FrontMatterVariant>,
    text: StringMatcher,
}

impl From<FrontMatterMatcher> for FrontMatterSelector {
    fn from(value: FrontMatterMatcher) -> Self {
        Self {
            variant: value.variant,
            text: value.text.into(),
        }
    }
}

impl MatchSelector<FrontMatter> for FrontMatterSelector {
    fn matches(&self, front_matter: &FrontMatter) -> bool {
        let variant_selected = self
            .variant
            .map(|selected| selected == front_matter.variant)
            .unwrap_or(true);
        variant_selected && self.text.matches(&front_matter.body)
    }
}
