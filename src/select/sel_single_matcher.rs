use crate::md_elem::elem::*;
use crate::select::match_selector::MatchSelector;
use crate::select::string_matcher::{StringMatchError, StringMatcher};
use crate::select::{BlockQuoteMatcher, FrontMatterMatcher, HtmlMatcher, ParagraphMatcher};
use paste::paste;

macro_rules! single_matcher_adapter {
    { $name:ident {$matcher_field:ident} $match_fn:ident $tree_struct_field:ident $selector_name:literal } => {
        paste! {
            #[derive(Debug, PartialEq)]
            pub(crate) struct [<$name Selector>] {
                matcher: StringMatcher,
            }

            impl MatchSelector<$name> for [<$name Selector>] {
                const NAME: &'static str = $selector_name;

                fn matches(&self, matcher: &$name) -> Result<bool, StringMatchError> {
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

single_matcher_adapter! { BlockQuote {text} matches_any body "block quote" }
single_matcher_adapter! { Paragraph {text} matches_inlines body "paragraph" }

#[derive(Debug, PartialEq)]
pub(crate) struct HtmlSelector {
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
    const NAME: &'static str = "html";

    fn matches(&self, html: &BlockHtml) -> Result<bool, StringMatchError> {
        self.matcher.matches(&html.value)
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct FrontMatterSelector {
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
    const NAME: &'static str = "front matter";

    fn matches(&self, front_matter: &FrontMatter) -> Result<bool, StringMatchError> {
        let variant_selected = self
            .variant
            .map(|selected| selected == front_matter.variant)
            .unwrap_or(true);
        Ok(variant_selected && self.text.matches(&front_matter.body)?)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::md_elem::{
        elem::{BlockHtml, BlockQuote, FrontMatter, FrontMatterVariant, Inline, Paragraph, Text, TextVariant},
        MdContext, MdElem,
    };
    use crate::select::{MatchReplace, Matcher, TrySelector};

    #[test]
    fn block_quote_selector_match_error() {
        let block_quote_matcher = BlockQuoteMatcher {
            text: MatchReplace {
                matcher: Matcher::Text {
                    case_sensitive: false,
                    anchor_start: false,
                    text: "test".to_string(),
                    anchor_end: false,
                },
                replacement: Some("replacement".to_string()),
            },
        };

        let block_quote = BlockQuote {
            body: vec![MdElem::Paragraph(Paragraph {
                body: vec![Inline::Text(Text {
                    variant: TextVariant::Plain,
                    value: "test content".to_string(),
                })],
            })],
        };

        let block_quote_selector = BlockQuoteSelector::from(block_quote_matcher);

        assert_eq!(
            block_quote_selector.matches(&block_quote),
            Err(StringMatchError::NotSupported)
        );

        assert_eq!(
            block_quote_selector
                .try_select(&MdContext::default(), block_quote)
                .unwrap_err()
                .to_string(),
            "block quote selector does not support string replace"
        );
    }

    #[test]
    fn paragraph_selector_match_error() {
        let paragraph_matcher = ParagraphMatcher {
            text: MatchReplace {
                matcher: Matcher::Text {
                    case_sensitive: false,
                    anchor_start: false,
                    text: "test".to_string(),
                    anchor_end: false,
                },
                replacement: Some("replacement".to_string()),
            },
        };

        let paragraph = Paragraph { body: vec![] };

        let paragraph_selector = ParagraphSelector::from(paragraph_matcher);

        assert_eq!(
            paragraph_selector.matches(&paragraph),
            Err(StringMatchError::NotSupported)
        );

        assert_eq!(
            paragraph_selector
                .try_select(&MdContext::default(), paragraph)
                .unwrap_err()
                .to_string(),
            "paragraph selector does not support string replace"
        );
    }

    #[test]
    fn html_selector_match_error() {
        let html_matcher = HtmlMatcher {
            html: MatchReplace {
                matcher: Matcher::Text {
                    case_sensitive: false,
                    anchor_start: false,
                    text: "div".to_string(),
                    anchor_end: false,
                },
                replacement: Some("replacement".to_string()),
            },
        };

        let block_html = BlockHtml {
            value: "<div>content</div>".to_string(),
        };

        let html_selector = HtmlSelector::from(html_matcher);

        assert_eq!(html_selector.matches(&block_html), Err(StringMatchError::NotSupported));

        assert_eq!(
            html_selector
                .try_select(&MdContext::default(), block_html)
                .unwrap_err()
                .to_string(),
            "html selector does not support string replace"
        );
    }

    #[test]
    fn front_matter_selector_match_error() {
        let front_matter_matcher = FrontMatterMatcher {
            variant: None,
            text: MatchReplace {
                matcher: Matcher::Text {
                    case_sensitive: false,
                    anchor_start: false,
                    text: "title".to_string(),
                    anchor_end: false,
                },
                replacement: Some("replacement".to_string()),
            },
        };

        let front_matter = FrontMatter {
            variant: FrontMatterVariant::Yaml,
            body: "title: test".to_string(),
        };

        let front_matter_selector = FrontMatterSelector::from(front_matter_matcher);

        assert_eq!(
            front_matter_selector.matches(&front_matter),
            Err(StringMatchError::NotSupported)
        );

        assert_eq!(
            front_matter_selector
                .try_select(&MdContext::default(), front_matter)
                .unwrap_err()
                .to_string(),
            "front matter selector does not support string replace"
        );
    }
}
