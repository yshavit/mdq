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
    use crate::select::{MatchReplace, Select, TrySelector};

    mod block_quote {
        use super::*;

        #[test]
        fn block_quote_find_matches() {
            let block_quote = new_block_quote("test content");

            let block_quote_selector = BlockQuoteSelector::from(BlockQuoteMatcher {
                text: MatchReplace::build(|b| b.match_regex("test")),
            });

            let selected = block_quote_selector
                .try_select(&MdContext::default(), block_quote)
                .unwrap();

            assert_eq!(
                selected,
                Select::Hit(vec![MdElem::BlockQuote(new_block_quote("test content"))])
            );
        }

        #[test]
        fn block_quote_replacement_matches() {
            let block_quote = new_block_quote("test content");

            let block_quote_selector = BlockQuoteSelector::from(BlockQuoteMatcher {
                text: MatchReplace::build(|b| b.match_regex("test").replacement("replacement")),
            });

            let selected = block_quote_selector
                .try_select(&MdContext::default(), block_quote)
                .unwrap();

            assert_eq!(
                selected,
                Select::Hit(vec![MdElem::BlockQuote(new_block_quote("replacement content"))])
            );
        }

        #[test]
        fn block_quote_find_misses() {
            let block_quote = new_block_quote("test content");

            let block_quote_selector = BlockQuoteSelector::from(BlockQuoteMatcher {
                text: MatchReplace::build(|b| b.match_regex("other")),
            });

            let selected = block_quote_selector
                .try_select(&MdContext::default(), block_quote)
                .unwrap();

            assert_eq!(
                selected,
                Select::Miss(MdElem::BlockQuote(new_block_quote("test content")))
            );
        }

        #[test]
        fn block_quote_replace_misses() {
            let block_quote = new_block_quote("test content");

            let block_quote_selector = BlockQuoteSelector::from(BlockQuoteMatcher {
                text: MatchReplace::build(|b| b.match_regex("other").replacement("replacement")),
            });

            let selected = block_quote_selector
                .try_select(&MdContext::default(), block_quote)
                .unwrap();

            assert_eq!(
                selected,
                Select::Miss(MdElem::BlockQuote(new_block_quote("test content")))
            );
        }

        fn new_block_quote(content: &str) -> BlockQuote {
            BlockQuote {
                body: vec![MdElem::Paragraph(Paragraph {
                    body: vec![Inline::Text(Text {
                        variant: TextVariant::Plain,
                        value: content.to_string(),
                    })],
                })],
            }
        }
    }

    mod paragraph {
        use super::*;

        #[test]
        fn paragraph_find_matches() {
            let paragraph = new_paragraph("test content");

            let paragraph_selector = ParagraphSelector::from(ParagraphMatcher {
                text: MatchReplace::build(|b| b.match_regex("test")),
            });

            let selected = paragraph_selector.try_select(&MdContext::default(), paragraph).unwrap();

            assert_eq!(
                selected,
                Select::Hit(vec![MdElem::Paragraph(new_paragraph("test content"))])
            );
        }

        #[test]
        fn paragraph_replacement_matches() {
            let paragraph = new_paragraph("test content");

            let paragraph_selector = ParagraphSelector::from(ParagraphMatcher {
                text: MatchReplace::build(|b| b.match_regex("test").replacement("replacement")),
            });

            let selected = paragraph_selector.try_select(&MdContext::default(), paragraph).unwrap();

            assert_eq!(
                selected,
                Select::Hit(vec![MdElem::Paragraph(new_paragraph("replacement content"))])
            );
        }

        #[test]
        fn paragraph_find_misses() {
            let paragraph = new_paragraph("test content");

            let paragraph_selector = ParagraphSelector::from(ParagraphMatcher {
                text: MatchReplace::build(|b| b.match_regex("other")),
            });

            let selected = paragraph_selector.try_select(&MdContext::default(), paragraph).unwrap();

            assert_eq!(selected, Select::Miss(MdElem::Paragraph(new_paragraph("test content"))));
        }

        #[test]
        fn paragraph_replace_misses() {
            let paragraph = new_paragraph("test content");

            let paragraph_selector = ParagraphSelector::from(ParagraphMatcher {
                text: MatchReplace::build(|b| b.match_regex("other").replacement("replacement")),
            });

            let selected = paragraph_selector.try_select(&MdContext::default(), paragraph).unwrap();

            assert_eq!(selected, Select::Miss(MdElem::Paragraph(new_paragraph("test content"))));
        }

        fn new_paragraph(content: &str) -> Paragraph {
            Paragraph {
                body: vec![Inline::Text(Text {
                    variant: TextVariant::Plain,
                    value: content.to_string(),
                })],
            }
        }
    }

    mod html {
        use super::*;

        #[test]
        fn html_find_matches() {
            let html = new_html("<div>test content</div>");

            let html_selector = HtmlSelector::from(HtmlMatcher {
                html: MatchReplace::build(|b| b.match_regex("test")),
            });

            let selected = html_selector.try_select(&MdContext::default(), html).unwrap();

            assert_eq!(
                selected,
                Select::Hit(vec![MdElem::BlockHtml(new_html("<div>test content</div>"))])
            );
        }

        #[test]
        fn html_replacement_matches() {
            let html = new_html("<div>test content</div>");

            let html_selector = HtmlSelector::from(HtmlMatcher {
                html: MatchReplace::build(|b| b.match_regex("test").replacement("replacement")),
            });

            let selected = html_selector.try_select(&MdContext::default(), html).unwrap();

            assert_eq!(
                selected,
                Select::Hit(vec![MdElem::BlockHtml(new_html("<div>replacement content</div>"))])
            );
        }

        #[test]
        fn html_find_misses() {
            let html = new_html("<div>test content</div>");

            let html_selector = HtmlSelector::from(HtmlMatcher {
                html: MatchReplace::build(|b| b.match_regex("other")),
            });

            let selected = html_selector.try_select(&MdContext::default(), html).unwrap();

            assert_eq!(
                selected,
                Select::Miss(MdElem::BlockHtml(new_html("<div>test content</div>")))
            );
        }

        #[test]
        fn html_replace_misses() {
            let html = new_html("<div>test content</div>");

            let html_selector = HtmlSelector::from(HtmlMatcher {
                html: MatchReplace::build(|b| b.match_regex("other").replacement("replacement")),
            });

            let selected = html_selector.try_select(&MdContext::default(), html).unwrap();

            assert_eq!(
                selected,
                Select::Miss(MdElem::BlockHtml(new_html("<div>test content</div>")))
            );
        }

        fn new_html(content: &str) -> BlockHtml {
            BlockHtml {
                value: content.to_string(),
            }
        }
    }

    mod front_matter {
        use super::*;

        #[test]
        fn front_matter_find_matches() {
            let front_matter = new_front_matter("title: test content");

            let front_matter_selector = FrontMatterSelector::from(FrontMatterMatcher {
                variant: None,
                text: MatchReplace::build(|b| b.match_regex("test")),
            });

            let selected = front_matter_selector
                .try_select(&MdContext::default(), front_matter)
                .unwrap();

            assert_eq!(
                selected,
                Select::Hit(vec![MdElem::FrontMatter(new_front_matter("title: test content"))])
            );
        }

        #[test]
        fn front_matter_replacement_matches() {
            let front_matter = new_front_matter("title: test content");

            let front_matter_selector = FrontMatterSelector::from(FrontMatterMatcher {
                variant: None,
                text: MatchReplace::build(|b| b.match_regex("test").replacement("replacement")),
            });

            let selected = front_matter_selector
                .try_select(&MdContext::default(), front_matter)
                .unwrap();

            assert_eq!(
                selected,
                Select::Hit(vec![MdElem::FrontMatter(new_front_matter(
                    "title: replacement content"
                ))])
            );
        }

        #[test]
        fn front_matter_find_misses() {
            let front_matter = new_front_matter("title: test content");

            let front_matter_selector = FrontMatterSelector::from(FrontMatterMatcher {
                variant: None,
                text: MatchReplace::build(|b| b.match_regex("other")),
            });

            let selected = front_matter_selector
                .try_select(&MdContext::default(), front_matter)
                .unwrap();

            assert_eq!(
                selected,
                Select::Miss(MdElem::FrontMatter(new_front_matter("title: test content")))
            );
        }

        #[test]
        fn front_matter_replace_misses() {
            let front_matter = new_front_matter("title: test content");

            let front_matter_selector = FrontMatterSelector::from(FrontMatterMatcher {
                variant: None,
                text: MatchReplace::build(|b| b.match_regex("other").replacement("replacement")),
            });

            let selected = front_matter_selector
                .try_select(&MdContext::default(), front_matter)
                .unwrap();

            assert_eq!(
                selected,
                Select::Miss(MdElem::FrontMatter(new_front_matter("title: test content")))
            );
        }

        fn new_front_matter(content: &str) -> FrontMatter {
            FrontMatter {
                variant: FrontMatterVariant::Yaml,
                body: content.to_string(),
            }
        }
    }
}
