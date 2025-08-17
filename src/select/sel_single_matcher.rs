use crate::md_elem::elem::*;
use crate::md_elem::MdContext;
use crate::select::string_matcher::StringMatcher;
use crate::select::{
    match_selector, BlockQuoteMatcher, FrontMatterMatcher, HtmlMatcher, ParagraphMatcher, Result, Select, TrySelector,
};
use paste::paste;

macro_rules! single_matcher_struct {
    { $name:ident {$matcher_field:ident} } => {
        paste! {
            #[derive(Debug, PartialEq)]
            pub(crate) struct [<$name Selector>] {
                matcher: StringMatcher,
            }

            impl From< [<$name Matcher>] > for [<$name Selector>] {
                fn from(value: [<$name Matcher>]) -> Self {
                    Self { matcher: value.$matcher_field.into() }
                }
            }
        }
    };
}

single_matcher_struct! { BlockQuote {text} }
single_matcher_struct! { Paragraph {text} }
single_matcher_struct! { Html {html} }

impl TrySelector<BlockQuote> for BlockQuoteSelector {
    fn try_select(&self, _: &MdContext, item: BlockQuote) -> Result<Select> {
        let replaced = self
            .matcher
            .match_replace_any(item.body)
            .map_err(|e| e.to_select_error("block quote"))?;

        let result = BlockQuote { body: replaced.item };
        Ok(match_selector::make_select_result(result, replaced.matched_any))
    }
}

impl TrySelector<Paragraph> for ParagraphSelector {
    fn try_select(&self, _: &MdContext, item: Paragraph) -> Result<Select> {
        let replaced = self
            .matcher
            .match_replace_inlines(item.body)
            .map_err(|e| e.to_select_error("paragraph"))?;

        let result = Paragraph { body: replaced.item };
        Ok(match_selector::make_select_result(result, replaced.matched_any))
    }
}

impl TrySelector<BlockHtml> for HtmlSelector {
    fn try_select(&self, _: &MdContext, item: BlockHtml) -> Result<Select> {
        let replaced = self
            .matcher
            .match_replace_string(item.value)
            .map_err(|e| e.to_select_error("html"))?;

        let result = BlockHtml { value: replaced.item };
        Ok(match_selector::make_select_result(result, replaced.matched_any))
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

impl TrySelector<FrontMatter> for FrontMatterSelector {
    fn try_select(&self, _: &MdContext, item: FrontMatter) -> Result<Select> {
        // Check if variant matches (if specified)
        let variant_selected = self.variant.map(|selected| selected == item.variant).unwrap_or(true);

        if !variant_selected {
            return Ok(Select::Miss(item.into()));
        }

        // Check and potentially replace the text
        let replaced = self
            .text
            .match_replace_string(item.body)
            .map_err(|e| e.to_select_error("front matter"))?;

        let result = FrontMatter {
            variant: item.variant,
            body: replaced.item,
        };
        Ok(match_selector::make_select_result(result, replaced.matched_any))
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
