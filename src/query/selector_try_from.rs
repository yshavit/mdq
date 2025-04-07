use crate::query::query::Rule;
use crate::query::traversal::{ByRule, OneOf, PairMatcher};
use crate::query::traversal_composites::{
    BlockQuoteTraverser, CodeBlockTraverser, HtmlTraverser, LinkTraverser, ListItemTraverser, ParagraphTraverser,
    SectionResults, SectionTraverser, TableTraverser,
};
use crate::query::{DetachedSpan, Pair, Pairs, ParseError, Query};
use crate::select::{
    AnyVariant, CodeBlockMatcher, LinklikeMatcher, ListItemMatcher, ListItemTask, Matcher, Selector, SelectorChain,
    TableSliceMatcher,
};

impl TryFrom<Pairs<'_>> for SelectorChain {
    type Error = ParseError;

    fn try_from(root: Pairs) -> Result<Self, Self::Error> {
        // Get "all" the selector chains; there should be at most 1.
        let selector_chains = ByRule::new(Rule::selector_chain).find_all_in(root);
        let mut selectors: Vec<Selector> = Vec::new();
        for chain in selector_chains {
            // within the chain, get the selectors; for each one, get its inners (there should be exactly one) and get
            // its selector.
            let selector_inners = ByRule::new(Rule::selector).find_all_in(chain.into_inner());
            for selector_pair in selector_inners {
                let selector = Selector::find_selector(selector_pair)?;
                selectors.push(selector);
            }
        }
        Ok(Self { selectors })
    }
}

impl Selector {
    pub fn try_parse(value: &'_ str) -> Result<SelectorChain, ParseError> {
        let parsed: Pairs = Query::parse(value).map_err(|err| ParseError::from(err))?;
        let parsed_selectors = SelectorChain::try_from(parsed).map_err(|e| ParseError::from(e))?;
        Ok(parsed_selectors)
    }

    fn find_selector(root: Pair) -> Result<Self, ParseError> {
        let span = DetachedSpan::from(&root);
        let to_parse_error = |es: String| -> ParseError { ParseError::Other(span, es) };

        let (as_rule, children) = (root.as_rule(), root.into_inner());

        match as_rule {
            Rule::select_section => {
                let SectionResults { title } = SectionTraverser::traverse(children);
                let matcher = Matcher::try_from(title.take().map_err(to_parse_error)?)?;
                Ok(Self::Section(matcher))
            }
            Rule::select_list_item => {
                let res = ListItemTraverser::traverse(children);
                let ordered = res.list_ordered.is_present();
                let task = if res.task_checked.is_present() {
                    ListItemTask::Selected
                } else if res.task_unchecked.is_present() {
                    ListItemTask::Unselected
                } else if res.task_either.is_present() {
                    ListItemTask::Either
                } else {
                    ListItemTask::None
                };
                let m = res.contents.take().map_err(to_parse_error)?;
                let matcher = Matcher::try_from(m)?;
                Ok(Self::ListItem(ListItemMatcher { ordered, task, matcher }))
            }
            Rule::select_link => {
                let res = LinkTraverser::traverse(children);
                let display_matcher = Matcher::try_from(res.display_text.take().map_err(to_parse_error)?)?;
                let url_matcher = Matcher::try_from(res.url_text.take().map_err(to_parse_error)?)?;
                let link_matcher = LinklikeMatcher {
                    display_matcher,
                    url_matcher,
                };
                if res.image_start.is_present() {
                    Ok(Self::Image(link_matcher))
                } else {
                    Ok(Self::Link(link_matcher))
                }
            }
            Rule::select_block_quote => {
                let res = BlockQuoteTraverser::traverse(children);
                let matcher = Matcher::try_from(res.text.take().map_err(to_parse_error)?)?;
                Ok(Self::BlockQuote(matcher))
            }
            Rule::select_code_block => {
                let res = CodeBlockTraverser::traverse(children);
                let language_matcher = Matcher::try_from(res.language.take().map_err(to_parse_error)?)?;
                let contents_matcher = Matcher::try_from(res.text.take().map_err(to_parse_error)?)?;
                Ok(Self::CodeBlock(CodeBlockMatcher {
                    language: language_matcher,
                    contents: contents_matcher,
                }))
            }
            Rule::select_html => {
                let res = HtmlTraverser::traverse(children);
                let matcher = Matcher::try_from(res.text.take().map_err(to_parse_error)?)?;
                Ok(Self::Html(matcher))
            }
            Rule::select_paragraph => {
                let res = ParagraphTraverser::traverse(children);
                let matcher = Matcher::try_from(res.text.take().map_err(to_parse_error)?)?;
                Ok(Self::Paragraph(matcher))
            }
            Rule::select_table => {
                let res = TableTraverser::traverse(children);
                let column_matcher_span = res.column.take().map_err(to_parse_error)?;
                let detached_span = match &column_matcher_span {
                    None => DetachedSpan::from(span),
                    Some(column_matcher_span) => DetachedSpan::from(column_matcher_span),
                };
                let column_matcher = Matcher::try_from(column_matcher_span)?;
                if matches!(column_matcher, Matcher::Any(AnyVariant::Implicit)) {
                    return Err(ParseError::Other(
                        detached_span,
                        "table column matcher cannot empty; use an explicit \"*\"".to_string(),
                    ));
                }
                let row_matcher = Matcher::try_from(res.row.take().map_err(to_parse_error)?)?;
                Ok(Self::Table(TableSliceMatcher {
                    column: column_matcher,
                    row: row_matcher,
                }))
            }
            Rule::selector | _ => {
                // We only expect to get here if we hit the Rule::selector rule. In that case, traversing the inners
                // (there should only be one) will get us the actual, concrete selector for this selector union.
                let mut one = OneOf::default();
                for child in children {
                    let found_in_child = Self::find_selector(child)?;
                    one.store(found_in_child);
                }
                let maybe = one.take().map_err(to_parse_error)?;
                Ok(maybe.unwrap())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::query::query::{Query, StringVariant};
    use crate::query::traversal::OneOf;
    use crate::query::Error;
    use pest::error::ErrorVariant;
    use pest::Span;
    use std::cmp::{max, min};

    impl Matcher {
        pub fn parse(variant: StringVariant, query_str: &str) -> Result<(Matcher, &str), ParseError> {
            let parse_attempt = variant.parse(query_str);
            let (only_pair, remaining) = match parse_attempt {
                Err(err) => {
                    let ErrorVariant::ParsingError { positives, .. } = &err.variant else {
                        return Err(err.into());
                    };
                    // See what this thing tried to parse. If it failed at trying to parse the string variant itself,
                    // it didn't consume any input; just return an empty parse. Otherwise, it found something to parse,
                    // but that something is invalid -- that's a real error.
                    // This is basically equivalent to a `string_variant?` usage in the grammar.
                    match positives.as_slice() {
                        &[only] if only == variant.as_rule() => (None, query_str),
                        _ => return Err(err.into()),
                    }
                }
                Ok((top, remaining)) => {
                    let mut span = DetachedSpan::default();
                    let mut one_of = OneOf::default();
                    for pair in top {
                        let pair_span = pair.as_span();
                        span.start = min(span.start, pair_span.start());
                        span.end = max(span.end, pair_span.start());
                        one_of.store(pair);
                    }
                    let matcher = one_of.take().map_err(|msg| ParseError::Other(span, msg))?;
                    (matcher, remaining)
                }
            };
            let matcher = Matcher::try_from(only_pair)?;
            Ok((matcher, remaining))
        }
    }

    mod chaining {
        use super::*;

        #[test]
        fn totally_empty() {
            find_empty_chain("");
        }

        #[test]
        fn only_spaces() {
            find_empty_chain("");
        }

        #[test]
        fn only_one_pipe() {
            find_empty_chain("|");
        }

        #[test]
        fn only_multiple_pipes() {
            find_empty_chain("|| |");
        }

        #[test]
        fn prefix_chaining() {
            find_selector("| #", Selector::Section(Matcher::Any(AnyVariant::Implicit)))
        }

        #[test]
        fn suffix_chaining() {
            find_selector("# |", Selector::Section(Matcher::Any(AnyVariant::Implicit)))
        }

        #[test]
        fn useful_chaining() {
            find_selectors(
                "# | []()",
                vec![
                    Selector::Section(Matcher::Any(AnyVariant::Implicit)),
                    Selector::Link(LinklikeMatcher {
                        display_matcher: Matcher::Any(AnyVariant::Implicit),
                        url_matcher: Matcher::Any(AnyVariant::Implicit),
                    }),
                ],
            );
        }

        #[test]
        fn empty_intermediate_chains() {
            find_selectors(
                "# || | []()",
                vec![
                    Selector::Section(Matcher::Any(AnyVariant::Implicit)),
                    Selector::Link(LinklikeMatcher {
                        display_matcher: Matcher::Any(AnyVariant::Implicit),
                        url_matcher: Matcher::Any(AnyVariant::Implicit),
                    }),
                ],
            );
        }
    }

    mod section {
        use super::*;

        #[test]
        fn section_no_matcher() {
            find_selector("#", Selector::Section(Matcher::Any(AnyVariant::Implicit)));
        }

        #[test]
        fn section_with_matcher() {
            find_selector("# foo", Selector::Section(matcher_text(false, "foo", false)));
        }
    }

    mod list_item {
        use super::*;
        use indoc::indoc;

        #[test]
        fn unordered_list_item_no_matcher() {
            find_selector(
                "-",
                Selector::ListItem(ListItemMatcher {
                    ordered: false,
                    task: ListItemTask::None,
                    matcher: Matcher::Any(AnyVariant::Implicit),
                }),
            )
        }

        #[test]
        fn unordered_list_item_with_matcher() {
            find_selector(
                "- hello",
                Selector::ListItem(ListItemMatcher {
                    ordered: false,
                    task: ListItemTask::None,
                    matcher: matcher_text(false, "hello", false),
                }),
            )
        }

        #[test]
        fn unordered_list_item_with_matcher_no_space() {
            expect_parse_error(
                "-hello",
                indoc! {r#"
                     --> 1:2
                      |
                    1 | -hello
                      |  ^---
                      |
                      = expected end of input or space"#},
            );
        }

        #[test]
        fn ordered_list_item_no_matcher() {
            find_selector(
                "1.",
                Selector::ListItem(ListItemMatcher {
                    ordered: true,
                    task: ListItemTask::None,
                    matcher: Matcher::Any(AnyVariant::Implicit),
                }),
            )
        }

        #[test]
        fn ordered_list_item_with_matcher() {
            find_selector(
                "1. world",
                Selector::ListItem(ListItemMatcher {
                    ordered: true,
                    task: ListItemTask::None,
                    matcher: matcher_text(false, "world", false),
                }),
            )
        }

        #[test]
        fn ordered_list_item_with_matcher_anchored() {
            find_selector(
                "1. ^ world $",
                Selector::ListItem(ListItemMatcher {
                    ordered: true,
                    task: ListItemTask::None,
                    matcher: matcher_text(true, "world", true),
                }),
            )
        }

        #[test]
        fn unordered_unchecked_task() {
            find_selector(
                "- [ ]",
                Selector::ListItem(ListItemMatcher {
                    ordered: false,
                    task: ListItemTask::Unselected,
                    matcher: Matcher::Any(AnyVariant::Implicit),
                }),
            )
        }

        #[test]
        fn unordered_checked_task() {
            find_selector(
                "- [x]",
                Selector::ListItem(ListItemMatcher {
                    ordered: false,
                    task: ListItemTask::Selected,
                    matcher: Matcher::Any(AnyVariant::Implicit),
                }),
            )
        }

        #[test]
        fn unordered_either_task() {
            find_selector(
                "- [?]",
                Selector::ListItem(ListItemMatcher {
                    ordered: false,
                    task: ListItemTask::Either,
                    matcher: Matcher::Any(AnyVariant::Implicit),
                }),
            )
        }

        #[test]
        fn task_cannot_have_extra_spaces() {
            expect_parse_error(
                "- [  ]",
                indoc! {r#"
                 --> 1:5
                  |
                1 | - [  ]
                  |     ^---
                  |
                  = expected "]""#},
            );
        }

        #[test]
        fn unordered_task_with_matcher() {
            find_selector(
                "- [?] my task",
                Selector::ListItem(ListItemMatcher {
                    ordered: false,
                    task: ListItemTask::Either,
                    matcher: matcher_text(false, "my task", false),
                }),
            )
        }

        #[test]
        fn ordered_task() {
            find_selector(
                "1. [ ] my ordered task",
                Selector::ListItem(ListItemMatcher {
                    ordered: true,
                    task: ListItemTask::Unselected,
                    matcher: matcher_text(false, "my ordered task", false),
                }),
            )
        }
    }

    mod link {
        use super::*;

        #[test]
        fn link_no_matchers() {
            find_selector(
                "[]()",
                Selector::Link(LinklikeMatcher {
                    display_matcher: Matcher::Any(AnyVariant::Implicit),
                    url_matcher: Matcher::Any(AnyVariant::Implicit),
                }),
            )
        }

        #[test]
        fn link_with_display() {
            find_selector(
                "[hello]()",
                Selector::Link(LinklikeMatcher {
                    display_matcher: matcher_text(false, "hello", false),
                    url_matcher: Matcher::Any(AnyVariant::Implicit),
                }),
            )
        }

        #[test]
        fn link_with_url() {
            find_selector(
                "[](example.com)",
                Selector::Link(LinklikeMatcher {
                    display_matcher: Matcher::Any(AnyVariant::Implicit),
                    url_matcher: matcher_text(false, "example.com", false),
                }),
            )
        }

        #[test]
        fn link_with_both() {
            find_selector(
                "[click here](example.com)",
                Selector::Link(LinklikeMatcher {
                    display_matcher: matcher_text(false, "click here", false),
                    url_matcher: matcher_text(false, "example.com", false),
                }),
            )
        }

        #[test]
        fn image_no_matchers() {
            find_selector(
                "![]()",
                Selector::Image(LinklikeMatcher {
                    display_matcher: Matcher::Any(AnyVariant::Implicit),
                    url_matcher: Matcher::Any(AnyVariant::Implicit),
                }),
            )
        }

        #[test]
        fn image_with_alt() {
            find_selector(
                "![alt text]()",
                Selector::Image(LinklikeMatcher {
                    display_matcher: matcher_text(false, "alt text", false),
                    url_matcher: Matcher::Any(AnyVariant::Implicit),
                }),
            )
        }
    }

    mod block_quote {
        use super::*;

        #[test]
        fn block_quote_no_matcher() {
            find_selector(">", Selector::BlockQuote(Matcher::Any(AnyVariant::Implicit)))
        }

        #[test]
        fn block_quote_with_text() {
            find_selector(
                "> quoted text",
                Selector::BlockQuote(matcher_text(false, "quoted text", false)),
            )
        }

        #[test]
        fn block_quote_with_anchored_text() {
            find_selector(
                "> ^start end$",
                Selector::BlockQuote(matcher_text(true, "start end", true)),
            )
        }
    }

    mod code_block {
        use super::*;

        #[test]
        fn code_block_no_matchers() {
            find_selector(
                "```",
                Selector::CodeBlock(CodeBlockMatcher {
                    language: Matcher::Any(AnyVariant::Implicit),
                    contents: Matcher::Any(AnyVariant::Implicit),
                }),
            )
        }

        #[test]
        fn code_block_with_only_language() {
            find_selector(
                "```rust",
                Selector::CodeBlock(CodeBlockMatcher {
                    language: matcher_text(false, "rust", false),
                    contents: Matcher::Any(AnyVariant::Implicit),
                }),
            )
        }

        #[test]
        fn code_block_with_only_language_and_trailing_space() {
            find_selector(
                "```rust",
                Selector::CodeBlock(CodeBlockMatcher {
                    language: matcher_text(false, "rust", false),
                    contents: Matcher::Any(AnyVariant::Implicit),
                }),
            )
        }

        #[test]
        fn code_block_with_only_content() {
            find_selector(
                "``` fn main()",
                Selector::CodeBlock(CodeBlockMatcher {
                    language: Matcher::Any(AnyVariant::Implicit),
                    contents: matcher_text(false, "fn main()", false),
                }),
            )
        }

        #[test]
        fn code_block_with_both() {
            find_selector(
                "```rust fn main()",
                Selector::CodeBlock(CodeBlockMatcher {
                    language: matcher_text(false, "rust", false),
                    contents: matcher_text(false, "fn main()", false),
                }),
            )
        }
    }

    mod html {
        use super::*;
        use indoc::indoc;
        use regex::Regex;

        #[test]
        fn html_no_matcher() {
            find_selector("</>", Selector::Html(Matcher::Any(AnyVariant::Implicit)))
        }

        #[test]
        fn html_with_text() {
            let matcher = Matcher::Text {
                case_sensitive: true,
                anchor_start: false,
                text: "<div>".to_string(),
                anchor_end: false,
            };
            find_selector("</> '<div>'", Selector::Html(matcher))
        }

        #[test]
        fn html_with_unquoted_text() {
            expect_parse_error(
                "</> <div>",
                indoc! {r#"
                     --> 1:5
                      |
                    1 | </> <div>
                      |     ^---
                      |
                      = expected end of input, "*", unquoted string, regex, quoted string, or "^""#},
            );
        }

        #[test]
        fn html_with_regex() {
            find_selector(
                "</> /<div.*>/",
                Selector::Html(Matcher::Regex(Regex::new("<div.*>").unwrap())),
            )
        }
    }

    mod paragraph {
        use super::*;

        #[test]
        fn paragraph_no_matcher() {
            find_selector("P:", Selector::Paragraph(Matcher::Any(AnyVariant::Implicit)))
        }

        #[test]
        fn paragraph_with_text() {
            find_selector(
                "P: some text",
                Selector::Paragraph(matcher_text(false, "some text", false)),
            )
        }

        #[test]
        fn paragraph_with_anchored_text() {
            find_selector("P: ^start$", Selector::Paragraph(matcher_text(true, "start", true)))
        }
    }

    mod table {
        use super::*;
        use indoc::indoc;

        #[test]
        fn table_no_header_matcher() {
            expect_parse_error(
                ":-: :-:",
                indoc! {r#"
                     --> 1:5
                      |
                    1 | :-: :-:
                      |     ^
                      |
                      = table column matcher cannot empty; use an explicit "*""#},
            );
        }

        #[test]
        fn table_no_spaces_at_all() {
            expect_parse_error(
                ":-::-:",
                indoc! {r#"
                     --> 1:4
                      |
                    1 | :-::-:
                      |    ^---
                      |
                      = expected space"#},
            );
        }

        #[test]
        fn table_asterisk_column() {
            find_selector(
                ":-: * :-:",
                Selector::Table(TableSliceMatcher {
                    column: Matcher::Any(AnyVariant::Explicit),
                    row: Matcher::Any(AnyVariant::Implicit),
                }),
            )
        }

        #[test]
        fn table_with_column() {
            find_selector(
                ":-: Header :-:",
                Selector::Table(TableSliceMatcher {
                    column: matcher_text(false, "Header", false),
                    row: Matcher::Any(AnyVariant::Implicit),
                }),
            )
        }

        #[test]
        fn table_with_row() {
            find_selector(
                ":-: * :-: Data",
                Selector::Table(TableSliceMatcher {
                    column: Matcher::Any(AnyVariant::Explicit),
                    row: matcher_text(false, "Data", false),
                }),
            )
        }

        #[test]
        fn table_with_both() {
            find_selector(
                ":-: Header :-: Data",
                Selector::Table(TableSliceMatcher {
                    column: matcher_text(false, "Header", false),
                    row: matcher_text(false, "Data", false),
                }),
            )
        }
    }

    mod invalid {
        use super::*;
        use indoc::indoc;

        #[test]
        fn just_a_string() {
            expect_parse_error(
                "hello",
                indoc! {r"
                     --> 1:1
                      |
                    1 | hello
                      | ^---
                      |
                      = expected valid query"},
            )
        }
    }

    fn find_empty_chain(query_text: &str) {
        find_selectors(query_text, vec![]);
    }

    fn find_selector(query_text: &str, expect: Selector) {
        find_selectors(query_text, vec![expect])
    }

    fn find_selectors(query_text: &str, expect: Vec<Selector>) {
        let pairs = Query::parse(query_text);
        let pairs = match pairs {
            Ok(pairs) => pairs,
            Err(err) => {
                panic!("{err}");
            }
        };

        let result = SelectorChain::try_from(pairs);
        assert_eq!(result, Ok(SelectorChain { selectors: expect }));
    }

    fn expect_parse_error(query_text: &str, expect: &str) {
        let pairs = Query::parse(query_text);
        let err_msg = match pairs {
            Ok(pairs) => match SelectorChain::try_from(pairs) {
                Ok(SelectorChain { selectors }) => panic!("{selectors:?}"),
                Err(ParseError::Pest(err)) => format!("{err}"),
                Err(ParseError::Other(span, message)) => {
                    let error = Error::new_from_span(
                        ErrorVariant::CustomError { message },
                        Span::new(query_text, span.start, span.end).unwrap(),
                    );
                    format! {"{error}"}
                }
            },
            Err(err) => format!("{err}"),
        };
        assert_eq!(err_msg, expect);
    }

    fn matcher_text(anchor_start: bool, text: &str, anchor_end: bool) -> Matcher {
        Matcher::Text {
            case_sensitive: false,
            anchor_start,
            text: text.to_string(),
            anchor_end,
        }
    }
}
