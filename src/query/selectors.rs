use crate::query::query::{
    BlockQuoteTraverser, ByRule, CodeBlockTraverser, HtmlTraverser, LinkTraverser, ListItemTraverser, PairMatcher,
    ParagraphTraverser, ParsedString, Rule, SectionResults, SectionTraverser, TableTraverser,
};
use pest::iterators::{Pair, Pairs};

#[derive(Eq, PartialEq, Debug)]
pub enum Matcher {
    Text(bool, String, bool),
    Regex(String),
    Any,
}

impl Matcher {
    fn take_from_option(pair: Option<Pair<Rule>>) -> Result<Matcher, String> {
        let Some(pair) = pair else { return Ok(Matcher::Any) };
        let parsed_string: ParsedString = pair.into_inner().try_into()?;
        Ok(if parsed_string.is_regex {
            Matcher::Regex(parsed_string.text)
        } else if parsed_string.is_equivalent_to_asterisk() {
            Matcher::Any
        } else {
            Matcher::Text(parsed_string.anchor_start, parsed_string.text, parsed_string.anchor_end)
        })
    }
}

#[derive(Eq, PartialEq, Debug)]
pub enum ListItemTask {
    Selected,
    Unselected,
    Either,
    None,
}

#[derive(Eq, PartialEq, Debug)]
pub struct ListItemMatcher {
    ordered: bool,
    task: ListItemTask,
    matcher: Matcher,
}

#[derive(Eq, PartialEq, Debug)]
pub struct LinklikeMatcher {
    display_matcher: Matcher,
    url_matcher: Matcher,
}

#[derive(Eq, PartialEq, Debug)]
pub struct CodeBlockMatcher {
    language: Matcher,
    contents: Matcher,
}

#[derive(Eq, PartialEq, Debug)]
pub struct TableMatcher {
    column: Matcher,
    row: Matcher,
}

#[derive(Eq, PartialEq, Debug)]
pub enum Selector {
    Chain(Vec<Selector>),
    Section(Matcher),
    ListItem(ListItemMatcher),
    Link(LinklikeMatcher),
    Image(LinklikeMatcher),
    BlockQuote(Matcher),
    CodeBlockMatcher(CodeBlockMatcher),
    Html(Matcher),
    Paragraph(Matcher),
    Table(TableMatcher),
}

impl Selector {
    pub fn from_top_pairs(root: Pairs<Rule>) -> Result<Self, String> {
        let selector_chains = ByRule::new(Rule::selector_chain).find_all_in(root);
        let mut all_selectors = Vec::new();
        for chain in selector_chains {
            let selector_inners = ByRule::new(Rule::selector).find_all_in(chain.into_inner());
            for selector_pair in selector_inners {
                for specific_selector_pair in selector_pair.into_inner() {
                    let selector = Self::find_selectors(specific_selector_pair)?;
                    all_selectors.push(selector);
                }
            }
        }
        Ok(if all_selectors.len() == 1 {
            all_selectors.pop().unwrap()
        } else {
            Self::Chain(all_selectors)
        })
    }

    fn find_selectors(root: Pair<Rule>) -> Result<Self, String> {
        let (as_rule, children) = (root.as_rule(), root.into_inner());

        match as_rule {
            Rule::select_section => {
                let SectionResults { title } = SectionTraverser::traverse(children);
                let matcher = Matcher::take_from_option(title.take()?)?;
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
                let m = res.contents.take()?;
                let matcher = Matcher::take_from_option(m)?;
                Ok(Self::ListItem(ListItemMatcher { ordered, task, matcher }))
            }
            Rule::select_link => {
                let res = LinkTraverser::traverse(children);
                let display_matcher = Matcher::take_from_option(res.display_text.take()?)?;
                let url_matcher = Matcher::take_from_option(res.url_text.take()?)?;
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
                let matcher = Matcher::take_from_option(res.text.take()?)?;
                Ok(Self::BlockQuote(matcher))
            }
            Rule::select_code_block => {
                let res = CodeBlockTraverser::traverse(children);
                let language_matcher = Matcher::take_from_option(res.language.take()?)?;
                let contents_matcher = Matcher::take_from_option(res.text.take()?)?;
                Ok(Self::CodeBlockMatcher(CodeBlockMatcher {
                    language: language_matcher,
                    contents: contents_matcher,
                }))
            }
            Rule::select_html => {
                let res = HtmlTraverser::traverse(children);
                let matcher = Matcher::take_from_option(res.text.take()?)?;
                Ok(Self::Html(matcher))
            }
            Rule::select_paragraph => {
                let res = ParagraphTraverser::traverse(children);
                let matcher = Matcher::take_from_option(res.text.take()?)?;
                Ok(Self::Paragraph(matcher))
            }
            Rule::select_table => {
                let res = TableTraverser::traverse(children);
                let column_matcher = Matcher::take_from_option(res.column.take()?)?;
                let row_matcher = Matcher::take_from_option(res.row.take()?)?;
                Ok(Self::Table(TableMatcher {
                    column: column_matcher,
                    row: row_matcher,
                }))
            }
            unknown => {
                Err(format!("unexpected selector rule: {unknown:?}"))
                // TODO I think what I should do here is to recurse down. That way, I can just call this on the top pair
                //  and have it "just work" without needing to separately extract the near-the-top chain.
                //  I should get exactly one result, across all of the children.
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::query::query::Query;

    mod empty {
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
    }

    mod section {
        use super::*;

        #[test]
        fn section_no_matcher() {
            find_selector("#", Selector::Section(Matcher::Any));
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
                    matcher: Matcher::Any,
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
                    matcher: Matcher::Any,
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
                    matcher: Matcher::Any,
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
                    matcher: Matcher::Any,
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
                    matcher: Matcher::Any,
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
                    display_matcher: Matcher::Any,
                    url_matcher: Matcher::Any,
                }),
            )
        }

        #[test]
        fn link_with_display() {
            find_selector(
                "[hello]()",
                Selector::Link(LinklikeMatcher {
                    display_matcher: matcher_text(false, "hello", false),
                    url_matcher: Matcher::Any,
                }),
            )
        }

        #[test]
        fn link_with_url() {
            find_selector(
                "[](example.com)",
                Selector::Link(LinklikeMatcher {
                    display_matcher: Matcher::Any,
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
                    display_matcher: Matcher::Any,
                    url_matcher: Matcher::Any,
                }),
            )
        }

        #[test]
        fn image_with_alt() {
            find_selector(
                "![alt text]()",
                Selector::Image(LinklikeMatcher {
                    display_matcher: matcher_text(false, "alt text", false),
                    url_matcher: Matcher::Any,
                }),
            )
        }
    }

    mod block_quote {
        use super::*;

        #[test]
        fn block_quote_no_matcher() {
            find_selector(">", Selector::BlockQuote(Matcher::Any))
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
                Selector::CodeBlockMatcher(CodeBlockMatcher {
                    language: Matcher::Any,
                    contents: Matcher::Any,
                }),
            )
        }

        #[test]
        fn code_block_with_only_language() {
            find_selector(
                "```rust",
                Selector::CodeBlockMatcher(CodeBlockMatcher {
                    language: matcher_text(false, "rust", false),
                    contents: Matcher::Any,
                }),
            )
        }

        #[test]
        fn code_block_with_only_language_and_trailing_space() {
            find_selector(
                "```rust",
                Selector::CodeBlockMatcher(CodeBlockMatcher {
                    language: matcher_text(false, "rust", false),
                    contents: Matcher::Any,
                }),
            )
        }

        #[test]
        fn code_block_with_only_content() {
            find_selector(
                "``` fn main()",
                Selector::CodeBlockMatcher(CodeBlockMatcher {
                    language: Matcher::Any,
                    contents: matcher_text(false, "fn main()", false),
                }),
            )
        }

        #[test]
        fn code_block_with_both() {
            find_selector(
                "```rust fn main()",
                Selector::CodeBlockMatcher(CodeBlockMatcher {
                    language: matcher_text(false, "rust", false),
                    contents: matcher_text(false, "fn main()", false),
                }),
            )
        }
    }

    mod html {
        use super::*;
        use indoc::indoc;

        #[test]
        fn html_no_matcher() {
            find_selector("</>", Selector::Html(Matcher::Any))
        }

        #[test]
        fn html_with_text() {
            find_selector("</> '<div>'", Selector::Html(matcher_text(false, "<div>", false)))
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
                      = expected end of input or string"#},
            );
        }

        #[test]
        fn html_with_regex() {
            find_selector("</> /<div.*>/", Selector::Html(Matcher::Regex("<div.*>".to_string())))
        }
    }

    mod paragraph {
        use super::*;

        #[test]
        fn paragraph_no_matcher() {
            find_selector("P:", Selector::Paragraph(Matcher::Any))
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
                      |     ^---
                      |
                      = expected explicit "*" or string"#},
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
                Selector::Table(TableMatcher {
                    column: Matcher::Any,
                    row: Matcher::Any,
                }),
            )
        }

        #[test]
        fn table_with_column() {
            find_selector(
                ":-: Header :-:",
                Selector::Table(TableMatcher {
                    column: matcher_text(false, "Header", false),
                    row: Matcher::Any,
                }),
            )
        }

        #[test]
        fn table_with_row() {
            find_selector(
                ":-: * :-: Data",
                Selector::Table(TableMatcher {
                    column: Matcher::Any,
                    row: matcher_text(false, "Data", false),
                }),
            )
        }

        #[test]
        fn table_with_both() {
            find_selector(
                ":-: Header :-: Data",
                Selector::Table(TableMatcher {
                    column: matcher_text(false, "Header", false),
                    row: matcher_text(false, "Data", false),
                }),
            )
        }
    }

    #[test]
    fn todo() {
        todo!("more of these. weird chaining, etc");
    }

    fn find_empty_chain(query_text: &str) {
        find_selector(query_text, Selector::Chain(vec![]));
    }

    fn find_selector(query_text: &str, expect: Selector) {
        let pairs = Query::parse(query_text);
        let pairs = match pairs {
            Ok(pairs) => pairs,
            Err(err) => {
                panic!("{err}");
            }
        };

        let result = Selector::from_top_pairs(pairs);
        assert_eq!(result, Ok(expect));
    }

    fn expect_parse_error(query_text: &str, expect: &str) {
        let pairs = Query::parse(query_text);
        match pairs {
            Ok(pairs) => panic! {"{pairs}"},
            Err(err) => {
                let err_msg = format!("{err}");
                assert_eq!(err_msg, expect);
            }
        }
    }

    fn matcher_text(anchor_start: bool, text: &str, anchor_end: bool) -> Matcher {
        Matcher::Text(anchor_start, text.to_string(), anchor_end)
    }
}
