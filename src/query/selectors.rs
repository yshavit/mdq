use crate::query::query::{
    ByRule, ListItemTraverser, PairMatcher, ParsedString, QueryPairs, Rule, SectionResults, SectionTraverser,
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
                let as_debug = format!("{children:?}");
                eprintln!("{as_debug}");
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
                Ok(Selector::ListItem(ListItemMatcher { ordered, task, matcher }))
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
    use pest::Parser;

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
        use crate::query::query::Rule::top;

        #[test]
        fn section_no_matcher() {
            find_selector("#", Selector::Section(Matcher::Any))
        }

        #[test]
        fn section_with_matcher() {
            let pairs = QueryPairs::parse(top, "# foo").unwrap();
            let result = Selector::from_top_pairs(pairs);
            assert_eq!(result, Ok(Selector::Section(matcher_text(false, "foo", false))));
        }
    }

    mod list_item {
        use super::*;

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

    #[test]
    fn todo() {
        todo!("more of these. weird chaining, etc");
    }

    fn find_empty_chain(query_text: &str) {
        find_selector(query_text, Selector::Chain(vec![]));
    }

    fn find_selector(query_text: &str, expect: Selector) {
        let pairs = QueryPairs::parse(Rule::top, query_text).unwrap();
        let result = Selector::from_top_pairs(pairs);
        assert_eq!(result, Ok(expect));
    }

    fn matcher_text(anchor_start: bool, text: &str, anchor_end: bool) -> Matcher {
        Matcher::Text(anchor_start, text.to_string(), anchor_end)
    }
}
