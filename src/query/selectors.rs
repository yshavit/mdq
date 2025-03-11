use crate::query::query::{Rule, RuleTree};

#[derive(Eq, PartialEq, Debug)]
pub enum Matcher {
    Text(bool, String, bool),
    Regex(String),
    Any,
}

impl Matcher {
    fn take_from_tree(source: &mut Vec<RuleTree>, by_tag: Option<&str>) -> Result<Matcher, String> {
        let extracted = RuleTree::extract_by_rule(source, by_tag);

        fn build(tree: RuleTree, to: &mut Vec<Matcher>) {
            match tree {
                RuleTree::Node(_, children) => {
                    children.into_iter().for_each(|child| build(child, to));
                }
                RuleTree::Text(parsed_string) => {
                    let matcher = if parsed_string.is_regex {
                        Matcher::Regex(parsed_string.text)
                    } else {
                        Matcher::Text(parsed_string.anchor_start, parsed_string.text, parsed_string.anchor_end)
                    };
                    to.push(matcher);
                }
                _ => {}
            }
        }

        let mut matchers = Vec::new();
        for child in extracted {
            build(child, &mut matchers);
        }

        let Some(first) = matchers.pop() else {
            return Ok(Matcher::Any);
        };
        if matchers.is_empty() {
            Ok(first)
        } else {
            Err(format!(
                "expected exactly one matcher under {under_rule:?}, but found {}",
                matchers.len()
            ))
        }
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
    pub fn from_top(mut roots: Vec<RuleTree>) -> Result<Option<Self>, String> {
        let selector_rules = RuleTree::extract_by_rule(&mut roots, Rule::selector);
        Self::one_from_trees(selector_rules)
    }

    fn one_from_trees(roots: Vec<RuleTree>) -> Result<Option<Self>, String> {
        let mut result = None;
        for child in roots {
            let from_child = Self::one_from_tree(child)?;
            result = match (result, from_child) {
                (None, None) => None,
                (Some(_), Some(_)) => return Err("multiple selectors found".to_string()),
                (None, s @ Some(_)) => s,
                (s @ Some(_), None) => s,
            }
        }
        Ok(result)
    }

    fn one_from_tree(root: RuleTree) -> Result<Option<Self>, String> {
        fn ok(value: Selector) -> Result<Option<Selector>, String> {
            Ok(Some(value))
        }

        match root {
            RuleTree::Node(rule, mut children) => match rule {
                Rule::selector => Selector::one_from_trees(children),
                Rule::select_section => {
                    let matcher = Matcher::take_from_tree(&mut children, Rule::string_to_pipe)?;
                    ok(Selector::Section(matcher))
                }
                Rule::select_list_item => {
                    let ordered = RuleTree::find_any(&children, [Rule::list_ordered]).is_some();
                    let task = match RuleTree::find_any(
                        &children,
                        [Rule::task_checked, Rule::task_unchecked, Rule::task_either],
                    ) {
                        Some(Rule::task_checked) => ListItemTask::Selected,
                        Some(Rule::task_unchecked) => ListItemTask::Unselected,
                        Some(Rule::task_either) => ListItemTask::Either,
                        _ => ListItemTask::None,
                    };
                    let matcher = Matcher::take_from_tree(&mut children, Rule::string_to_pipe)?;
                    ok(Selector::ListItem(ListItemMatcher { ordered, task, matcher }))
                }
                Rule::select_link => {
                    let matcher = LinklikeMatcher {
                        display_matcher: Matcher::take_from_tree(&mut children, Rule::string_to_bracket)?,
                        url_matcher: Matcher::take_from_tree(&mut children, Rule::string_to_paren)?,
                    };
                    ok(if RuleTree::contains_rule(&children, Rule::image_start) {
                        Selector::Image(matcher)
                    } else {
                        Selector::Link(matcher)
                    })
                }
                Rule::select_block_quote => {
                    let matcher = Matcher::take_from_tree(&mut children, Rule::string_to_pipe)?;
                    ok(Selector::BlockQuote(matcher))
                }
                Rule::select_code_block => {
                    let language = Matcher::take_from_tree(&mut children, Rule::string_to_space)?;
                    let contents = Matcher::take_from_tree(&mut children, Rule::string_to_pipe)?;
                    ok(Selector::CodeBlockMatcher(CodeBlockMatcher { language, contents }))
                }
                Rule::select_html => {
                    let matcher = Matcher::take_from_tree(&mut children, Rule::string_to_pipe)?;
                    ok(Selector::Html(matcher))
                }
                Rule::select_paragraph => {
                    let matcher = Matcher::take_from_tree(&mut children, Rule::string_to_pipe)?;
                    ok(Selector::Paragraph(matcher))
                }
                Rule::select_table => {
                    let column = Matcher::take_from_tree(&mut children, Rule::string_to_colon)?;
                    let row = Matcher::take_from_tree(&mut children, Rule::string_to_pipe)?;
                    ok(Selector::Table(TableMatcher { column, row }))
                }
                unknown => Err(format!("unrecognized element: {unknown:?}")),
            },
            RuleTree::Leaf(Rule::selector, "|") => Ok(None),
            unknown => Err(format!("unrecognized element: {unknown:?}")),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod empty {
        use super::*;

        #[test]
        fn totally_empty() {
            find_none("");
        }

        #[test]
        fn only_spaces() {
            find_none("");
        }

        #[test]
        fn only_one_pipe() {
            find_none("|");
        }

        #[test]
        fn only_multiple_pipes() {
            find_none("|| |");
        }
    }

    mod section {
        use super::*;

        #[test]
        fn section_no_matcher() {
            find_selector("#", Selector::Section(Matcher::Any))
        }

        #[test]
        fn section_with_matcher() {
            find_selector("# foo", Selector::Section(matcher_text(false, "foo", false)))
        }
    }

    fn find_none(query_text: &str) {
        let rule_tree = RuleTree::parse(query_text).unwrap();
        let from_parse_tree = Selector::from_top(rule_tree).unwrap();
        assert_eq!(from_parse_tree, None);
    }

    fn find_selector(query_text: &str, expect: Selector) {
        let rule_tree = RuleTree::parse(query_text).unwrap();
        let from_parse_tree = Selector::from_top(rule_tree).unwrap();
        assert_eq!(from_parse_tree, Some(expect));
    }

    fn matcher_text(anchor_start: bool, text: &str, anchor_end: bool) -> Matcher {
        Matcher::Text(anchor_start, text.to_string(), anchor_end)
    }
}
