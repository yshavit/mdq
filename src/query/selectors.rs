use crate::query::query::{Rule, RuleTree};

#[derive(Eq, PartialEq, Debug)]
pub enum Matcher {
    Text(bool, String, bool),
    Regex(String),
    Any,
}

impl Matcher {
    fn take_from_tree(source: &mut Vec<RuleTree>, under_rule: Rule) -> Result<Matcher, String> {
        let extracted = RuleTree::extract_by_rule(source, under_rule);

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

#[derive(Eq, PartialEq, Debug)]
enum FromTreeResult<'a> {
    Good(Selector),
    Error(String),
    None(RuleTree<'a>),
}

impl From<Result<Selector, String>> for FromTreeResult<'_> {
    fn from(value: Result<Selector, String>) -> Self {
        match value {
            Ok(good) => Self::Good(good),
            Err(msg) => Self::Error(msg),
        }
    }
}

impl Selector {
    fn one_from_tree(root: RuleTree) -> FromTreeResult {
        /// small helper that lets us use "?;" syntax
        fn make_result<'a, F: FnOnce() -> Result<Selector, String>>(f: F) -> FromTreeResult<'a> {
            f().into()
        }

        match root {
            RuleTree::Node(rule, mut children) => match rule {
                Rule::select_section => make_result(|| {
                    let matcher = Matcher::take_from_tree(&mut children, Rule::string_to_pipe)?;
                    Ok(Selector::Section(matcher))
                }),
                Rule::select_list_item => make_result(|| {
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
                    Ok(Selector::ListItem(ListItemMatcher { ordered, task, matcher }))
                }),
                Rule::select_link => make_result(|| {
                    let matcher = LinklikeMatcher {
                        display_matcher: Matcher::take_from_tree(&mut children, Rule::string_to_bracket)?,
                        url_matcher: Matcher::take_from_tree(&mut children, Rule::string_to_paren)?,
                    };
                    Ok(if RuleTree::contains_rule(&children, Rule::image_start) {
                        Selector::Image(matcher)
                    } else {
                        Selector::Link(matcher)
                    })
                }),
                Rule::select_block_quote => make_result(|| {
                    let matcher = Matcher::take_from_tree(&mut children, Rule::string_to_pipe)?;
                    Ok(Selector::BlockQuote(matcher))
                }),
                Rule::select_code_block => make_result(|| {
                    let language = Matcher::take_from_tree(&mut children, Rule::string_to_space)?;
                    let contents = Matcher::take_from_tree(&mut children, Rule::string_to_pipe)?;
                    Ok(Selector::CodeBlockMatcher(CodeBlockMatcher { language, contents }))
                }),
                Rule::select_html => make_result(|| {
                    let matcher = Matcher::take_from_tree(&mut children, Rule::string_to_pipe)?;
                    Ok(Selector::Html(matcher))
                }),
                Rule::select_paragraph => make_result(|| {
                    let matcher = Matcher::take_from_tree(&mut children, Rule::string_to_pipe)?;
                    Ok(Selector::Paragraph(matcher))
                }),
                Rule::select_table => make_result(|| {
                    let column = Matcher::take_from_tree(&mut children, Rule::string_to_colon)?;
                    let row = Matcher::take_from_tree(&mut children, Rule::string_to_pipe)?;
                    Ok(Selector::Table(TableMatcher { column, row }))
                }),
                // TODO do I really want None here, or just to recurse down?
                //  If I don't want None, maybe I can just remove it, and use a normal Result?
                _ => FromTreeResult::None(RuleTree::Node(rule, children)),
            },
            unknown => FromTreeResult::None(unknown),
        }
    }
}
