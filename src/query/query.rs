use crate::vec_utils::vec_extract_if_to;
use pest::iterators::{Pair, Pairs};
use pest_derive::Parser;
use std::borrow::Cow;
use std::fmt::{Debug, Formatter, Write};

#[derive(Parser)]
#[grammar = "query/grammar.pest"] // relative to src
struct QueryPairs;

#[derive(Eq, PartialEq)]
pub enum RuleTree<'a> {
    Node(Rule, Vec<RuleTree<'a>>),
    Leaf(Rule, &'a str),
    Text(ParsedString),
    Error(String),
}

struct ExtractedRuleTrees<'a> {
    extracted: Vec<RuleTree<'a>>,
    remaining: Option<RuleTree<'a>>,
}

impl<'a> RuleTree<'a> {
    /// Extracts all subtrees that are Nodes or Leafs matching the given rule. For each one that matches, that whole
    /// RuleTree will be removed from the source and added to the resulting vec. This recurses down the source tree
    /// as needed. The source tree's remaining elements may be reordered.
    pub fn extract_by_rule(source: &mut Vec<Self>, rule: Rule) -> Vec<Self> {
        fn build_results<'a>(roots: &mut Vec<RuleTree<'a>>, look_for: Rule, to: &mut Vec<RuleTree<'a>>) {
            // extract children that match the rule, and then descend to whoever's left
            vec_extract_if_to(roots, |child| child.is_rule(look_for), to);
            for child in roots {
                // only Nodes need to recurse
                if let RuleTree::Node(_, children) = child {
                    build_results(children, look_for, to);
                }
            }
        }
        let mut results = Vec::new();
        build_results(source, rule, &mut results);
        results
    }

    /// Finds any instance of the given rules in the tree. If multiple match, it's undefined which gets returned. If
    /// none match, returns None.
    pub fn find_any<const N: usize>(source: &Vec<Self>, rules: [Rule; N]) -> Option<Rule> {
        for item in source {
            match item {
                RuleTree::Node(my_rule, children) => {
                    if rules.contains(my_rule) {
                        return Some(*my_rule);
                    } else if let result @ Some(_) = Self::find_any(children, rules) {
                        return result;
                    }
                }
                RuleTree::Leaf(my_rule, _) => {
                    if rules.contains(my_rule) {
                        return Some(*my_rule);
                    }
                }
                _ => {}
            }
        }
        None
    }

    pub fn contains_rule(source: &Vec<Self>, rule: Rule) -> bool {
        for item in source {
            match item {
                RuleTree::Node(my_rule, children) => {
                    if *my_rule == rule {
                        return true;
                    } else if Self::contains_rule(children, rule) {
                        return true;
                    }
                }
                &RuleTree::Leaf(my_rule, _) => {
                    return my_rule == rule;
                }
                _ => {}
            }
        }
        false
    }

    fn is_rule(&self, rule: Rule) -> bool {
        match self {
            &RuleTree::Node(self_rule, _) | &RuleTree::Leaf(self_rule, _) => self_rule == rule,
            _ => false,
        }
    }
}

#[derive(Eq, PartialEq)]
pub struct ParsedString {
    pub text: String,
    pub anchor_start: bool,
    pub anchor_end: bool,
    pub is_regex: bool,
}

impl Debug for ParsedString {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.is_regex {
            f.write_char('/')?;
            let escaped = if self.text.contains("/") {
                Cow::Owned(self.text.replace("/", "//"))
            } else {
                Cow::Borrowed(&self.text)
            };
            f.write_str(&escaped)?;
            f.write_char('/')?;
        } else {
            if self.anchor_start {
                f.write_char('^')?;
            }
            write!(f, "{:?}", self.text)?;
            if self.anchor_end {
                f.write_char('$')?;
            }
        }
        Ok(())
    }
}

impl ParsedString {
    fn get_string(pairs: Pairs<Rule>) -> Result<Self, String> {
        let mut s = Self {
            text: String::with_capacity(pairs.as_str().len()),
            anchor_start: false,
            anchor_end: false,
            is_regex: false,
        };
        s.build_string(pairs).map(|_| s)
    }

    fn build_string(&mut self, pairs: Pairs<Rule>) -> Result<(), String> {
        for pair in pairs {
            match pair.as_rule() {
                Rule::literal_char => {
                    self.text.push_str(pair.as_str());
                }
                Rule::escaped_char => {
                    // we'll iterate, but we should really only have one
                    for input_ch in pair.as_str().chars() {
                        let result = match input_ch {
                            result @ ('"' | '\'' | '\\') => result,
                            '`' => '\'',
                            'n' => '\n',
                            'r' => '\r',
                            't' => '\t',
                            err => {
                                return Err(format!("invalid escape char: {err:?}"));
                            }
                        };
                        self.text.push(result);
                    }
                }
                Rule::unicode_seq => {
                    let seq = pair.as_str();
                    let Ok(code_point) = u32::from_str_radix(pair.as_str(), 16) else {
                        return Err(format!("invalid unicode sequence: {seq}"));
                    };
                    let Some(ch) = char::from_u32(code_point) else {
                        return Err(format!("invalid unicode sequence: {seq}"));
                    };
                    self.text.push(ch);
                }
                Rule::anchor_start => {
                    self.anchor_start = true;
                }
                Rule::anchor_end => {
                    self.anchor_end = true;
                }
                Rule::unquoted_string_to_pipe
                | Rule::unquoted_string_to_paren
                | Rule::unquoted_string_to_bracket
                | Rule::unquoted_string_to_space
                | Rule::unquoted_string_to_colon => {
                    self.text.push_str(pair.as_str().trim_end());
                }
                Rule::regex => {
                    self.is_regex = true;
                    Self::build_string(self, pair.into_inner())?;
                }
                Rule::regex_normal_char => {
                    self.text.push_str(pair.as_str());
                }
                Rule::regex_escaped_slash => {
                    self.text.push('/');
                }
                _ => {
                    Self::build_string(self, pair.into_inner())?;
                }
            }
        }
        Ok(())
    }
}

impl<'a> Into<RuleTree<'a>> for Pair<'a, Rule> {
    fn into(self) -> RuleTree<'a> {
        let rule = self.as_rule();
        match rule {
            Rule::string_to_pipe
            | Rule::string_to_paren
            | Rule::string_to_bracket
            | Rule::string_to_space
            | Rule::string_to_colon => match ParsedString::get_string(self.into_inner()) {
                Ok(text) => RuleTree::Text(text),
                Err(msg) => RuleTree::Error(msg),
            },
            _ => {
                let text = self.as_str();
                let inners = self.into_inner();
                match inners.len() {
                    0 => RuleTree::Leaf(rule, text),
                    _ => RuleTree::Node(rule, inners.into_iter().map(|p| p.into()).collect()),
                }
            }
        }
    }
}

impl<'a> Debug for RuleTree<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        fn pretty_print(indentation: usize, item: &RuleTree, f: &mut Formatter<'_>) -> std::fmt::Result {
            for _ in 0..indentation {
                f.write_str("  ")?;
            }
            match item {
                RuleTree::Node(rule, children) => {
                    write!(f, "{rule:?}:\n")?;
                    for child in children {
                        pretty_print(indentation + 1, child, f)?;
                    }
                    Ok(())
                }
                RuleTree::Leaf(rule, text) => match *text {
                    "" => write!(f, "{rule:?}: <empty>\n"),
                    _ => write!(f, "{rule:?}: {text}\n"),
                },
                RuleTree::Text(text) => {
                    write!(f, "{text:?}")
                }
                RuleTree::Error(text) => {
                    write!(f, "ERROR: {text:?}")
                }
            }
        }
        if f.alternate() {
            pretty_print(0, self, f)
        } else {
            match self {
                RuleTree::Node(rule, children) => {
                    write!(f, "Branch({rule:?}, {children:?})")
                }
                RuleTree::Leaf(rule, text) => {
                    write!(f, "Leaf({rule:?}, {text:?})")
                }
                RuleTree::Text(text) => {
                    write!(f, "Text({text:?})")
                }
                RuleTree::Error(text) => {
                    write!(f, "Error({text:?})")
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pest::Parser;
    use pretty_assertions::assert_eq;

    mod strings {
        use super::*;

        #[test]
        fn single_quoted_string() {
            check_parse(
                Rule::string_to_pipe,
                "'hello'\"X",
                parsed_text(false, "hello", false),
                "\"X",
            );
        }

        #[test]
        fn double_quoted_string() {
            check_parse(
                Rule::string_to_pipe,
                "\"hello\"'X",
                parsed_text(false, "hello", false),
                "'X",
            );
        }

        #[test]
        fn quoted_string_newline() {
            check_parse(
                Rule::string_to_pipe,
                r"'hello\nworld'",
                parsed_text(false, "hello\nworld", false),
                "",
            );
        }

        #[test]
        fn quoted_string_snowman() {
            check_parse(
                Rule::string_to_pipe,
                r"'hello\u{2603}world'",
                parsed_text(false, "hello☃world", false),
                "",
            );
        }

        #[test]
        fn unquoted_string_to_pipe() {
            check_parse(
                Rule::string_to_pipe,
                r"hello'\n | world | multiple | pipes",
                parsed_text(false, r"hello'\n", false),
                "| world | multiple | pipes",
            );
        }

        #[test]
        fn unquoted_no_end_pipe() {
            check_parse(
                Rule::string_to_pipe,
                r"hello world   ",
                parsed_text(false, r"hello world", false),
                "",
            );
        }

        #[test]
        fn unquoted_string_to_paren() {
            check_parse(
                Rule::string_to_paren,
                r"hello'\n ) world ) multiple ) parens",
                parsed_text(false, r"hello'\n", false),
                ") world ) multiple ) parens",
            );
        }

        #[test]
        fn unquoted_no_end_paren() {
            check_parse(
                Rule::string_to_paren,
                r"hello world   ",
                parsed_text(false, r"hello world", false),
                "",
            );
        }

        #[test]
        fn unquoted_string_to_bracket() {
            check_parse(
                Rule::string_to_bracket,
                r"hello'\n ] world ] multiple ] brackets",
                parsed_text(false, r"hello'\n", false),
                "] world ] multiple ] brackets",
            );
        }

        #[test]
        fn unquoted_no_end_bracket() {
            check_parse(
                Rule::string_to_bracket,
                r"hello world   ",
                parsed_text(false, r"hello world", false),
                "",
            );
        }

        #[test]
        fn anchors_double_quoted_no_space() {
            check_parse(
                Rule::string_to_pipe,
                "^\"hello\"$",
                parsed_text(true, "hello", true),
                "",
            );
        }

        #[test]
        fn anchors_single_quoted_with_space() {
            check_parse(
                Rule::string_to_pipe,
                "^ 'hello' $",
                parsed_text(true, "hello", true),
                "",
            );
        }

        #[test]
        fn anchors_unquoted_to_pipe_with_space() {
            check_parse(
                Rule::string_to_pipe,
                "^ hello $ there",
                parsed_text(true, "hello", true),
                " there",
            );
        }

        #[test]
        fn anchors_unquoted_to_pipe_no_space() {
            check_parse(
                Rule::string_to_pipe,
                "^hello$ there",
                parsed_text(true, "hello", true),
                " there",
            );
        }
    }

    mod regexes {
        use super::*;

        #[test]
        fn normal_regex() {
            check_parse(Rule::string_to_pipe, "/hello there$/", parsed_regex("hello there$"), "");
        }

        #[test]
        fn regex_with_escaped_slash() {
            check_parse(
                Rule::string_to_pipe,
                r"/hello\/there/",
                parsed_regex(r"hello/there"),
                "",
            );
        }
    }

    mod select_section {
        use super::*;

        #[test]
        fn with_matcher() {
            check_parse(
                Rule::select_section,
                r"# foo",
                RuleTree::Node(
                    Rule::select_section,
                    vec![
                        RuleTree::Leaf(Rule::section_start, "# "),
                        parsed_text(false, "foo", false),
                    ],
                ),
                "",
            );
        }

        #[test]
        fn no_matcher() {
            check_parse(
                Rule::select_section,
                r"#",
                RuleTree::Node(Rule::select_section, vec![RuleTree::Leaf(Rule::section_start, "#")]),
                "",
            );
        }
    }

    mod select_lists {
        use crate::query::query::tests::{check_parse, parsed_text};
        use crate::query::query::{Rule, RuleTree};

        #[test]
        fn list_item_no_matcher() {
            check_parse(
                Rule::select_list_item,
                r"-",
                RuleTree::Node(
                    Rule::select_list_item,
                    vec![RuleTree::Node(
                        Rule::list_start,
                        vec![RuleTree::Node(Rule::list_unordered, vec![])],
                    )],
                ),
                "",
            );
        }

        #[test]
        fn list_item_with_matcher() {
            check_parse(
                Rule::select_list_item,
                r"- foo",
                RuleTree::Node(
                    Rule::select_list_item,
                    vec![
                        RuleTree::Node(Rule::list_start, vec![RuleTree::Node(Rule::list_unordered, vec![])]),
                        parsed_text(false, "foo", false),
                    ],
                ),
                "",
            );
        }
    }

    mod chaining {
        #[test]
        fn two_chained_together() {
            todo!()
        }

        #[test]
        fn empty_rule_in_middle() {
            // "# foo | | # bar"
            todo!()
        }

        #[test]
        fn empty_rule_at_start() {
            // "| # foo"
            todo!()
        }

        #[test]
        fn empty_rule_at_end() {
            // "# foo |"
            todo!()
        }
    }

    fn check_parse(rule: Rule, input: &str, expect: RuleTree, remaining: &str) {
        let pairs = QueryPairs::parse(rule, input).unwrap();
        let consumed = pairs.as_str();
        let rule_tree: Vec<RuleTree> = pairs.into_iter().map(|p| p.into()).collect();
        assert_eq!(rule_tree, vec![expect]);
        assert_eq!(&input[consumed.len()..], remaining);
    }

    fn parsed_text(anchor_start: bool, text: &str, anchor_end: bool) -> RuleTree {
        RuleTree::Text(ParsedString {
            anchor_start,
            text: text.to_string(),
            anchor_end,
            is_regex: false,
        })
    }

    fn parsed_regex(text: &str) -> RuleTree {
        RuleTree::Text(ParsedString {
            anchor_start: false,
            text: text.to_string(),
            anchor_end: false,
            is_regex: true,
        })
    }
}
