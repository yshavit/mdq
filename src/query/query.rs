use pest::iterators::{Pair, Pairs};
use pest_derive::Parser;
use std::fmt::{Debug, Formatter};

#[derive(Parser)]
#[grammar = "query/grammar.pest"] // relative to src
struct QueryPairs;

#[derive(Eq, PartialEq)]
enum RuleTree<'a> {
    Node(Rule, Vec<RuleTree<'a>>),
    Leaf(Rule, &'a str),
    Text(String),
    Error(String),
}

impl<'a> RuleTree<'a> {
    fn get_string(pairs: Pairs<Rule>) -> Result<String, String> {
        let mut s = String::with_capacity(pairs.as_str().len());
        Self::build_string(&mut s, pairs).map(|_| s)
    }

    fn build_string(to: &mut String, pairs: Pairs<Rule>) -> Result<(), String> {
        for pair in pairs {
            match pair.as_rule() {
                Rule::literal_char => {
                    to.push_str(pair.as_str());
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
                        to.push(result);
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
                    to.push(ch);
                }
                _ => {
                    Self::build_string(to, pair.into_inner())?;
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
            Rule::string => match RuleTree::get_string(self.into_inner()) {
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
            check_parse(Rule::string, "'hello'\"X", RuleTree::Text("hello".to_string()), "\"X");
        }

        #[test]
        fn double_quoted_string() {
            check_parse(Rule::string, "\"hello\"'X", RuleTree::Text("hello".to_string()), "'X");
        }

        #[test]
        fn quoted_string_newline() {
            check_parse(
                Rule::string,
                r"'hello\nworld'",
                RuleTree::Text("hello\nworld".to_string()),
                "",
            );
        }

        #[test]
        fn quoted_string_snowman() {
            check_parse(
                Rule::string,
                r"'hello\u{2603}world'",
                RuleTree::Text("hello☃world".to_string()),
                "",
            );
        }

        #[test]
        fn unquoted_string_to_pipe() {
            check_parse(
                Rule::string,
                r"hello'\n | world | multiple | pipes",
                RuleTree::Text(r"hello'\n".to_string()),
                "| world | multiple | pipes",
            );
        }

        #[test]
        fn unquoted_no_end_pipe() {
            check_parse(
                Rule::string,
                r"hello world ",
                RuleTree::Text(r"hello world ".to_string()),
                "",
            );
        }

        #[test]
        fn anchors() {
            todo!();
        }
    }

    fn check_parse(rule: Rule, input: &str, expect: RuleTree, remaining: &str) {
        let pairs = QueryPairs::parse(rule, input).unwrap();
        let consumed = pairs.as_str();
        let rule_tree: Vec<RuleTree> = pairs.into_iter().map(|p| p.into()).collect();
        assert_eq!(rule_tree, vec![expect]);
        assert_eq!(&input[consumed.len()..], remaining);
    }
}
