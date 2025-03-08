use pest::iterators::{Pair, Pairs};
use pest_derive::Parser;
use std::fmt::{Debug, Formatter, Write};

#[derive(Parser)]
#[grammar = "query/grammar.pest"] // relative to src
struct QueryPairs;

#[derive(Eq, PartialEq)]
enum RuleTree<'a> {
    Node(Rule, Vec<RuleTree<'a>>),
    Leaf(Rule, &'a str),
    Text(ParsedString),
    Error(String),
}

#[derive(Eq, PartialEq)]
struct ParsedString {
    text: String,
    anchor_start: bool,
    anchor_end: bool,
}

impl Debug for ParsedString {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.anchor_start {
            f.write_char('^')?;
        }
        write!(f, "{:?}", self.text)?;
        if self.anchor_end {
            f.write_char('$')?;
        }
        Ok(())
    }
}

impl<'a> RuleTree<'a> {
    fn get_string(pairs: Pairs<Rule>) -> Result<ParsedString, String> {
        let mut s = ParsedString {
            text: String::with_capacity(pairs.as_str().len()),
            anchor_start: false,
            anchor_end: false,
        };
        Self::build_string(&mut s, pairs).map(|_| s)
    }

    fn build_string(parsed: &mut ParsedString, pairs: Pairs<Rule>) -> Result<(), String> {
        for pair in pairs {
            match pair.as_rule() {
                Rule::literal_char => {
                    parsed.text.push_str(pair.as_str());
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
                        parsed.text.push(result);
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
                    parsed.text.push(ch);
                }
                Rule::anchor_start => {
                    parsed.anchor_start = true;
                }
                Rule::anchor_end => {
                    parsed.anchor_end = true;
                }
                _ => {
                    Self::build_string(parsed, pair.into_inner())?;
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
            check_parse(Rule::string, "'hello'\"X", parsed_text(false, "hello", false), "\"X");
        }

        #[test]
        fn double_quoted_string() {
            check_parse(Rule::string, "\"hello\"'X", parsed_text(false, "hello", false), "'X");
        }

        #[test]
        fn quoted_string_newline() {
            check_parse(
                Rule::string,
                r"'hello\nworld'",
                parsed_text(false, "hello\nworld", false),
                "",
            );
        }

        #[test]
        fn quoted_string_snowman() {
            check_parse(
                Rule::string,
                r"'hello\u{2603}world'",
                parsed_text(false, "hello☃world", false),
                "",
            );
        }

        #[test]
        fn unquoted_string_to_pipe() {
            check_parse(
                Rule::string,
                r"hello'\n | world | multiple | pipes",
                parsed_text(false, r"hello'\n", false),
                "| world | multiple | pipes",
            );
        }

        #[test]
        fn unquoted_no_end_pipe() {
            check_parse(
                Rule::string,
                r"hello world ",
                parsed_text(false, r"hello world ", false),
                "",
            );
        }

        #[test]
        fn anchors() {
            todo!();
        }

        #[test]
        fn anchors_double_quoted_no_space() {
            check_parse(Rule::string, "^\"hello\"$", parsed_text(true, "hello", true), "");
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
        })
    }
}
