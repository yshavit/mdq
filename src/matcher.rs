use crate::fmt_str::inlines_to_plain_string;
use crate::query::query::Rule::regex;
use crate::query::selectors::Matcher;
use crate::tree::{Inline, MdElem};
use regex::Regex;
use std::borrow::Borrow;

#[derive(Debug)]
pub struct StringMatcher {
    re: Regex,
}

impl PartialEq for StringMatcher {
    fn eq(&self, other: &Self) -> bool {
        self.re.as_str() == other.re.as_str()
    }
}

impl StringMatcher {
    pub fn matches(&self, haystack: &str) -> bool {
        self.re.is_match(haystack)
    }

    pub fn matches_inlines<I: Borrow<Inline>>(&self, haystack: &[I]) -> bool {
        self.matches(&inlines_to_plain_string(haystack))
    }

    pub fn matches_any<N: Borrow<MdElem>>(&self, haystacks: &[N]) -> bool {
        haystacks.iter().any(|node| self.matches_node(node.borrow()))
    }

    fn matches_node(&self, node: &MdElem) -> bool {
        match node {
            MdElem::Paragraph(p) => self.matches_inlines(&p.body),
            MdElem::ThematicBreak | MdElem::CodeBlock(_) => false,
            MdElem::Table(table) => {
                for row in &table.rows {
                    for cell in row {
                        if self.matches_inlines(cell) {
                            return true;
                        }
                    }
                }
                false
            }
            MdElem::List(list) => list.items.iter().any(|li| self.matches_any(&li.item)),
            MdElem::BlockQuote(block) => self.matches_any(&block.body),
            MdElem::Section(section) => {
                if self.matches_inlines(&section.title) {
                    return true;
                }
                self.matches_any(&section.body)
            }
            MdElem::Html(html) => self.matches(html),
            MdElem::Inline(inline) => self.matches_inlines(&[inline]),
        }
    }
    
    pub fn for_string(s: Matcher) -> Self {
        todo!() use SubstringToRegex
    }

    pub fn any() -> Self {
        Self {
            re: Regex::new(".*").expect("internal error"),
        }
    }

    fn empty() -> Self {
        Self {
            re: Regex::new("^$").expect("internal error"),
        }
    }

    fn regex(re: Regex) -> Self {
        Self { re }
    }
}

impl From<Matcher> for StringMatcher {
    fn from(value: Matcher) -> Self {
        match value {
            Matcher::Text(start_anchor, text, end_anchor) => todo!(),
            Matcher::Regex(re) => todo!(),
            Matcher::Any => todo!(),
        }
    }
}

struct SubstringToRegex {
    look_for: String,
    case_sensitive: bool,
    anchor_start: bool,
    anchor_end: bool,
}

impl SubstringToRegex {
    fn to_string_matcher(&self) -> StringMatcher {
        let mut pattern = String::with_capacity(self.look_for.len() + 10); // +10 for modifiers, escapes, etc
        if !self.case_sensitive && !self.look_for.is_empty() {
            // (is_empty isn't necessary, just makes for a cleaner regex)
            pattern.push_str("(?i)");
        }
        if self.anchor_start {
            pattern.push('^');
        }
        pattern.push_str(&regex::escape(&self.look_for));
        if self.anchor_end {
            pattern.push('$');
        }

        let re = Regex::new(&pattern).expect("internal error");
        StringMatcher { re }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use indoc::indoc;
    use std::str::FromStr;

    #[test]
    fn bareword() {
        parse_and_check("hello", re_insensitive("hello"), "");
        parse_and_check("hello ", re_insensitive("hello"), "");
        parse_and_check("hello / goodbye", re_insensitive("hello / goodbye"), "");
        parse_and_check("hello| goodbye", re_insensitive("hello"), "| goodbye");
        parse_and_check("hello | goodbye", re_insensitive("hello"), "| goodbye");
        parse_and_check_with(']', "foo] rest", re_insensitive("foo"), "] rest");
    }

    #[test]
    fn bareword_anchor_start() {
        let m = parse_and_check("^ hello |after", re_insensitive("^hello"), "|after");
        assert_eq!(false, m.matches("pre hello"));
        assert_eq!(true, m.matches("hello"));
        assert_eq!(true, m.matches("hello post"));
    }

    #[test]
    fn bareword_anchor_end() {
        let m = parse_and_check(" hello $ |after", re_insensitive("hello$"), " |after");
        assert_eq!(true, m.matches("pre hello"));
        assert_eq!(true, m.matches("hello"));
        assert_eq!(false, m.matches("hello post"));
    }

    #[test]
    fn only_starting_anchor() {
        expect_err(
            "^ |",
            ParseErrorReason::InvalidSyntax(ERR_MESSAGE_NO_ANCHOR_WITHOUT_TEXT.to_string()),
            Position { line: 0, column: 2 },
        );
        expect_err(
            "^",
            ParseErrorReason::UnexpectedEndOfInput,
            Position { line: 0, column: 1 },
        );
    }

    #[test]
    fn only_ending_anchor() {
        expect_err(
            "$ |",
            ParseErrorReason::InvalidSyntax(ERR_MESSAGE_NO_ANCHOR_WITHOUT_TEXT.to_string()),
            Position { line: 0, column: 0 },
        );
        expect_err(
            "$",
            ParseErrorReason::InvalidSyntax(ERR_MESSAGE_NO_ANCHOR_WITHOUT_TEXT.to_string()),
            Position { line: 0, column: 0 },
        );
    }

    #[test]
    fn only_both_anchors() {
        let matcher = parse_and_check("^$ |after", re("^$"), " |after");
        assert_eq!(matcher.matches(""), true);
        assert_eq!(matcher.matches("x"), false);
        assert_eq!(matcher.matches("\n"), false);
    }

    #[test]
    fn bareword_case_sensitivity() {
        let m = parse_and_check("hello", re_insensitive("hello"), "");
        assert_eq!(true, m.matches("hello"));
        assert_eq!(true, m.matches("HELLO"));
    }

    #[test]
    fn quoted_case_sensitivity() {
        let m = parse_and_check("'hello'", re("hello"), "");
        assert_eq!(true, m.matches("hello"));
        assert_eq!(false, m.matches("HELLO"));
    }

    #[test]
    fn quoted_anchor_start() {
        let m = parse_and_check("^'hello'", re("^hello"), "");
        assert_eq!(false, m.matches("pre hello"));
        assert_eq!(true, m.matches("hello"));
        assert_eq!(true, m.matches("hello post"));
    }

    #[test]
    fn quoted_anchor_end() {
        let m = parse_and_check("'hello'$", re("hello$"), "");
        assert_eq!(true, m.matches("pre hello"));
        assert_eq!(true, m.matches("hello"));
        assert_eq!(false, m.matches("hello post"));
    }

    #[test]
    fn anchor_whitespace() {
        parse_and_check("^foo", re("(?i)^foo"), "");
        parse_and_check("^     foo", re("(?i)^foo"), "");
        parse_and_check("^   'foo'", re("^foo"), "");

        parse_and_check("bar$", re("(?i)bar$"), "");
        parse_and_check("bar     $", re("(?i)bar$"), "");
        parse_and_check("'bar'   $", re("bar$"), "");

        parse_and_check("  ^  foobar  $  ", re("(?i)^foobar$"), "  ");
    }

    #[test]
    fn bareword_regex_char() {
        let m = parse_and_check("hello.world", re_insensitive("hello\\.world"), "");
        assert_eq!(true, m.matches("hello.world"));
        assert_eq!(false, m.matches("hello world")); // the period is _not_ a regex any
    }

    #[test]
    fn bareword_end_delimiters() {
        parse_and_check_with('@', "hello@world", re_insensitive("hello"), "@world");

        // "$" is always an end delimiter
        parse_and_check_with(
            '@',
            "hello$world",
            re_insensitive("hello$"),
            "world", // note: the dollar sign got consumed!
        );
    }

    /// Checks double-quoted string.
    ///
    /// Specifically:
    /// - single quotes can appear escaped or unescaped
    /// - double quotes must be escaped
    /// - \r, \n, \t
    /// - unicode code points
    #[test]
    fn double_quoted_string() {
        parse_and_check(
            r#" "hello world's ☃ \' \" \` \r \n \t says \"\u{2603}\" to me"_"#,
            re("hello world's ☃ ' \" ' \r \n \t says \"☃\" to me"),
            "_",
        );
    }

    /// Checks double-quoted string.
    ///
    /// See [double_quoted_string], except that _double_ quotes may be unescaped, and single quotes must be escaped.
    #[test]
    fn single_quoted_string() {
        parse_and_check(
            r#" 'hello world\'s ☃ \' \" \` \r \n \t says "\u{2603}" to me'_"#,
            re("hello world's ☃ ' \" ' \r \n \t says \"☃\" to me"),
            "_",
        );
    }

    #[test]
    fn quote_errs() {
        expect_err(
            r#" " "#,
            ParseErrorReason::Expected('"'),
            Position {
                line: 0,
                column: r#" " "#.len(),
            },
        );
        expect_err(
            r#" ' "#,
            ParseErrorReason::Expected('\''),
            Position {
                line: 0,
                column: " ' ".len(),
            },
        );
        expect_err(
            r#" '\"#,
            ParseErrorReason::UnexpectedEndOfInput,
            Position {
                line: 0,
                column: r#" "\"#.len(),
            },
        );
        expect_err(
            r#" "\x" "#,
            ParseErrorReason::InvalidEscape,
            Position {
                line: 0,
                column: r#" "\x"#.len(),
            },
        );
        expect_err(
            r#" "\u2603" "#,
            ParseErrorReason::Expected('{'),
            Position {
                line: 0,
                column: r#" "\u2"#.len(),
            },
        );
        expect_err(
            r#" "\u{}" "#,
            ParseErrorReason::InvalidEscape,
            Position {
                line: 0,
                column: r#" "\u{}"#.len(),
            },
        );
        expect_err(
            r#" "\u{12345678}" "#, // out of range
            ParseErrorReason::Expected('}'),
            Position {
                line: 0,
                column: r#" "\u{1234567"#.len(),
            },
        );
        expect_err(
            r#" "\u{snowman}" "#,
            ParseErrorReason::InvalidEscape,
            Position {
                line: 0,
                column: r#" "\u{s"#.len(),
            },
        );
        expect_err(
            r#" "\u{2603"#,
            ParseErrorReason::Expected('}'),
            Position {
                line: 0,
                column: r#" "\u{2603"#.len(),
            },
        );
    }

    //noinspection RegExpSingleCharAlternation (for the "(a|b)" case)
    #[test]
    fn regex() {
        parse_and_check(r#"/foo/"#, StringMatcher::regex(Regex::new("foo").unwrap()), "");
        parse_and_check(r#"/foo /"#, StringMatcher::regex(Regex::new("foo ").unwrap()), "");
        parse_and_check(r#"/foo/bar"#, StringMatcher::regex(Regex::new("foo").unwrap()), "bar");
        parse_and_check(r#"//"#, StringMatcher::regex(Regex::new("").unwrap()), "");
        parse_and_check(r#"/(a|b)/"#, StringMatcher::regex(Regex::new("(a|b)").unwrap()), "");
        parse_and_check(r#"/\d/"#, StringMatcher::regex(Regex::new("\\d").unwrap()), "");
        parse_and_check(
            r#"/fizz\/buzz/"#,
            StringMatcher::regex(Regex::new("fizz/buzz").unwrap()),
            "",
        );

        expect_err(
            r#"/unclosed"#,
            ParseErrorReason::UnexpectedEndOfInput,
            Position {
                line: 0,
                column: "/unclosed".len(),
            },
        );

        expect_err(
            r#"/(unclosed paren/"#,
            ParseErrorReason::InvalidSyntax(
                indoc! {r#"
                    regex parse error:
                        (unclosed paren
                        ^
                    error: unclosed group"#}
                .to_string(),
            ),
            Position {
                line: 0,
                column: "/(unclosed paren/".len(),
            },
        );
    }

    #[test]
    fn any() {
        let empty_matcher = parse_and_check("| rest", StringMatcher::any(), "| rest");
        assert_eq!(empty_matcher.matches(""), true);

        parse_and_check(" | rest", StringMatcher::any(), "| rest");
        parse_and_check("*| rest", StringMatcher::any(), "| rest");
        parse_and_check(" * | rest", StringMatcher::any(), "| rest");
        parse_and_check_with(']', "] rest", StringMatcher::any(), "] rest");
    }

    fn parse_and_check_with(
        bareword_end: char,
        text: &str,
        expect: StringMatcher,
        expect_remaining: &str,
    ) -> StringMatcher {
        let mut iter = ParsingIterator::new(text);
        let matcher = StringMatcher::read(&mut iter, bareword_end).unwrap();
        assert_eq!(matcher, expect);
        let remaining: String = iter.collect();
        assert_eq!(&remaining, expect_remaining);
        expect
    }

    fn parse_and_check(text: &str, expect: StringMatcher, expect_remaining: &str) -> StringMatcher {
        parse_and_check_with(SELECTOR_SEPARATOR, text, expect, expect_remaining)
    }

    fn expect_err(text: &str, expect: ParseErrorReason, at: Position) {
        let mut iter = ParsingIterator::new(text);
        let err = StringMatcher::read(&mut iter, SELECTOR_SEPARATOR).expect_err("expected to fail parsing");
        assert_eq!(iter.input_position(), at);
        assert_eq!(err, expect);
    }

    fn re(value: &str) -> StringMatcher {
        StringMatcher {
            re: Regex::new(value).expect("test error"),
        }
    }

    fn re_insensitive(value: &str) -> StringMatcher {
        let mut s = String::with_capacity(value.len() + 3);
        s.push_str("(?i)");
        s.push_str(value);
        re(&s)
    }

    impl From<&str> for StringMatcher {
        fn from(value: &str) -> Self {
            Self {
                re: Regex::from_str(value).unwrap(),
            }
        }
    }
}
