use crate::md_elem::elem::*;
use crate::md_elem::*;
use crate::output::inlines_to_plain_string;
use crate::select::Matcher;
use fancy_regex::Regex;
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
        match self.re.is_match(haystack) {
            Ok(m) => m,
            Err(e) => {
                panic!("failed to evaluate regular expression: {e}");
            }
        }
    }

    pub fn matches_inlines<I: Borrow<Inline>>(&self, haystack: &[I]) -> bool {
        self.matches(&inlines_to_plain_string(haystack))
    }

    pub fn matches_any<N: Borrow<MdElem>>(&self, haystacks: &[N]) -> bool {
        haystacks.iter().any(|node| self.matches_node(node.borrow()))
    }

    fn matches_node(&self, node: &MdElem) -> bool {
        match node {
            MdElem::Doc(elems) => self.matches_any(elems),
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
            MdElem::BlockHtml(html) => self.matches(&html.value),
            MdElem::Inline(inline) => self.matches_inlines(&[inline]),
        }
    }

    pub fn any() -> Self {
        Self {
            re: Regex::new(".*").expect("internal error"),
        }
    }

    fn regex(re: Regex) -> Self {
        Self { re }
    }
}

impl From<Matcher> for StringMatcher {
    fn from(value: Matcher) -> Self {
        match value {
            Matcher::Text {
                case_sensitive,
                anchor_start,
                text,
                anchor_end,
            } => SubstringToRegex {
                look_for: text,
                case_sensitive,
                anchor_start,
                anchor_end,
            }
            .to_string_matcher(),
            Matcher::Regex(re) => Self::regex(re.re),
            Matcher::Any { .. } => Self::any(),
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
        pattern.push_str(&fancy_regex::escape(&self.look_for));
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
    use crate::query::{ParseError, StringVariant};
    use std::str::FromStr;

    #[test]
    fn bareword() {
        parse_and_check("hello", re_insensitive("hello"), "");
        parse_and_check("hello ", re_insensitive("hello"), "");
        parse_and_check("hello / goodbye", re_insensitive("hello / goodbye"), "");
        parse_and_check("hello| goodbye", re_insensitive("hello"), "| goodbye");
        parse_and_check("hello | goodbye", re_insensitive("hello"), "| goodbye");
        parse_and_check_with(
            StringVariant::AngleBracket,
            "foo> rest",
            re_insensitive("foo"),
            "> rest",
        );
    }

    #[test]
    fn bareword_anchor_start() {
        let m = parse_and_check("^ hello |after", re_insensitive("^hello"), "|after");
        assert!(!m.matches("pre hello"));
        assert!(m.matches("hello"));
        assert!(m.matches("hello post"));
    }

    #[test]
    fn bareword_anchor_end() {
        let m = parse_and_check(" hello $ |after", re_insensitive("hello$"), "|after");
        assert!(m.matches("pre hello"));
        assert!(m.matches("hello"));
        assert!(!m.matches("hello post"));
    }

    #[test]
    fn only_starting_anchor() {
        parse_and_check("^ |", StringMatcher::any(), "^ |");
        parse_and_check("^", StringMatcher::any(), "^");
    }

    #[test]
    fn only_ending_anchor() {
        parse_and_check("$ |", StringMatcher::any(), "$ |");
        parse_and_check("$", StringMatcher::any(), "$");
    }

    #[test]
    fn only_both_anchors() {
        let matcher = parse_and_check("^$ |after", re("^$"), "|after");
        assert!(matcher.matches(""));
        assert!(!matcher.matches("x"));
        assert!(!matcher.matches("\n"));

        parse_and_check("^  $ |after", re("^$"), "|after");
    }

    #[test]
    fn bareword_case_sensitivity() {
        let m = parse_and_check("hello", re_insensitive("hello"), "");
        assert!(m.matches("hello"));
        assert!(m.matches("HELLO"));
    }

    #[test]
    fn quoted_case_sensitivity() {
        let m = parse_and_check("'hello'", re("hello"), "");
        assert!(m.matches("hello"));
        assert!(!m.matches("HELLO"));
    }

    #[test]
    fn quoted_anchor_start() {
        let m = parse_and_check("^'hello'", re("^hello"), "");
        assert!(!m.matches("pre hello"));
        assert!(m.matches("hello"));
        assert!(m.matches("hello post"));
    }

    #[test]
    fn quoted_anchor_end() {
        let m = parse_and_check("'hello'$", re("hello$"), "");
        assert!(m.matches("pre hello"));
        assert!(m.matches("hello"));
        assert!(!m.matches("hello post"));
    }

    #[test]
    fn anchor_whitespace() {
        parse_and_check("^foo", re("(?i)^foo"), "");
        parse_and_check("^     foo", re("(?i)^foo"), "");
        parse_and_check("^   'foo'", re("^foo"), "");

        parse_and_check("bar$", re("(?i)bar$"), "");
        parse_and_check("bar     $", re("(?i)bar$"), "");
        parse_and_check("'bar'   $", re("bar$"), "");

        parse_and_check("^  foobar  $  ", re("(?i)^foobar$"), "");
    }

    #[test]
    fn bareword_regex_char() {
        let m = parse_and_check("hello.world", re_insensitive("hello\\.world"), "");
        assert!(m.matches("hello.world"));
        assert!(!m.matches("hello world")); // the period is _not_ a regex any
    }

    #[test]
    fn bareword_end_delimiters() {
        parse_and_check_with(
            StringVariant::AngleBracket,
            "hello>world",
            re_insensitive("hello"),
            ">world",
        );

        // "$" is always an end delimiter
        parse_and_check_with(
            StringVariant::AngleBracket,
            "hello$world",
            re_insensitive("hello$"),
            "world", // note: the dollar sign got consumed, since it's part of the string
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
        expect_empty(r#"" "#);
        expect_empty(r#"' "#);
        expect_empty(r#"'\"#);
        expect_empty(r#""\x" "#);
        expect_empty(r#""\u2603" "#);
        expect_empty(r#""\u{}" "#);
        expect_empty(r#""\u{12345678}" "#); // out of range
        expect_empty(r#""\u{snowman}" "#);
        expect_empty(r#""\u{2603"#);
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

        expect_empty(r#"/unclosed"#);

        expect_err(r#"/(unclosed paren/"#);
    }

    #[test]
    fn any() {
        let empty_matcher = parse_and_check("| rest", StringMatcher::any(), "| rest");
        assert!(empty_matcher.matches(""));

        parse_and_check("| rest", StringMatcher::any(), "| rest");
        parse_and_check("*| rest", StringMatcher::any(), "| rest");
        parse_and_check("* | rest", StringMatcher::any(), "| rest");
        parse_and_check_with(StringVariant::AngleBracket, "> rest", StringMatcher::any(), "> rest");
    }

    /// Test for fancy_regex specific feature (lookaround)
    #[test]
    fn fancy_regex_lookahead() {
        let matcher = re(r#"foo(?=bar)"#); // Positive lookahead: matches "foo" only if followed by "bar"
        assert!(matcher.matches("foobar"));
        assert!(!matcher.matches("foo"));
        assert!(!matcher.matches("foobaz"));
    }

    fn parse_and_check_with(
        string_variant: StringVariant,
        text: &str,
        expect: StringMatcher,
        expect_remaining: &str,
    ) -> StringMatcher {
        let (actual_matcher, actual_remaining) = match Matcher::parse(string_variant, text) {
            Ok(parsed) => parsed,
            Err(err) => {
                let public_err = ParseError::new(err);
                panic!("{public_err:?}")
            }
        };
        let actual_string_matcher: StringMatcher = actual_matcher.into();
        assert_eq!(actual_string_matcher, expect);
        assert_eq!(actual_remaining, expect_remaining);
        expect
    }

    fn parse_and_check(text: &str, expect: StringMatcher, expect_remaining: &str) -> StringMatcher {
        parse_and_check_with(StringVariant::Pipe, text, expect, expect_remaining)
    }

    fn expect_empty(text: &str) {
        parse_and_check(text, StringMatcher::any(), text);
    }

    fn expect_err(text: &str) {
        if let Ok(unexpected) = Matcher::parse(StringVariant::Pipe, text) {
            panic!("unexpected success: {unexpected:?}")
        }
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
