use crate::md_elem::elem::*;
use crate::md_elem::*;
use crate::output::inlines_to_plain_string;
use crate::select::{MatchReplace, Matcher, SelectError};
use fancy_regex::Regex;
use std::borrow::Borrow;

#[derive(Debug)]
pub struct StringMatcher {
    re: Regex,
    replacement: Option<String>,
}

#[derive(Clone, Debug)]
pub(crate) enum StringMatchError {
    NotSupported,
    RegexError(Box<fancy_regex::Error>),
}

impl StringMatchError {
    pub fn to_select_error(&self, selector_name: &str) -> SelectError {
        let message = match self {
            StringMatchError::NotSupported => format!("{selector_name} selector does not support string replace"),
            StringMatchError::RegexError(err) => format!("regex evaluation error in {selector_name} selector: {err}"),
        };
        SelectError::new(message)
    }
}

impl PartialEq for StringMatcher {
    fn eq(&self, other: &Self) -> bool {
        self.re.as_str() == other.re.as_str() && self.replacement == other.replacement
    }
}

impl StringMatcher {
    pub fn matches(&self, haystack: &str) -> Result<bool, StringMatchError> {
        if self.replacement.is_some() {
            return Err(StringMatchError::NotSupported);
        }
        match self.re.is_match(haystack) {
            Ok(m) => Ok(m),
            Err(e) => Err(StringMatchError::RegexError(Box::new(e))),
        }
    }

    pub fn matches_inlines<I: Borrow<Inline>>(&self, haystack: &[I]) -> Result<bool, StringMatchError> {
        self.matches(&inlines_to_plain_string(haystack))
    }

    pub fn matches_any<N: Borrow<MdElem>>(&self, haystacks: &[N]) -> Result<bool, StringMatchError> {
        for node in haystacks {
            if self.matches_node(node.borrow())? {
                return Ok(true);
            }
        }
        Ok(false)
    }

    fn matches_node(&self, node: &MdElem) -> Result<bool, StringMatchError> {
        match node {
            MdElem::Doc(elems) => self.matches_any(elems),
            MdElem::Paragraph(p) => self.matches_inlines(&p.body),
            MdElem::Table(table) => {
                for row in &table.rows {
                    for cell in row {
                        if self.matches_inlines(cell)? {
                            return Ok(true);
                        }
                    }
                }
                Ok(false)
            }
            MdElem::List(list) => {
                for item in &list.items {
                    if self.matches_any(&item.item)? {
                        return Ok(true);
                    }
                }
                Ok(false)
            }
            MdElem::BlockQuote(block) => self.matches_any(&block.body),
            MdElem::Section(section) => {
                if self.matches_inlines(&section.title)? {
                    return Ok(true);
                }
                self.matches_any(&section.body)
            }
            MdElem::BlockHtml(html) => self.matches(&html.value),
            MdElem::Inline(inline) => self.matches_inlines(&[inline]),
            // Base cases: these don't recurse, so we say the StringMatcher doesn't match them. A Selector still may,
            // but that's Selector-specific logic, not StringMatcher logic.
            MdElem::ThematicBreak | MdElem::CodeBlock(_) | MdElem::FrontMatter(_) => Ok(false),
        }
    }

    fn re_for_any() -> Regex {
        Regex::new(".*").expect("internal error")
    }

    fn re_for_regex(re: Regex) -> Regex {
        re
    }

    fn re_from_text(text: String, case_sensitive: bool, anchor_start: bool, anchor_end: bool) -> Regex {
        let mut pattern = String::with_capacity(text.len() + 10); // +10 for modifiers, escapes, etc
        if !case_sensitive && !text.is_empty() {
            // (is_empty isn't necessary, just makes for a cleaner regex)
            pattern.push_str("(?i)");
        }
        if anchor_start {
            pattern.push('^');
        }
        pattern.push_str(&fancy_regex::escape(&text));
        if anchor_end {
            pattern.push('$');
        }

        Regex::new(&pattern).expect("internal error")
    }
}

impl From<MatchReplace> for StringMatcher {
    fn from(value: MatchReplace) -> Self {
        let MatchReplace { matcher, replacement } = value;
        let re = match matcher {
            Matcher::Text {
                case_sensitive,
                anchor_start,
                text,
                anchor_end,
            } => Self::re_from_text(text, case_sensitive, anchor_start, anchor_end),
            Matcher::Regex(re) => Self::re_for_regex(re.re),
            Matcher::Any { .. } => Self::re_for_any(),
        };
        Self { re, replacement }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::query::{ParseError, StringVariant};
    use std::str::FromStr;

    /// PartialEq implementation for StringMatchError. This is only available in tests, so that we don't expose the
    /// brittle handling of our `RegexError` variant.
    impl PartialEq for StringMatchError {
        fn eq(&self, other: &Self) -> bool {
            match (self, other) {
                (StringMatchError::NotSupported, StringMatchError::NotSupported) => true,
                (StringMatchError::RegexError(_), StringMatchError::RegexError(_)) => {
                    // We can't compare fancy_regex::Error instances, so we'll consider them equal
                    // if they're both RegexError variants. This is sufficient for our testing needs.
                    true
                }
                _ => false,
            }
        }
    }

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
        assert!(!m.matches("pre hello").unwrap());
        assert!(m.matches("hello").unwrap());
        assert!(m.matches("hello post").unwrap());
    }

    #[test]
    fn bareword_anchor_end() {
        let m = parse_and_check(" hello $ |after", re_insensitive("hello$"), "|after");
        assert!(m.matches("pre hello").unwrap());
        assert!(m.matches("hello").unwrap());
        assert!(!m.matches("hello post").unwrap());
    }

    #[test]
    fn only_starting_anchor() {
        parse_and_check("^ |", StringMatcher::re_for_any(), "^ |");
        parse_and_check("^", StringMatcher::re_for_any(), "^");
    }

    #[test]
    fn only_ending_anchor() {
        parse_and_check("$ |", StringMatcher::re_for_any(), "$ |");
        parse_and_check("$", StringMatcher::re_for_any(), "$");
    }

    #[test]
    fn only_both_anchors() {
        let matcher = parse_and_check("^$ |after", re("^$"), "|after");
        assert!(matcher.matches("").unwrap());
        assert!(!matcher.matches("x").unwrap());
        assert!(!matcher.matches("\n").unwrap());

        parse_and_check("^  $ |after", re("^$"), "|after");
    }

    #[test]
    fn bareword_case_sensitivity() {
        let m = parse_and_check("hello", re_insensitive("hello"), "");
        assert!(m.matches("hello").unwrap());
        assert!(m.matches("HELLO").unwrap());
    }

    #[test]
    fn quoted_case_sensitivity() {
        let m = parse_and_check("'hello'", re("hello"), "");
        assert!(m.matches("hello").unwrap());
        assert!(!m.matches("HELLO").unwrap());
    }

    #[test]
    fn quoted_anchor_start() {
        let m = parse_and_check("^'hello'", re("^hello"), "");
        assert!(!m.matches("pre hello").unwrap());
        assert!(m.matches("hello").unwrap());
        assert!(m.matches("hello post").unwrap());
    }

    #[test]
    fn quoted_anchor_end() {
        let m = parse_and_check("'hello'$", re("hello$"), "");
        assert!(m.matches("pre hello").unwrap());
        assert!(m.matches("hello").unwrap());
        assert!(!m.matches("hello post").unwrap());
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
        assert!(m.matches("hello.world").unwrap());
        assert!(!m.matches("hello world").unwrap()); // the period is _not_ a regex any
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
        parse_and_check(r#"/foo/"#, StringMatcher::re_for_regex(Regex::new("foo").unwrap()), "");
        parse_and_check(
            r#"/foo /"#,
            StringMatcher::re_for_regex(Regex::new("foo ").unwrap()),
            "",
        );
        parse_and_check(
            r#"/foo/bar"#,
            StringMatcher::re_for_regex(Regex::new("foo").unwrap()),
            "bar",
        );
        parse_and_check(r#"//"#, StringMatcher::re_for_regex(Regex::new("").unwrap()), "");
        parse_and_check(
            r#"/(a|b)/"#,
            StringMatcher::re_for_regex(Regex::new("(a|b)").unwrap()),
            "",
        );
        parse_and_check(r#"/\d/"#, StringMatcher::re_for_regex(Regex::new("\\d").unwrap()), "");
        parse_and_check(
            r#"/fizz\/buzz/"#,
            StringMatcher::re_for_regex(Regex::new("fizz/buzz").unwrap()),
            "",
        );

        expect_empty(r#"/unclosed"#);

        expect_err(r#"/(unclosed paren/"#);
    }

    #[test]
    fn any() {
        let empty_matcher = parse_and_check("| rest", StringMatcher::re_for_any(), "| rest");
        assert!(empty_matcher.matches("").unwrap());

        parse_and_check("| rest", StringMatcher::re_for_any(), "| rest");
        parse_and_check("*| rest", StringMatcher::re_for_any(), "| rest");
        parse_and_check("* | rest", StringMatcher::re_for_any(), "| rest");
        parse_and_check_with(
            StringVariant::AngleBracket,
            "> rest",
            StringMatcher::re_for_any(),
            "> rest",
        );
    }

    /// Test for fancy_regex specific feature (lookaround)
    #[test]
    fn fancy_regex_lookahead() {
        let re_instance = re(r#"foo(?=bar)"#); // Positive lookahead: matches "foo" only if followed by "bar"
        let matcher = StringMatcher {
            re: re_instance,
            replacement: None,
        };
        assert!(matcher.matches("foobar").unwrap());
        assert!(!matcher.matches("foo").unwrap());
        assert!(!matcher.matches("foobaz").unwrap());
    }

    #[test]
    fn matches_with_replacement_returns_not_supported_error() {
        let matcher_with_replacement = StringMatcher::from(MatchReplace {
            matcher: Matcher::Text {
                case_sensitive: false,
                anchor_start: false,
                text: "hello".to_string(),
                anchor_end: false,
            },
            replacement: Some("world".to_string()),
        });

        assert_eq!(
            matcher_with_replacement.matches("hello"),
            Err(StringMatchError::NotSupported)
        );
    }

    #[test]
    fn matches_inlines_with_replacement_returns_not_supported_error() {
        let matcher_with_replacement = StringMatcher::from(MatchReplace {
            matcher: Matcher::Any { explicit: false },
            replacement: Some("replacement".to_string()),
        });

        let inlines: Vec<Inline> = vec![]; // empty inlines for simplicity
        assert_eq!(
            matcher_with_replacement.matches_inlines(&inlines),
            Err(StringMatchError::NotSupported)
        );
    }

    #[test]
    fn string_match_error_to_select_error_formatting() {
        let not_supported_error = StringMatchError::NotSupported;
        let select_error = not_supported_error.to_select_error("section");
        assert_eq!(
            select_error.to_string(),
            "section selector does not support string replace"
        );
    }

    fn parse_and_check_with(
        string_variant: StringVariant,
        text: &str,
        expect_re: Regex,
        expect_remaining: &str,
    ) -> StringMatcher {
        let (actual_matcher, actual_remaining) = match Matcher::parse(string_variant, text) {
            Ok(parsed) => parsed,
            Err(err) => {
                let public_err = ParseError::new(err);
                panic!("{public_err:?}")
            }
        };
        let expect = StringMatcher {
            re: expect_re,
            replacement: None,
        };
        let actual_string_matcher: StringMatcher = actual_matcher.into();
        assert_eq!(actual_string_matcher, expect);
        assert_eq!(actual_remaining, expect_remaining);
        expect
    }

    fn parse_and_check(text: &str, expect: Regex, expect_remaining: &str) -> StringMatcher {
        parse_and_check_with(StringVariant::Pipe, text, expect, expect_remaining)
    }

    fn expect_empty(text: &str) {
        parse_and_check(text, StringMatcher::re_for_any(), text);
    }

    fn expect_err(text: &str) {
        if let Ok(unexpected) = Matcher::parse(StringVariant::Pipe, text) {
            panic!("unexpected success: {unexpected:?}")
        }
    }

    fn re(value: &str) -> Regex {
        Regex::new(value).expect("test error")
    }

    fn re_insensitive(value: &str) -> Regex {
        let mut s = String::with_capacity(value.len() + 3);
        s.push_str("(?i)");
        s.push_str(value);
        re(&s)
    }

    impl From<&str> for StringMatcher {
        fn from(value: &str) -> Self {
            Self {
                re: Regex::from_str(value).unwrap(),
                replacement: None,
            }
        }
    }
}
