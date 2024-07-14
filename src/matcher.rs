use crate::fmt_str::inlines_to_plain_string;
use crate::parsing_iter::ParsingIterator;
use crate::select::{ParseErrorReason, ParseResult};
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

const ERR_MESSAGE_NO_ANCHOR_WITHOUT_TEXT: &str = r#"can't have single anchor without text"#;

impl StringMatcher {
    const BAREWORD_ANCHOR_END: char = '$';

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
            MdElem::Inline(inline) => self.matches_inlines(&[inline]),
        }
    }

    pub fn read(chars: &mut ParsingIterator, bareword_end: char) -> ParseResult<Self> {
        chars.drop_whitespace();
        let peek_ch = match chars.peek() {
            None => return Ok(StringMatcher::any()),
            Some(ch) => ch,
        };

        let (anchor_start, peek_ch) = if peek_ch == '^' {
            let _ = chars.next(); // drop the ^ char
            chars.drop_whitespace();
            let next_ch = chars.peek().ok_or_else(|| ParseErrorReason::UnexpectedEndOfInput)?;
            (true, next_ch)
        } else {
            (false, peek_ch)
        };

        if peek_ch.is_alphanumeric() {
            return Ok(Self::parse_matcher_bare(chars, bareword_end, anchor_start));
        }
        match peek_ch {
            '*' => {
                let _ = chars.next(); // drop the char we peeked
                chars.drop_whitespace();
                Ok(StringMatcher::any())
            }
            '/' => {
                let _ = chars.next();
                Self::parse_regex_matcher(chars)
            }
            ch @ ('\'' | '"') => {
                let _ = chars.next();
                Self::parse_matcher_quoted(chars, ch, anchor_start)
            }
            '$' => {
                if anchor_start {
                    let _ = chars.next();
                    Ok(StringMatcher::empty())
                } else {
                    Err(ParseErrorReason::InvalidSyntax(
                        ERR_MESSAGE_NO_ANCHOR_WITHOUT_TEXT.to_string(),
                    ))
                }
            }
            other if other == bareword_end => {
                // do *not* consume the bareword end delimiter!
                if anchor_start {
                    Err(ParseErrorReason::InvalidSyntax(
                        ERR_MESSAGE_NO_ANCHOR_WITHOUT_TEXT.to_string(),
                    ))
                } else {
                    Ok(StringMatcher::any())
                }
            }
            _ => Err(ParseErrorReason::InvalidSyntax(
                "invalid string specifier (must be quoted or a bareword that starts with a letter)".to_string(),
            )),
        }
    }

    fn parse_matcher_bare(chars: &mut ParsingIterator, bareword_end: char, anchor_start: bool) -> Self {
        let mut result = String::with_capacity(20); // just a guess
        let mut dropped = String::with_capacity(8); // also a guess

        let anchor_end = loop {
            // Drop whitespace, but keep a record of it. If we see a char within this bareword (ie not end-of-input or
            // the bareword_end), then we'll append that whitespace back.
            chars.drop_to_while(&mut dropped, |ch| ch.is_whitespace());
            let Some(ch) = chars.peek() else {
                break false;
            };
            if ch == Self::BAREWORD_ANCHOR_END {
                let _ = chars.next();
                break true;
            }
            if ch == bareword_end {
                break false;
            }
            let _ = chars.next();
            result.push_str(&dropped);
            result.push(ch);
        };

        SubstringToRegex {
            look_for: result,
            case_sensitive: false,
            anchor_start,
            anchor_end,
        }
        .to_string_matcher()
    }

    fn parse_matcher_quoted(chars: &mut ParsingIterator, ending_char: char, anchor_start: bool) -> ParseResult<Self> {
        let mut result = String::with_capacity(20); // just a guess
        loop {
            match chars.next().ok_or_else(|| ParseErrorReason::Expected(ending_char))? {
                '\\' => {
                    let escaped = match chars.next().ok_or_else(|| ParseErrorReason::UnexpectedEndOfInput)? {
                        escaped @ ('\'' | '"' | '\\') => escaped,
                        'n' => '\n',
                        'r' => '\r',
                        't' => '\t',
                        'u' => {
                            chars.require_char('{')?;
                            let mut hex_str = String::with_capacity(6);
                            loop {
                                match chars.next().ok_or_else(|| ParseErrorReason::Expected('}'))? {
                                    '}' => break,
                                    ch if ch.is_ascii_hexdigit() => {
                                        hex_str.push(ch);
                                        if hex_str.len() > 6 {
                                            return Err(ParseErrorReason::Expected('}'));
                                        }
                                    }
                                    _ => return Err(ParseErrorReason::InvalidEscape),
                                }
                            }
                            let as_int =
                                u32::from_str_radix(&hex_str, 16).map_err(|_| ParseErrorReason::InvalidEscape)?;
                            char::from_u32(as_int).ok_or_else(|| ParseErrorReason::InvalidEscape)?
                        }
                        _ => return Err(ParseErrorReason::InvalidEscape),
                    };
                    result.push(escaped);
                }
                ch if ch == ending_char => break,
                ch => result.push(ch),
            }
        }
        chars.drop_whitespace();
        let anchor_end = chars.consume_if(Self::BAREWORD_ANCHOR_END);
        Ok(SubstringToRegex {
            look_for: result,
            case_sensitive: true,
            anchor_start,
            anchor_end,
        }
        .to_string_matcher())
    }

    fn parse_regex_matcher(chars: &mut ParsingIterator) -> ParseResult<StringMatcher> {
        let mut result = String::with_capacity(20); // just a guess

        loop {
            match chars.next() {
                None => return Err(ParseErrorReason::UnexpectedEndOfInput),
                Some('\\') => match chars.next() {
                    None => return Err(ParseErrorReason::UnexpectedEndOfInput),
                    Some('/') => result.push('/'),
                    Some(escaped) => {
                        result.push('\\');
                        result.push(escaped);
                    }
                },
                Some('/') => break,
                Some(ch) => result.push(ch),
            }
        }

        match Regex::new(&result) {
            Ok(re) => Ok(StringMatcher::regex(re)),
            Err(err) => Err(ParseErrorReason::InvalidSyntax(err.to_string())),
        }
    }

    fn any() -> Self {
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
    use crate::parse_common::Position;
    use crate::select::SELECTOR_SEPARATOR;
    use indoc::indoc;

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
            r#" "hello world's ☃ \' \" \r \n \t says \"\u{2603}\" to me"_"#,
            re("hello world's ☃ ' \" \r \n \t says \"☃\" to me"),
            "_",
        );
    }

    /// Checks double-quoted string.
    ///
    /// See [double_quoted_string], except that _double_ quotes may be unescaped, and single quotes must be escaped.
    #[test]
    fn single_quoted_string() {
        parse_and_check(
            r#" 'hello world\'s ☃ \' \" \r \n \t says "\u{2603}" to me'_"#,
            re("hello world's ☃ ' \" \r \n \t says \"☃\" to me"),
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
}
