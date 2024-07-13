use crate::fmt_str::inlines_to_plain_string;
use crate::parsing_iter::ParsingIterator;
use crate::select::{ParseErrorReason, ParseResult};
use crate::tree::{Inline, MdElem};
use regex::Regex;
use std::borrow::Borrow;

#[derive(Debug)]
pub enum StringMatcher {
    Any,
    Substring(String),
    Regex(Regex),
}

impl PartialEq for StringMatcher {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (StringMatcher::Any, StringMatcher::Any) => true,
            (StringMatcher::Substring(lhs), StringMatcher::Substring(rhs)) => lhs == rhs,
            (StringMatcher::Regex(lhs), StringMatcher::Regex(rhs)) => lhs.as_str() == rhs.as_str(),
            _ => false,
        }
    }
}

impl StringMatcher {
    const BAREWORD_ANCHOR_END: char = '$';

    pub fn matches(&self, haystack: &str) -> bool {
        match self {
            StringMatcher::Any => true,
            StringMatcher::Substring(look_for) => {
                let pattern = format!("(?i){}", regex::escape(look_for));
                let re = Regex::new(&pattern).expect("internal error");
                re.is_match(haystack)
            }
            StringMatcher::Regex(re) => re.is_match(haystack),
        }
    }

    pub fn matches_inlines<I: Borrow<Inline>>(&self, haystack: &[I]) -> bool {
        self.matches(&inlines_to_plain_string(haystack))
    }

    pub fn matches_any<N: Borrow<MdElem>>(&self, haystacks: &[N]) -> bool {
        if matches!(self, StringMatcher::Any) {
            return true;
        }
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

    pub fn read(chars: &mut ParsingIterator, bareword_end: char) -> ParseResult<StringMatcher> {
        chars.drop_whitespace();
        let peek_ch = match chars.peek() {
            None => return Ok(StringMatcher::Any),
            Some(ch) => ch,
        };
        if peek_ch.is_alphanumeric() {
            return Ok(Self::parse_matcher_bare(chars, bareword_end));
        }
        match peek_ch {
            '*' => {
                let _ = chars.next(); // drop the char we peeked
                chars.drop_whitespace();
                Ok(StringMatcher::Any)
            }
            '/' => {
                let _ = chars.next();
                Self::parse_regex_matcher(chars)
            }
            ch @ ('\'' | '"') => {
                let _ = chars.next();
                Self::parse_matcher_quoted(chars, ch)
            }
            other if other == bareword_end => Ok(StringMatcher::Any), // do *not* consume it!
            _ => Err(ParseErrorReason::InvalidSyntax(
                "invalid string specifier (must be quoted or a bareword that starts with a letter)".to_string(),
            )),
        }
    }

    fn parse_matcher_bare(chars: &mut ParsingIterator, bareword_end: char) -> StringMatcher {
        let mut result = String::with_capacity(20); // just a guess
        let mut dropped = String::with_capacity(8); // also a guess

        loop {
            // Drop whitespace, but keep a record of it. If we see a char within this bareword (ie not end-of-input or
            // the bareword_end), then we'll append that whitespace back.
            chars.drop_to_while(&mut dropped, |ch| ch.is_whitespace());
            let Some(ch) = chars.peek() else {
                break;
            };
            if ch == Self::BAREWORD_ANCHOR_END {
                let _ = chars.next();
                break;
            }
            if ch == bareword_end {
                break;
            }
            let _ = chars.next();
            result.push_str(&dropped);
            result.push(ch);
        }

        StringMatcher::Substring(result)
    }

    fn parse_matcher_quoted(chars: &mut ParsingIterator, ending_char: char) -> ParseResult<StringMatcher> {
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
        Ok(StringMatcher::Substring(result))
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
            Ok(re) => Ok(StringMatcher::Regex(re)),
            Err(err) => Err(ParseErrorReason::InvalidSyntax(err.to_string())),
        }
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
        parse_and_check("hello", StringMatcher::Substring("hello".to_string()), "");
        parse_and_check("hello ", StringMatcher::Substring("hello".to_string()), "");
        parse_and_check(
            "hello / goodbye",
            StringMatcher::Substring("hello / goodbye".to_string()),
            "",
        );
        parse_and_check(
            "hello| goodbye",
            StringMatcher::Substring("hello".to_string()),
            "| goodbye",
        );
        parse_and_check(
            "hello | goodbye",
            StringMatcher::Substring("hello".to_string()),
            "| goodbye",
        );
        parse_and_check_with(']', "foo] rest", StringMatcher::Substring("foo".to_string()), "] rest");
    }

    #[test]
    fn bareword_case_sensitivity() {
        let m = parse_and_check("hello", StringMatcher::Substring("hello".to_string()), "");
        assert_eq!(true, m.matches("hello"));
        assert_eq!(true, m.matches("HELLO"));
    }

    #[test]
    fn bareword_regex_char() {
        let m = parse_and_check("hello.world", StringMatcher::Substring("hello.world".to_string()), "");
        assert_eq!(true, m.matches("hello.world"));
        assert_eq!(false, m.matches("hello world")); // the period is _not_ a regex any
    }

    #[test]
    fn bareword_end_delimiters() {
        parse_and_check_with(
            '@',
            "hello@world",
            StringMatcher::Substring("hello".to_string()),
            "@world",
        );

        // "$" is always an end delimiter
        parse_and_check_with(
            '@',
            "hello$world",
            StringMatcher::Substring("hello".to_string()),
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
            StringMatcher::Substring("hello world's ☃ ' \" \r \n \t says \"☃\" to me".to_string()),
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
            StringMatcher::Substring("hello world's ☃ ' \" \r \n \t says \"☃\" to me".to_string()),
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
        parse_and_check(r#"/foo/"#, StringMatcher::Regex(Regex::new("foo").unwrap()), "");
        parse_and_check(r#"/foo /"#, StringMatcher::Regex(Regex::new("foo ").unwrap()), "");
        parse_and_check(r#"/foo/bar"#, StringMatcher::Regex(Regex::new("foo").unwrap()), "bar");
        parse_and_check(r#"//"#, StringMatcher::Regex(Regex::new("").unwrap()), "");
        parse_and_check(r#"/(a|b)/"#, StringMatcher::Regex(Regex::new("(a|b)").unwrap()), "");
        parse_and_check(r#"/\d/"#, StringMatcher::Regex(Regex::new("\\d").unwrap()), "");
        parse_and_check(
            r#"/fizz\/buzz/"#,
            StringMatcher::Regex(Regex::new("fizz/buzz").unwrap()),
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
        parse_and_check("| rest", StringMatcher::Any, "| rest");
        parse_and_check(" | rest", StringMatcher::Any, "| rest");
        parse_and_check("*| rest", StringMatcher::Any, "| rest");
        parse_and_check(" * | rest", StringMatcher::Any, "| rest");
        parse_and_check_with(']', "] rest", StringMatcher::Any, "] rest");
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
}
