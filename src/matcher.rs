use crate::parse_common::ParseErrorReason;
use crate::parsing_iter::ParsingIterator;
use crate::select::{ParseResult, SubstringMatcher};
use regex::Regex;

#[derive(Debug)]
pub enum Matcher {
    Any,
    Substring(SubstringMatcher),
    Regex(Regex),
}

impl PartialEq for Matcher {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Matcher::Any, Matcher::Any) => true,
            (Matcher::Substring(lhs), Matcher::Substring(rhs)) => lhs == rhs,
            (Matcher::Regex(lhs), Matcher::Regex(rhs)) => lhs.as_str() == rhs.as_str(),
            _ => false,
        }
    }
}

impl Matcher {
    pub fn matches(&self, haystack: &str) -> bool {
        match self {
            Matcher::Any => true,
            Matcher::Substring(SubstringMatcher { look_for }) => haystack.contains(look_for),
            Matcher::Regex(re) => re.is_match(haystack),
        }
    }

    pub fn parse_matcher<C: Iterator<Item = char>>(chars: &mut ParsingIterator<C>) -> ParseResult<Matcher> {
        chars.drop_while(|ch| ch.is_whitespace());
        let peek_ch = match chars.peek() {
            None => return Ok(Matcher::Any),
            Some(ch) => ch,
        };
        if peek_ch.is_alphanumeric() {
            return Ok(Self::parse_matcher_bare(chars));
        }
        let _ = chars.next(); // drop the char we peeked
        match peek_ch {
            '*' => Ok(Matcher::Any),
            '/' => {
                if matches!(chars.peek(), Some('/')) {
                    // not a regex, but an empty matcher
                    let _ = chars.next(); // drop the '/'
                    Ok(Matcher::Any)
                } else {
                    Self::parse_regex_matcher(chars)
                }
            }
            _ => Err(ParseErrorReason::UnexpectedCharacter(peek_ch)),
        }
    }

    fn parse_matcher_bare<C: Iterator<Item = char>>(chars: &mut ParsingIterator<C>) -> Matcher {
        let mut result = String::with_capacity(20); // just a guess
        let mut dropped = String::with_capacity(8); // also a guess

        loop {
            chars.drop_to_while(&mut dropped, |ch| ch.is_whitespace());
            let Some(ch) = chars.next() else {
                break;
            };
            if ch == '/' && matches!(chars.peek(), Some('/')) {
                chars.next(); // consume the second '/'
                break;
            }
            result.push_str(&dropped);
            result.push(ch);
        }

        Matcher::Substring(SubstringMatcher { look_for: result })
    }

    fn parse_regex_matcher<C: Iterator<Item = char>>(chars: &mut ParsingIterator<C>) -> ParseResult<Matcher> {
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
            Ok(re) => Ok(Matcher::Regex(re)),
            Err(err) => Err(ParseErrorReason::InvalidSyntax(err.to_string())),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parse_common::{parse_and_check_mapped, Position};
    use indoc::indoc;

    #[test]
    fn bareword() {
        parse_and_check(
            "hello",
            Matcher::Substring(SubstringMatcher {
                look_for: "hello".to_string(),
            }),
            "",
        );
        parse_and_check(
            "hello ",
            Matcher::Substring(SubstringMatcher {
                look_for: "hello".to_string(),
            }),
            "",
        );
        parse_and_check(
            "hello / goodbye",
            Matcher::Substring(SubstringMatcher {
                look_for: "hello / goodbye".to_string(),
            }),
            "",
        );
        parse_and_check(
            "hello// goodbye",
            Matcher::Substring(SubstringMatcher {
                look_for: "hello".to_string(),
            }),
            " goodbye",
        );
        parse_and_check(
            "hello // goodbye",
            Matcher::Substring(SubstringMatcher {
                look_for: "hello".to_string(),
            }),
            " goodbye",
        );
    }

    #[test]
    fn regex() {
        parse_and_check(r#"/foo/"#, Matcher::Regex(Regex::new("foo").unwrap()), "");
        parse_and_check(r#"/foo /"#, Matcher::Regex(Regex::new("foo ").unwrap()), "");
        parse_and_check(r#"/foo/bar"#, Matcher::Regex(Regex::new("foo").unwrap()), "bar");
        parse_and_check(r#"//"#, Matcher::Any, ""); // NOT a regex!
        parse_and_check(r#"/\d/"#, Matcher::Regex(Regex::new("\\d").unwrap()), "");
        parse_and_check(r#"/fizz\/buzz/"#, Matcher::Regex(Regex::new("fizz/buzz").unwrap()), "");

        expect_err(
            r#"/unclosed"#,
            ParseErrorReason::UnexpectedEndOfInput,
            Position {
                line: 1,
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
                line: 1,
                column: "/(unclosed paren/".len(),
            },
        );
    }

    fn parse_and_check(text: &str, expect: Matcher, expect_remaining: &str) {
        parse_and_check_mapped(text, expect, expect_remaining, |iter| Matcher::parse_matcher(iter))
    }

    fn expect_err(text: &str, expect: ParseErrorReason, at: Position) {
        let mut iter = ParsingIterator::new(text.chars());
        let err = Matcher::parse_matcher(&mut iter).expect_err("expected to fail parsing");
        assert_eq!(iter.input_position(), at);
        assert_eq!(err, expect);
    }
}
