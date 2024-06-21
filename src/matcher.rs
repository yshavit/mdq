use crate::fmt_str::inlines_to_plain_string;
use crate::parsing_iter::ParsingIterator;
use crate::select::{ParseErrorReason, ParseResult, SELECTOR_SEPARATOR};
use crate::tree::{Inline, MdqNode};
use regex::Regex;
use std::borrow::Borrow;

#[derive(Debug)]
pub enum Matcher {
    // TODO rename to StringMatcher
    Any,
    Substring(String),
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
        // TODO deprecate in favor of matches_inlines
        match self {
            Matcher::Any => true,
            Matcher::Substring(look_for) => haystack.contains(look_for),
            Matcher::Regex(re) => re.is_match(haystack),
        }
    }

    pub fn matches_inlines<I: Borrow<Inline>>(&self, haystack: &[I]) -> bool {
        self.matches(&inlines_to_plain_string(haystack))
    }

    pub fn matches_any<N: Borrow<MdqNode>>(&self, haystacks: &[N]) -> bool {
        if matches!(self, Matcher::Any) {
            return true;
        }
        haystacks.iter().any(|node| self.matches_node(node.borrow()))
    }

    fn matches_node(&self, node: &MdqNode) -> bool {
        match node {
            MdqNode::Section(section) => {
                if self.matches_inlines(&section.title) {
                    return true;
                }
                self.matches_any(&section.body)
            }
            MdqNode::Paragraph(paragraph) => self.matches_inlines(&paragraph.body),
            MdqNode::BlockQuote(block) => self.matches_any(&block.body),
            MdqNode::List(list) => list.items.iter().any(|li| self.matches_any(&li.item)),
            MdqNode::Table(table) => {
                for row in &table.rows {
                    for cell in row {
                        if self.matches_inlines(cell) {
                            return true;
                        }
                    }
                }
                false
            }
            MdqNode::ThematicBreak => false,
            MdqNode::CodeBlock(_) => false,
            MdqNode::Inline(inline) => self.matches_inlines(&[inline]),
        }
    }

    pub fn parse_matcher(chars: &mut ParsingIterator) -> ParseResult<Matcher> {
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
            '*' | SELECTOR_SEPARATOR => Ok(Matcher::Any),
            '/' => Self::parse_regex_matcher(chars),
            _ => Err(ParseErrorReason::UnexpectedCharacter(peek_ch)),
        }
    }

    fn parse_matcher_bare(chars: &mut ParsingIterator) -> Matcher {
        let mut result = String::with_capacity(20); // just a guess
        let mut dropped = String::with_capacity(8); // also a guess

        loop {
            chars.drop_to_while(&mut dropped, |ch| ch.is_whitespace());
            let Some(ch) = chars.next() else {
                break;
            };
            if ch == SELECTOR_SEPARATOR {
                break;
            }
            result.push_str(&dropped);
            result.push(ch);
        }

        Matcher::Substring(result)
    }

    fn parse_regex_matcher(chars: &mut ParsingIterator) -> ParseResult<Matcher> {
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
    use crate::parse_common::Position;
    use indoc::indoc;

    #[test]
    fn bareword() {
        parse_and_check("hello", Matcher::Substring("hello".to_string()), "");
        parse_and_check("hello ", Matcher::Substring("hello".to_string()), "");
        parse_and_check("hello / goodbye", Matcher::Substring("hello / goodbye".to_string()), "");
        parse_and_check("hello| goodbye", Matcher::Substring("hello".to_string()), " goodbye");
        parse_and_check("hello | goodbye", Matcher::Substring("hello".to_string()), " goodbye");
    }

    #[test]
    fn regex() {
        parse_and_check(r#"/foo/"#, Matcher::Regex(Regex::new("foo").unwrap()), "");
        parse_and_check(r#"/foo /"#, Matcher::Regex(Regex::new("foo ").unwrap()), "");
        parse_and_check(r#"/foo/bar"#, Matcher::Regex(Regex::new("foo").unwrap()), "bar");
        parse_and_check(r#"//"#, Matcher::Regex(Regex::new("").unwrap()), "");
        parse_and_check(r#"/(a|b)/"#, Matcher::Regex(Regex::new("(a|b)").unwrap()), "");
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
        let mut iter = ParsingIterator::new(text);
        let matcher = Matcher::parse_matcher(&mut iter).unwrap();
        assert_eq!(matcher, expect);
        let remaining: String = iter.collect();
        assert_eq!(&remaining, expect_remaining);
    }

    fn expect_err(text: &str, expect: ParseErrorReason, at: Position) {
        let mut iter = ParsingIterator::new(text);
        let err = Matcher::parse_matcher(&mut iter).expect_err("expected to fail parsing");
        assert_eq!(iter.input_position(), at);
        assert_eq!(err, expect);
    }
}
