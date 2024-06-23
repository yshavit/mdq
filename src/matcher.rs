use crate::fmt_str::inlines_to_plain_string;
use crate::parsing_iter::ParsingIterator;
use crate::select::{ParseErrorReason, ParseResult, SELECTOR_SEPARATOR};
use crate::tree::{Block, Inline, LeafBlock, MdqNode};
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
    pub fn matches(&self, haystack: &str) -> bool {
        match self {
            StringMatcher::Any => true,
            StringMatcher::Substring(look_for) => haystack.contains(look_for),
            StringMatcher::Regex(re) => re.is_match(haystack),
        }
    }

    pub fn matches_inlines<I: Borrow<Inline>>(&self, haystack: &[I]) -> bool {
        self.matches(&inlines_to_plain_string(haystack))
    }

    pub fn matches_any<N: Borrow<MdqNode>>(&self, haystacks: &[N]) -> bool {
        if matches!(self, StringMatcher::Any) {
            return true;
        }
        haystacks.iter().any(|node| self.matches_node(node.borrow()))
    }

    fn matches_block(&self, block: &Block) -> bool {
        match block {
            Block::LeafBlock(LeafBlock::Paragraph(p)) => self.matches_inlines(&p.body),
            Block::LeafBlock(LeafBlock::ThematicBreak | LeafBlock::CodeBlock(_)) => false,
        }
    }

    fn matches_node(&self, node: &MdqNode) -> bool {
        match node {
            MdqNode::Block(block) => self.matches_block(block),

            MdqNode::Section(section) => {
                if self.matches_inlines(&section.title) {
                    return true;
                }
                self.matches_any(&section.body)
            }
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
            MdqNode::Inline(inline) => self.matches_inlines(&[inline]),
        }
    }

    pub fn read(chars: &mut ParsingIterator) -> ParseResult<StringMatcher> {
        chars.drop_while(|ch| ch.is_whitespace());
        let peek_ch = match chars.peek() {
            None => return Ok(StringMatcher::Any),
            Some(ch) => ch,
        };
        if peek_ch.is_alphanumeric() {
            return Ok(Self::parse_matcher_bare(chars));
        }
        let _ = chars.next(); // drop the char we peeked
        match peek_ch {
            '*' | SELECTOR_SEPARATOR => Ok(StringMatcher::Any),
            '/' => Self::parse_regex_matcher(chars),
            _ => Err(ParseErrorReason::UnexpectedCharacter(peek_ch)),
        }
    }

    fn parse_matcher_bare(chars: &mut ParsingIterator) -> StringMatcher {
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

        StringMatcher::Substring(result)
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
            " goodbye",
        );
        parse_and_check(
            "hello | goodbye",
            StringMatcher::Substring("hello".to_string()),
            " goodbye",
        );
    }

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

    fn parse_and_check(text: &str, expect: StringMatcher, expect_remaining: &str) {
        let mut iter = ParsingIterator::new(text);
        let matcher = StringMatcher::read(&mut iter).unwrap();
        assert_eq!(matcher, expect);
        let remaining: String = iter.collect();
        assert_eq!(&remaining, expect_remaining);
    }

    fn expect_err(text: &str, expect: ParseErrorReason, at: Position) {
        let mut iter = ParsingIterator::new(text);
        let err = StringMatcher::read(&mut iter).expect_err("expected to fail parsing");
        assert_eq!(iter.input_position(), at);
        assert_eq!(err, expect);
    }
}
