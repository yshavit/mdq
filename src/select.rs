use crate::fmt_str::inlines_to_plain_string;
use crate::parsing_iter::{ParsingIterator, Position};
use crate::tree::MdqNode;
use regex::Regex;

#[derive(Debug, PartialEq)]
pub enum Selector {
    Any,
    // List(Matcher, ListType, Selected),  // should this be list or list item?
    // Image { text: Matcher, href: Matcher },
    // Link { text: Matcher, href: Matcher },
    // CodeBlock(Matcher),
    Heading(HeadingSelector),
    // TODO I need an "any", or maybe just a "paragraph", or maybe both
    // TODO does it make sense to select on a block quote?
}

impl Selector {
    pub fn find_nodes<'a>(&self, nodes: Vec<&'a MdqNode>) -> Vec<&'a MdqNode> {
        let mut result = Vec::with_capacity(8); // arbitrary guess
        for node in nodes {
            self.find_nodes_one(&mut result, node);
        }
        result
    }

    // TODO need better name -- here but also in all the other methods
    pub fn find_nodes_one<'a>(&self, out: &mut Vec<&'a MdqNode>, node: &'a MdqNode) {
        match (self, node) {
            (Selector::Heading(selector), MdqNode::Header(header)) => {
                let header_text = inlines_to_plain_string(&header.title);
                if selector.matcher.matches(&header_text) {
                    header.body.iter().for_each(|child| out.push(child));
                }
            }
            (_, MdqNode::Root(root)) => {
                for node in &root.body {
                    self.find_nodes_one(out, node);
                }
            }
            _ => {
                // TODO better recursion
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct HeadingSelector {
    matcher: Matcher,
}

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
    fn matches(&self, haystack: &str) -> bool {
        match self {
            Matcher::Any => true,
            Matcher::Substring(SubstringMatcher { look_for }) => haystack.contains(look_for),
            Matcher::Regex(re) => re.is_match(haystack),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct SubstringMatcher {
    pub look_for: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    position: Position,
    reason: ParseErrorReason,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseErrorReason {
    UnexpectedCharacter(char),
    UnexpectedEndOfInput,
    InvalidSyntax(String),
}

pub fn parse_selectors(text: &str) -> core::result::Result<Vec<Selector>, ParseError> {
    let mut iter = ParsingIterator::new(text.chars());
    let mut selectors = Vec::with_capacity(5); // just a guess

    loop {
        iter.drop_while(|ch| ch.is_whitespace());
        if iter.peek().is_none() {
            break;
        }
        let selector = Parser::parse_selector(&mut iter).map_err(|reason| ParseError {
            reason,
            position: iter.input_position(),
        })?;
        selectors.push(selector);
    }

    Ok(selectors)
}

/// just a helper so that I don't have to keep repeating the `C: Iterator<Item = char>` bounds.
/// TODO is there a better way to do that?
/// TODO ooor, I could use this parser to maintain a String that I can use as a scratch space, which has an initial
/// capacity enough for the original string.
struct Parser<C>(std::marker::PhantomData<C>);
type Result<T> = core::result::Result<T, ParseErrorReason>;

impl<C> Parser<C>
where
    C: Iterator<Item = char>,
{
    fn parse_selector(chars: &mut ParsingIterator<C>) -> Result<Selector> {
        chars.drop_while(|ch| ch.is_whitespace()); // should already be the case, but this is cheap and future-proof
        match chars.next() {
            None => Ok(Selector::Any), // unexpected, but future-proof
            Some('#') => Self::parse_header(chars),

            Some(other) => Err(ParseErrorReason::UnexpectedCharacter(other)),
        }
    }

    fn parse_header(chars: &mut ParsingIterator<C>) -> Result<Selector> {
        let matcher = Self::parse_matcher(chars)?;
        Ok(Selector::Heading(HeadingSelector { matcher }))
    }

    fn parse_matcher(chars: &mut ParsingIterator<C>) -> Result<Matcher> {
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

    fn parse_matcher_bare(chars: &mut ParsingIterator<C>) -> Matcher {
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

    fn parse_regex_matcher(chars: &mut ParsingIterator<C>) -> Result<Matcher> {
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
    use std::fmt::Debug;

    mod parse_selector {
        use super::*;

        #[test]
        fn header() {
            parse_and_check(
                "# foo",
                Selector::Heading(HeadingSelector {
                    matcher: Matcher::Substring(SubstringMatcher {
                        look_for: "foo".to_string(),
                    }),
                }),
                "",
            );

            parse_and_check("# ", Selector::Heading(HeadingSelector { matcher: Matcher::Any }), "");

            parse_and_check(
                "# // next",
                Selector::Heading(HeadingSelector { matcher: Matcher::Any }),
                " next",
            );
        }

        fn parse_and_check(text: &str, expect: Selector, expect_remaining: &str) {
            parse_and_check_mapped(text, expect, expect_remaining, |iter| Parser::parse_selector(iter))
        }
    }

    mod parse_matcher {
        use super::*;
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
            parse_and_check_mapped(text, expect, expect_remaining, |iter| Parser::parse_matcher(iter))
        }

        fn expect_err(text: &str, expect: ParseErrorReason, at: Position) {
            let mut iter = ParsingIterator::new(text.chars());
            let err = Parser::parse_matcher(&mut iter).expect_err("expected to fail parsing");
            assert_eq!(iter.input_position(), at);
            assert_eq!(err, expect);
        }
    }

    fn parse_and_check_mapped<E, F>(text: &str, expect: E, expect_remaining: &str, mapper: F)
    where
        E: PartialEq + Debug,
        F: FnOnce(&mut ParsingIterator<std::str::Chars>) -> Result<E>,
    {
        let mut iter = ParsingIterator::new(text.chars());
        let actual = mapper(&mut iter).expect("failure in the test itself");
        assert_eq!(actual, expect);
        let mut actual_remaining = String::new();
        while let Some(ch) = iter.next() {
            actual_remaining.push(ch);
        }
        assert_eq!(&actual_remaining, expect_remaining);
    }
}
