use crate::fmt_str::inlines_to_plain_string;
use crate::matcher::Matcher;
use crate::parse_common::{ParseError, ParseErrorReason};
use crate::parsing_iter::ParsingIterator;
use crate::tree::MdqNode;

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

#[derive(Debug, PartialEq)]
pub struct SubstringMatcher {
    pub look_for: String,
}

pub fn parse_selectors(text: &str) -> Result<Vec<Selector>, ParseError> {
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
pub type ParseResult<T> = Result<T, ParseErrorReason>;

impl<C> Parser<C>
where
    C: Iterator<Item = char>,
{
    fn parse_selector(chars: &mut ParsingIterator<C>) -> ParseResult<Selector> {
        chars.drop_while(|ch| ch.is_whitespace()); // should already be the case, but this is cheap and future-proof
        match chars.next() {
            None => Ok(Selector::Any), // unexpected, but future-proof
            Some('#') => Self::parse_header(chars),

            Some(other) => Err(ParseErrorReason::UnexpectedCharacter(other)),
        }
    }

    fn parse_header(chars: &mut ParsingIterator<C>) -> ParseResult<Selector> {
        let matcher = Matcher::parse_matcher(chars)?;
        Ok(Selector::Heading(HeadingSelector { matcher }))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parse_common::parse_and_check_mapped;

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
