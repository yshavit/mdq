use crate::parse_common::Position;
use crate::tree::MdqNode;
use crate::tree_ref::MdqNodeRef;

pub trait Selector<'a, I: Copy> {
    fn matches(&self, item: I) -> bool;
    fn pick(&self, item: I) -> SelectResult<'a>;

    fn try_select(&self, item: I) -> Option<SelectResult<'a>> {
        if self.matches(item) {
            Some(self.pick(item))
        } else {
            None
        }
    }
}

pub enum SelectResult<'a> {
    One(MdqNodeRef<'a>),
    Multi(&'a Vec<MdqNode>),
}

pub type ParseResult<T> = Result<T, ParseErrorReason>;

pub const SELECTOR_SEPARATOR: char = '|';

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub position: Position,
    pub reason: ParseErrorReason,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseErrorReason {
    UnexpectedCharacter(char),
    UnexpectedEndOfInput,
    InvalidSyntax(String),
}

#[cfg(test)]
pub mod test_util {
    use crate::parsing_iter::ParsingIterator;
    use crate::select::test_util::parse_selector;
    use crate::select::SelectHolder;
    use crate::selectors::base::ParseResult;
    use std::fmt::Debug;

    pub fn parse_and_check(text: &str, expect: SelectHolder, expect_remaining: &str) {
        parse_and_check_mapped(text, expect, expect_remaining, |iter| parse_selector(iter))
    }

    pub fn parse_and_check_mapped<E, F>(text: &str, expect: E, expect_remaining: &str, mapper: F)
    where
        E: PartialEq + Debug,
        F: FnOnce(&mut ParsingIterator<std::str::Chars>) -> ParseResult<E>,
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
