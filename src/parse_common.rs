#[cfg(test)]
pub use test_util::*;

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

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Position {
    /// 1-indexed line position
    pub line: usize,
    /// 1-indexed char column position within the line.
    pub column: usize,
}

#[cfg(test)]
mod test_util {
    use crate::parsing_iter::ParsingIterator;
    use crate::select::ParseResult;
    use std::fmt::Debug;

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
