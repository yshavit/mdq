use crate::parse_common::Position;
use crate::tree::MdElem;
use crate::tree_ref::MdElemRef;

pub enum SelectResult<'a> {
    One(MdElemRef<'a>),
    Multi(&'a Vec<MdElem>),
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
