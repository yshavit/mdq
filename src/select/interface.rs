use crate::parse_common::Position;
use crate::tree::MdqNode;
use crate::tree_ref::MdqNodeRef;

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
