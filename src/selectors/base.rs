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
