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
