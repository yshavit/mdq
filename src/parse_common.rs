#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Position {
    /// 0-indexed line position
    pub line: usize,
    /// 0-indexed char column position within the line.
    pub column: usize,
}
