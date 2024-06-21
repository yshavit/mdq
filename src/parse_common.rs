#[cfg(test)]
pub use crate::selectors::base::test_util::*;

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Position {
    /// 1-indexed line position
    pub line: usize,
    /// 1-indexed char column position within the line.
    pub column: usize,
}
