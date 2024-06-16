/// An iterator that ignores newlines and counts position within the input.
struct ParsingIterator<I: Iterator<Item = char>> {
    iter: I,
    position: Position,
}

impl<I: Iterator<Item = char>> ParsingIterator<I> {
    pub fn new(iter: I) -> Self {
        Self {
            iter,
            position: Position { line: 1, column: 0 },
        }
    }
}

impl<I: Iterator<Item = char>> Iterator for ParsingIterator<I> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.iter.next() {
                None => return None,
                Some('\n') => {
                    self.position.line += 1;
                    self.position.column = 0;
                }
                Some(ch) => {
                    self.position.column += 1;
                    return Some(ch);
                }
            }
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
struct Position {
    /// 1-indexed line position
    line: usize,
    /// 1-indexed char column position within the line.
    column: usize,
}

#[cfg(test)]
mod tests {
    use crate::parsing_iter::{ParsingIterator, Position};

    #[test]
    fn basic() {
        let mut iter = ParsingIterator::new("ABC\nD\n\nEF".chars());
        assert_eq!(iter.position, Position { line: 1, column: 0 });
        next_and_check(&mut iter, Some('A'), Position { line: 1, column: 1 });
        next_and_check(&mut iter, Some('B'), Position { line: 1, column: 2 });
        next_and_check(&mut iter, Some('C'), Position { line: 1, column: 3 });
        next_and_check(&mut iter, Some('D'), Position { line: 2, column: 1 });
        next_and_check(&mut iter, Some('E'), Position { line: 4, column: 1 });
        next_and_check(&mut iter, Some('F'), Position { line: 4, column: 2 });
        next_and_check(&mut iter, None, Position { line: 4, column: 2 });
        // and it's unchanged if I call it again
        next_and_check(&mut iter, None, Position { line: 4, column: 2 });
    }

    #[test]
    fn empty() {
        let mut iter = ParsingIterator::new("".chars());
        next_and_check(&mut iter, None, Position { line: 1, column: 0 });
    }

    #[test]
    fn prefix_and_trailing_newlines() {
        let mut iter = ParsingIterator::new("\nA\n\n".chars());
        next_and_check(&mut iter, Some('A'), Position { line: 2, column: 1 });
        next_and_check(&mut iter, None, Position { line: 4, column: 0 });
        next_and_check(&mut iter, None, Position { line: 4, column: 0 });
    }

    #[test]
    fn only_newlines() {
        let mut iter = ParsingIterator::new("\n\n\n".chars());
        next_and_check(&mut iter, None, Position { line: 4, column: 0 });
        next_and_check(&mut iter, None, Position { line: 4, column: 0 });
    }

    fn next_and_check<I>(iter: &mut ParsingIterator<I>, expect_ch: Option<char>, expect_pos: Position)
    where
        I: Iterator<Item = char>,
    {
        let next_item = iter.next();
        assert_eq!(next_item, expect_ch);
        assert_eq!(iter.position, expect_pos);
    }
}
