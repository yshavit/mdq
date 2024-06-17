/// An iterator that ignores newlines and counts position within the input.
pub struct ParsingIterator<I: Iterator<Item = char>> {
    iter: I,
    position: Position,
    pending: Option<char>,
}

impl<I: Iterator<Item = char>> ParsingIterator<I> {
    pub fn new(iter: I) -> Self {
        Self {
            iter,
            position: Position { line: 1, column: 0 },
            pending: None,
        }
    }

    pub fn input_position(&self) -> Position {
        self.position
    }

    pub fn peek(&mut self) -> Option<char> {
        if let pending @ Some(_) = self.pending {
            return pending;
        }
        match self.iter.next() {
            None => None,
            peek @ Some(_) => {
                self.pending = peek;
                peek
            }
        }
    }

    pub fn drop_to_while<F>(&mut self, dropped_chars: &mut String, keep_dropping: F)
    where
        F: Fn(char) -> bool,
    {
        dropped_chars.clear();
        loop {
            match self.peek() {
                None => return,
                Some(ch) => {
                    if keep_dropping(ch) {
                        dropped_chars.push(ch);
                        let _ = self.next();
                    } else {
                        return;
                    }
                }
            }
        }
    }

    pub fn drop_while<F>(&mut self, keep_dropping: F) -> String
    where
        F: Fn(char) -> bool,
    {
        let mut dropped_chars = String::with_capacity(8); // arbitrary guess
        self.drop_to_while(&mut dropped_chars, keep_dropping);
        dropped_chars
    }

    fn update_position(&mut self, ch: char) -> Option<char> {
        if ch == '\n' {
            self.position.line += 1;
            self.position.column = 0;
        } else {
            self.position.column += 1;
        }
        Some(ch)
    }
}

impl<I: Iterator<Item = char>> Iterator for ParsingIterator<I> {
    type Item = char;

    fn next(&mut self) -> Option<Self::Item> {
        if let Some(ch) = self.pending {
            self.pending = None;
            return self.update_position(ch);
        };
        match self.iter.next() {
            None => None,
            Some(ch) => self.update_position(ch),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub struct Position {
    /// 1-indexed line position
    pub line: usize,
    /// 1-indexed char column position within the line.
    pub column: usize,
}

#[cfg(test)]
mod tests {
    use crate::parsing_iter::{ParsingIterator, Position};

    #[test]
    fn basic() {
        let mut iter = ParsingIterator::new("AB\nC".chars());
        assert_eq!(iter.position, Position { line: 1, column: 0 });
        next_and_check(&mut iter, Some('A'), Position { line: 1, column: 1 });
        next_and_check(&mut iter, Some('B'), Position { line: 1, column: 2 });
        next_and_check(&mut iter, Some('\n'), Position { line: 2, column: 0 });
        next_and_check(&mut iter, Some('C'), Position { line: 2, column: 1 });

        next_and_check(&mut iter, None, Position { line: 2, column: 1 });
        // and it's unchanged if I call it again
        next_and_check(&mut iter, None, Position { line: 2, column: 1 });
    }

    #[test]
    fn empty() {
        let mut iter = ParsingIterator::new("".chars());
        next_and_check(&mut iter, None, Position { line: 1, column: 0 });
    }

    #[test]
    fn peek_after_read() {
        let mut iter = ParsingIterator::new("AB".chars());
        next_and_check(&mut iter, Some('A'), Position { line: 1, column: 1 });
        peek_and_check(&mut iter, Some('B'), Position { line: 1, column: 1 }); // position unchanged!
        next_and_check(&mut iter, Some('B'), Position { line: 1, column: 2 });
        peek_and_check(&mut iter, None, Position { line: 1, column: 2 });
        next_and_check(&mut iter, None, Position { line: 1, column: 2 });

        // no-ops
        peek_and_check(&mut iter, None, Position { line: 1, column: 2 });
        peek_and_check(&mut iter, None, Position { line: 1, column: 2 });
        next_and_check(&mut iter, None, Position { line: 1, column: 2 });
        next_and_check(&mut iter, None, Position { line: 1, column: 2 });
    }

    #[test]
    fn peek_initial() {
        let mut iter = ParsingIterator::new("A".chars());
        peek_and_check(&mut iter, Some('A'), Position { line: 1, column: 0 });
        next_and_check(&mut iter, Some('A'), Position { line: 1, column: 1 });
    }

    #[test]
    fn drop_while() {
        let mut iter = ParsingIterator::new("A \t B".chars());

        assert_eq!("", iter.drop_while(|ch| ch.is_whitespace()));
        next_and_check(&mut iter, Some('A'), Position { line: 1, column: 1 });

        assert_eq!(" \t ", iter.drop_while(|ch| ch.is_whitespace()));
        peek_and_check(&mut iter, Some('B'), Position { line: 1, column: 4 });
        next_and_check(&mut iter, Some('B'), Position { line: 1, column: 5 });

        assert_eq!("", iter.drop_while(|ch| ch.is_whitespace()));
        peek_and_check(&mut iter, None, Position { line: 1, column: 5 });
        next_and_check(&mut iter, None, Position { line: 1, column: 5 });
    }

    fn next_and_check<I>(iter: &mut ParsingIterator<I>, expect_ch: Option<char>, expect_pos: Position)
    where
        I: Iterator<Item = char>,
    {
        let next_item = iter.next();
        assert_eq!(next_item, expect_ch);
        assert_eq!(iter.input_position(), expect_pos);
    }

    fn peek_and_check<I>(iter: &mut ParsingIterator<I>, expect_ch: Option<char>, expect_pos: Position)
    where
        I: Iterator<Item = char>,
    {
        let next_item = iter.peek();
        assert_eq!(next_item, expect_ch);
        assert_eq!(iter.input_position(), expect_pos);
    }
}
