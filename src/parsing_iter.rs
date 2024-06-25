use crate::parse_common::Position;
use crate::select::{ParseErrorReason, ParseResult};
use std::str::Chars;

/// An iterator that ignores newlines and counts position within the input.
pub struct ParsingIterator<'a> {
    iter: Chars<'a>,
    position: Position,
    pending: Option<char>,
}

impl<'a> ParsingIterator<'a> {
    pub fn new(from: &'a str) -> Self {
        Self {
            iter: from.chars(),
            position: Position { line: 0, column: 0 },
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

    pub fn drop_whitespace(&mut self) -> String {
        self.drop_while(|ch| ch.is_whitespace())
    }

    pub fn require_whitespace(&mut self, description: &str) -> ParseResult<()> {
        if self.drop_while(|ch| ch.is_whitespace()).is_empty() && self.peek().is_some() {
            return Err(ParseErrorReason::InvalidSyntax(format!(
                "{} must be followed by whitespace",
                description
            )));
        } else {
            Ok(())
        }
    }

    pub fn require_char(&mut self, ch: char) -> ParseResult<()> {
        self.require_char_or_else(ch, || ParseErrorReason::Expected(ch))
    }

    pub fn require_char_or_else<F>(&mut self, ch: char, or_else: F) -> ParseResult<()>
    where
        F: FnOnce() -> ParseErrorReason,
    {
        match self.next() {
            Some(actual) if actual == ch => Ok(()),
            _ => Err(or_else()),
        }
    }

    pub fn require_str(&mut self, s: &str) -> ParseResult<()> {
        for ch in s.chars() {
            self.require_char(ch)?;
        }
        Ok(())
    }

    pub fn consume_if(&mut self, ch: char) -> bool {
        match self.peek() {
            Some(actual) if actual == ch => {
                self.next();
                true
            }
            _ => false,
        }
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

impl Iterator for ParsingIterator<'_> {
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

#[cfg(test)]
mod tests {
    use crate::parsing_iter::{ParsingIterator, Position};
    use crate::select::ParseErrorReason;

    #[test]
    fn basic() {
        let mut iter = ParsingIterator::new("AB\nC");
        assert_eq!(iter.position, Position { line: 0, column: 0 });
        next_and_check(&mut iter, Some('A'), Position { line: 0, column: 1 });
        next_and_check(&mut iter, Some('B'), Position { line: 0, column: 2 });
        next_and_check(&mut iter, Some('\n'), Position { line: 1, column: 0 });
        next_and_check(&mut iter, Some('C'), Position { line: 1, column: 1 });

        next_and_check(&mut iter, None, Position { line: 1, column: 1 });
        // and it's unchanged if I call it again
        next_and_check(&mut iter, None, Position { line: 1, column: 1 });
    }

    #[test]
    fn empty() {
        let mut iter = ParsingIterator::new("");
        next_and_check(&mut iter, None, Position { line: 0, column: 0 });
    }

    #[test]
    fn peek_after_read() {
        let mut iter = ParsingIterator::new("AB");
        next_and_check(&mut iter, Some('A'), Position { line: 0, column: 1 });
        peek_and_check(&mut iter, Some('B'), Position { line: 0, column: 1 }); // position unchanged!
        next_and_check(&mut iter, Some('B'), Position { line: 0, column: 2 });
        peek_and_check(&mut iter, None, Position { line: 0, column: 2 });
        next_and_check(&mut iter, None, Position { line: 0, column: 2 });

        // no-ops
        peek_and_check(&mut iter, None, Position { line: 0, column: 2 });
        peek_and_check(&mut iter, None, Position { line: 0, column: 2 });
        next_and_check(&mut iter, None, Position { line: 0, column: 2 });
        next_and_check(&mut iter, None, Position { line: 0, column: 2 });
    }

    #[test]
    fn peek_initial() {
        let mut iter = ParsingIterator::new("A");
        peek_and_check(&mut iter, Some('A'), Position { line: 0, column: 0 });
        next_and_check(&mut iter, Some('A'), Position { line: 0, column: 1 });
    }

    #[test]
    fn drop_while() {
        let mut iter = ParsingIterator::new("A \t B");

        assert_eq!(iter.drop_while(|ch| ch.is_whitespace()), "",);
        next_and_check(&mut iter, Some('A'), Position { line: 0, column: 1 });

        assert_eq!(iter.drop_while(|ch| ch.is_whitespace()), " \t ");
        peek_and_check(&mut iter, Some('B'), Position { line: 0, column: 4 });
        next_and_check(&mut iter, Some('B'), Position { line: 0, column: 5 });

        assert_eq!(iter.drop_while(|ch| ch.is_whitespace()), "");
        peek_and_check(&mut iter, None, Position { line: 0, column: 5 });
        next_and_check(&mut iter, None, Position { line: 0, column: 5 });
    }

    #[test]
    fn drop_whitespace() {
        let mut iter = ParsingIterator::new("  \t\r\n\t  B");
        assert_eq!(iter.drop_whitespace(), "  \t\r\n\t  ");
        assert_eq!(iter.next(), Some('B'));
    }

    #[test]
    fn require_whitespace() {
        let mut iter = ParsingIterator::new("  BC");
        assert_eq!(iter.require_whitespace("foo"), Ok(()));
        assert_eq!(iter.next(), Some('B'));
        assert_eq!(
            iter.require_whitespace("bar"),
            Err(ParseErrorReason::InvalidSyntax(
                "bar must be followed by whitespace".to_string()
            ))
        );
        assert_eq!(iter.next(), Some('C'));
        assert_eq!(iter.require_whitespace("foo"), Ok(()),); // EOL counts as whitespace
    }

    #[test]
    fn require_char() {
        let mut iter = ParsingIterator::new("AB");
        assert_eq!(iter.require_char('A'), Ok(()));
        assert_eq!(iter.require_char('F'), Err(ParseErrorReason::Expected('F')));
    }

    #[test]
    fn require_str() {
        let mut iter = ParsingIterator::new("ABCD");
        assert_eq!(iter.require_str("ABC"), Ok(()));
        assert_eq!(iter.require_str("FGH"), Err(ParseErrorReason::Expected('F')));
    }

    #[test]
    fn consume_if() {
        let mut iter = ParsingIterator::new("ABC");
        assert_eq!(iter.consume_if('A'), true);
        assert_eq!(iter.position, Position { line: 0, column: 1 });
        assert_eq!(iter.consume_if('F'), false);
        assert_eq!(iter.position, Position { line: 0, column: 1 });
    }

    fn next_and_check(iter: &mut ParsingIterator, expect_ch: Option<char>, expect_pos: Position) {
        let next_item = iter.next();
        assert_eq!(next_item, expect_ch);
        assert_eq!(iter.input_position(), expect_pos);
    }

    fn peek_and_check(iter: &mut ParsingIterator, expect_ch: Option<char>, expect_pos: Position) {
        let next_item = iter.peek();
        assert_eq!(next_item, expect_ch);
        assert_eq!(iter.input_position(), expect_pos);
    }
}
