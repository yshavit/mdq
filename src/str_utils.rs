use markdown::mdast::AlignKind;
use std::borrow::Borrow;
use std::fmt::Alignment;
use std::iter::Peekable;
use std::str::CharIndices;

use crate::output::{Output, SimpleWrite};

pub fn pad_to<A, W>(output: &mut Output<W>, input: &str, min_width: usize, alignment: A)
where
    A: ToAlignment,
    W: SimpleWrite,
{
    if input.len() >= min_width {
        return output.write_str(input);
    }

    let padding = min_width - input.len();

    match standard_align(alignment) {
        Some(Alignment::Left) | None => {
            output.write_str(input);
            (0..padding).for_each(|_| output.write_char(' '));
        }
        Some(Alignment::Center) => {
            let left_pad = padding / 2; // round down
            let right_pad = padding - left_pad;
            (0..left_pad).for_each(|_| output.write_char(' '));
            output.write_str(input);
            (0..right_pad).for_each(|_| output.write_char(' '));
        }
        Some(Alignment::Right) => {
            (0..padding).for_each(|_| output.write_char(' '));
            output.write_str(input);
        }
    }
}

pub fn standard_align<A>(mdast_align: A) -> Option<Alignment>
where
    A: ToAlignment,
{
    mdast_align.to_alignment()
}

pub trait ToAlignment {
    fn to_alignment(self) -> Option<Alignment>;
}

impl ToAlignment for Alignment {
    fn to_alignment(self) -> Option<Alignment> {
        Some(self)
    }
}

impl ToAlignment for AlignKind {
    fn to_alignment(self) -> Option<Alignment> {
        match self {
            AlignKind::Left => Some(Alignment::Left),
            AlignKind::Right => Some(Alignment::Right),
            AlignKind::Center => Some(Alignment::Center),
            AlignKind::None => None,
        }
    }
}

impl<A: Borrow<AlignKind>> ToAlignment for Option<A> {
    fn to_alignment(self) -> Option<Alignment> {
        match self {
            Some(a) => a.borrow().to_alignment(),
            None => None,
        }
    }
}

pub struct CountingWriter<'a, W> {
    underlying: &'a mut W,
    count: usize,
}

impl<'a, W: SimpleWrite> CountingWriter<'a, W> {
    pub fn wrap(underlying: &'a mut W) -> Self {
        Self { underlying, count: 0 }
    }

    fn write_str(&mut self, text: &str) -> std::io::Result<()> {
        self.count += text.len();
        self.underlying.write_str(text)
    }

    pub fn count(&self) -> usize {
        self.count
    }
}

impl<'a, W: SimpleWrite> SimpleWrite for CountingWriter<'a, W> {
    fn write_str(&mut self, text: &str) -> std::io::Result<()> {
        Self::write_str(self, text)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.underlying.flush()
    }
}

impl<'a, W: SimpleWrite> std::fmt::Write for CountingWriter<'a, W> {
    fn write_str(&mut self, text: &str) -> std::fmt::Result {
        Self::write_str(self, text).map_err(|_| std::fmt::Error)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub enum WhitespaceSplit<'a> {
    /// A stretch of at least one whitespace.
    Whitespace,
    /// A word, along with its length in chars.
    ///
    /// The length component is the same as `word.chars().count()`, but is provided here for
    /// convenience and efficiency (since the iterator would have had to find the chars to do the
    /// split, anyway).
    Word(&'a str, usize),
}

pub struct WhitespaceSplitter<'a> {
    text: &'a str,
    chars: Peekable<CharIndices<'a>>,
}

impl<'a> From<&'a str> for WhitespaceSplitter<'a> {
    fn from(text: &'a str) -> Self {
        Self {
            text,
            chars: text.char_indices().peekable(),
        }
    }
}

impl<'a> Iterator for WhitespaceSplitter<'a> {
    type Item = WhitespaceSplit<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        let Some((next_idx, next_ch)) = self.chars.next() else {
            return None;
        };
        if next_ch.is_ascii_whitespace() {
            while matches!(self.chars.peek(), Some((_, future_ch)) if future_ch.is_whitespace()) {
                self.chars.next();
            }
            Some(WhitespaceSplit::Whitespace)
        } else {
            let start_idx = next_idx;
            let mut end_idx_inclusive = start_idx;
            let mut chars_count = 1; // start at 1 since we already have the first char
            while let Some((future_idx, future_ch)) = self.chars.peek() {
                if future_ch.is_ascii_whitespace() {
                    break;
                }
                end_idx_inclusive = *future_idx;
                self.chars.next();
                chars_count += 1;
            }
            Some(WhitespaceSplit::Word(
                &self.text[start_idx..=end_idx_inclusive],
                chars_count,
            ))
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn left_pad() {
        assert_eq!("a    ", output_and_get(|out| pad_to(out, "a", 5, Alignment::Left)));
    }

    #[test]
    fn right_pad() {
        assert_eq!("    a", output_and_get(|out| pad_to(out, "a", 5, Alignment::Right)));
    }

    /// center pad, with the same amount of padding on each side
    #[test]
    fn center_pad_even() {
        assert_eq!("  a  ", output_and_get(|out| pad_to(out, "a", 5, Alignment::Center)));
    }

    /// center pad, with different amount of padding on each side
    #[test]
    fn center_pad_uneven() {
        assert_eq!(" ab  ", output_and_get(|out| pad_to(out, "ab", 5, Alignment::Center)));
    }

    #[test]
    fn string_already_right_size() {
        for align in [Alignment::Left, Alignment::Center, Alignment::Right] {
            assert_eq!("abcde", output_and_get(|out| pad_to(out, "abcde", 5, align)));
        }
    }

    #[test]
    fn string_already_too_big() {
        for align in [Alignment::Left, Alignment::Center, Alignment::Right] {
            assert_eq!("abcdef", output_and_get(|out| pad_to(out, "abcdef", 3, align)));
        }
    }

    mod whitespace_splitter {
        use super::*;

        #[test]
        fn empty() {
            assert_eq!(get_splits(""), vec![]);
        }

        #[test]
        fn all_whitespace() {
            assert_eq!(get_splits(" \t\n\r "), vec![WhitespaceSplit::Whitespace]);
        }

        #[test]
        fn all_word() {
            assert_eq!(get_splits("hello"), vec![WhitespaceSplit::Word("hello", 5)]);
        }

        #[test]
        fn mixed_start_and_end_with_whitespace() {
            assert_eq!(
                get_splits(" hello "),
                vec![
                    WhitespaceSplit::Whitespace,
                    WhitespaceSplit::Word("hello", 5),
                    WhitespaceSplit::Whitespace,
                ]
            );
        }

        #[test]
        fn mixed_start_and_end_with_word() {
            assert_eq!(
                get_splits("hello, world"),
                vec![
                    WhitespaceSplit::Word("hello,", 6),
                    WhitespaceSplit::Whitespace,
                    WhitespaceSplit::Word("world", 5),
                ]
            );
        }

        fn get_splits(text: &str) -> Vec<WhitespaceSplit> {
            WhitespaceSplitter::from(text).collect()
        }
    }

    fn output_and_get<F>(action: F) -> String
    where
        F: FnOnce(&mut Output<String>),
    {
        let mut output = Output::new(String::new());
        action(&mut output);
        output.take_underlying().unwrap()
    }
}
