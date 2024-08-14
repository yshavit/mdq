use std::borrow::Borrow;
use std::fmt::Alignment;

use markdown::mdast::AlignKind;

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

    fn output_and_get<F>(action: F) -> String
    where
        F: FnOnce(&mut Output<String>),
    {
        let mut output = Output::new(String::new());
        action(&mut output);
        output.take_underlying().unwrap()
    }
}
