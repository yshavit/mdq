use std::borrow::Borrow;
use std::fmt::Alignment;
use std::io::Write;

use markdown::mdast::AlignKind;

use crate::output::Output;

pub fn pad_to<A, W>(output: &mut Output<W>, input: &str, min_width: usize, alignment: A)
where
    A: ToAlignment,
    W: Write,
{
    if input.len() >= min_width {
        return output.write_str(input);
    }

    let padding = min_width - input.len();

    match standard_align(alignment) {
        Alignment::Left => {
            output.write_str(input);
            (0..padding).for_each(|_| output.write_char(' '));
        }
        Alignment::Center => {
            let left_pad = padding / 2; // round down
            let right_pad = padding - left_pad;
            (0..left_pad).for_each(|_| output.write_char(' '));
            output.write_str(input);
            (0..right_pad).for_each(|_| output.write_char(' '));
        }
        Alignment::Right => {
            (0..padding).for_each(|_| output.write_char(' '));
            output.write_str(input);
        }
    }
}

pub fn standard_align<A>(mdast_align: A) -> Alignment
where
    A: ToAlignment,
{
    mdast_align.to_alignment()
}

const DEFAULT_ALIGNMENT: Alignment = Alignment::Left;

pub trait ToAlignment {
    fn to_alignment(&self) -> Alignment;
}

impl ToAlignment for Alignment {
    fn to_alignment(&self) -> Alignment {
        *self
    }
}

impl ToAlignment for &AlignKind {
    fn to_alignment(&self) -> Alignment {
        match self {
            AlignKind::Left => Alignment::Left,
            AlignKind::Right => Alignment::Right,
            AlignKind::Center => Alignment::Center,
            _ => DEFAULT_ALIGNMENT,
        }
    }
}

impl<A: Borrow<AlignKind>> ToAlignment for Option<A> {
    fn to_alignment(&self) -> Alignment {
        match self {
            Some(a) => a.borrow().to_alignment(),
            None => DEFAULT_ALIGNMENT,
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn left_pad() {
        assert_eq!(
            "a    ",
            output_and_get(|out| pad_to(out, "a", 5, Alignment::Left))
        );
    }

    #[test]
    fn right_pad() {
        assert_eq!(
            "    a",
            output_and_get(|out| pad_to(out, "a", 5, Alignment::Right))
        );
    }

    /// center pad, with the same amount of padding on each side
    #[test]
    fn center_pad_even() {
        assert_eq!(
            "  a  ",
            output_and_get(|out| pad_to(out, "a", 5, Alignment::Center))
        );
    }

    /// center pad, with different amount of padding on each side
    #[test]
    fn center_pad_uneven() {
        assert_eq!(
            " ab  ",
            output_and_get(|out| pad_to(out, "ab", 5, Alignment::Center))
        );
    }

    #[test]
    fn string_already_right_size() {
        for align in [Alignment::Left, Alignment::Center, Alignment::Right] {
            assert_eq!(
                "abcde",
                output_and_get(|out| pad_to(out, "abcde", 5, align))
            );
        }
    }

    #[test]
    fn string_already_too_big() {
        for align in [Alignment::Left, Alignment::Center, Alignment::Right] {
            assert_eq!(
                "abcdef",
                output_and_get(|out| pad_to(out, "abcdef", 3, align))
            );
        }
    }

    fn output_and_get<F>(action: F) -> String
    where
        F: FnOnce(&mut Output<Vec<u8>>),
    {
        let vec = Vec::with_capacity(16);
        let mut output = Output::new(vec);
        action(&mut output);
        let vec = output.take_underlying().unwrap();
        String::from_utf8(vec).unwrap()
    }
}
