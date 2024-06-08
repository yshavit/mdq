use std::borrow::Borrow;
use std::fmt::Alignment;

use markdown::mdast::AlignKind;

use crate::output::Output;

pub fn pad_to<A, W>(output: &mut W, input: &str, min_width: usize, alignment: A)
where
    A: ToAlignment,
    W: Paddable,
{
    if input.len() >= min_width {
        return output.write_str(input);
    }

    let padding = min_width - input.len();

    match standard_align(alignment) {
        Alignment::Left => {
            output.write_str(input);
            (0..padding).for_each(|_| output.write_ch(' '));
        }
        Alignment::Center => {
            let left_pad = padding / 2; // round down
            let right_pad = padding - left_pad;
            (0..left_pad).for_each(|_| output.write_ch(' '));
            output.write_str(input);
            (0..right_pad).for_each(|_| output.write_ch(' '));
        }
        Alignment::Right => {
            (0..padding).for_each(|_| output.write_ch(' '));
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

pub trait Paddable {
    fn write_ch(&mut self, ch: char);
    fn write_str(&mut self, string: &str);
}

impl<W: std::io::Write> Paddable for Output<W> {
    fn write_ch(&mut self, ch: char) {
        self.write_char(ch);
    }

    fn write_str(&mut self, string: &str) {
        Output::write_str(self, string); // writing it in UFCS format so it's clear it's not infinite recursion
    }
}

#[cfg(test)]
mod test {
    use super::*;

// TODO

    impl Paddable for String {
        fn write_ch(&mut self, ch: char) {
            self.push(ch);
        }

        fn write_str(&mut self, string: &str) {
            self.push_str(string);
        }
    }
}
