use crate::md_elem::elem::ColumnAlignment;
use crate::util::output::{Output, SimpleWrite};

pub fn pad_to<W>(output: &mut Output<W>, input: &str, min_width: usize, alignment: Option<ColumnAlignment>)
where
    W: SimpleWrite,
{
    if input.len() >= min_width {
        return output.write_str(input);
    }

    let padding = min_width - input.len();

    match alignment {
        Some(ColumnAlignment::Left) | None => {
            output.write_str(input);
            (0..padding).for_each(|_| output.write_char(' '));
        }
        Some(ColumnAlignment::Center) => {
            let left_pad = padding / 2; // round down
            let right_pad = padding - left_pad;
            (0..left_pad).for_each(|_| output.write_char(' '));
            output.write_str(input);
            (0..right_pad).for_each(|_| output.write_char(' '));
        }
        Some(ColumnAlignment::Right) => {
            (0..padding).for_each(|_| output.write_char(' '));
            output.write_str(input);
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
        for ch in text.chars() {
            self.write_char(ch)?;
        }
        Ok(())
    }

    pub fn count(&self) -> usize {
        self.count
    }
}

impl<W: SimpleWrite> SimpleWrite for CountingWriter<'_, W> {
    fn write_char(&mut self, ch: char) -> std::io::Result<()> {
        self.underlying.write_char(ch)?;
        self.count += 1;
        Ok(())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.underlying.flush()
    }
}

impl<W: SimpleWrite> std::fmt::Write for CountingWriter<'_, W> {
    fn write_str(&mut self, text: &str) -> std::fmt::Result {
        Self::write_str(self, text).map_err(|_| std::fmt::Error)
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn left_pad() {
        assert_eq!(
            "a    ",
            output_and_get(|out| pad_to(out, "a", 5, Some(ColumnAlignment::Left)))
        );
    }

    #[test]
    fn right_pad() {
        assert_eq!(
            "    a",
            output_and_get(|out| pad_to(out, "a", 5, Some(ColumnAlignment::Right)))
        );
    }

    /// center pad, with the same amount of padding on each side
    #[test]
    fn center_pad_even() {
        assert_eq!(
            "  a  ",
            output_and_get(|out| pad_to(out, "a", 5, Some(ColumnAlignment::Center)))
        );
    }

    /// center pad, with different amount of padding on each side
    #[test]
    fn center_pad_uneven() {
        assert_eq!(
            " ab  ",
            output_and_get(|out| pad_to(out, "ab", 5, Some(ColumnAlignment::Center)))
        );
    }

    #[test]
    fn string_already_right_size() {
        for align in [ColumnAlignment::Left, ColumnAlignment::Center, ColumnAlignment::Right] {
            assert_eq!("abcde", output_and_get(|out| pad_to(out, "abcde", 5, Some(align))));
        }
    }

    #[test]
    fn string_already_too_big() {
        for align in [ColumnAlignment::Left, ColumnAlignment::Center, ColumnAlignment::Right] {
            assert_eq!("abcdef", output_and_get(|out| pad_to(out, "abcdef", 3, Some(align))));
        }
    }

    fn output_and_get<F>(action: F) -> String
    where
        F: FnOnce(&mut Output<String>),
    {
        let mut output = Output::without_text_wrapping(String::new());
        action(&mut output);
        output.take_underlying().unwrap()
    }
}
