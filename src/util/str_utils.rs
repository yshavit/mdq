use crate::md_elem::elem::ColumnAlignment;
use crate::util::output::{Output, SimpleWrite};

pub(crate) fn pad_to<W>(output: &mut Output<W>, input: &str, min_width: usize, alignment: Option<ColumnAlignment>)
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

pub(crate) struct CountingWriter<'a, W> {
    underlying: &'a mut W,
    count: usize,
}

impl<'a, W: SimpleWrite> CountingWriter<'a, W> {
    pub(crate) fn wrap(underlying: &'a mut W) -> Self {
        Self { underlying, count: 0 }
    }

    fn write_str(&mut self, text: &str) -> std::io::Result<()> {
        for ch in text.chars() {
            self.write_char(ch)?;
        }
        Ok(())
    }

    pub(crate) fn count(&self) -> usize {
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

/// A struct that represents trimmed leading empty lines from a string.
///
/// An "empty line" is defined as a line that consists of zero or more whitespace characters,
/// and nothing else.
pub(crate) struct TrimmedEmptyLines<S> {
    pub(crate) trimmed: S,
    pub(crate) remaining: S,
}

impl<'a> From<&'a str> for TrimmedEmptyLines<&'a str> {
    fn from(s: &'a str) -> Self {
        let mut start = 0;
        // using split_inclusive() instead of just split() because we need to count \r\n as 2 chars; so we can't just take
        // the split()s, and assume a one-char newline for each one.
        for line in s.split_inclusive('\n') {
            if line.chars().all(|c| c.is_whitespace()) {
                start += line.len();
            } else {
                break;
            }
        }
        Self {
            trimmed: &s[..start],
            remaining: &s[start..],
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

    mod trim_leading_empty_lines {
        use super::*;

        #[test]
        fn starts_with_newline() {
            check("\nhello\nworld", "hello\nworld");
        }

        #[test]
        fn starts_with_space_then_newline() {
            check("  \nhello\nworld", "hello\nworld");
        }

        #[test]
        fn starts_with_space_then_char() {
            check("  a\nhello\nworld", "  a\nhello\nworld");
        }

        #[test]
        fn starts_with_char() {
            check("hello world", "hello world");
        }

        #[test]
        fn empty() {
            check("", "");
        }

        #[test]
        fn all_newlines() {
            check("\n\n\n", "");
        }

        #[test]
        fn crlf() {
            check("\r\n\r\nhello", "hello");
        }

        #[test]
        fn just_cr() {
            check("\rhello", "\rhello");
        }

        fn check(given: &str, expected: &str) {
            assert_eq!(TrimmedEmptyLines::from(given).remaining, expected);
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
