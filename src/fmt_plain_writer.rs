use std::cmp::min;
use std::io::Write;

pub struct NewlineCollapser<W> {
    max_newlines: usize,
    underlying: W,
    /// How many newlines are in this current stretch, or None if we haven't written anything yet.
    current_newline_stretch: Option<usize>,
}

impl<W> NewlineCollapser<W>
where
    W: Write,
{
    pub fn new(underlying: W, max_newlines: usize) -> Self {
        Self {
            max_newlines,
            underlying,
            current_newline_stretch: None,
        }
    }

    pub fn take_underlying(self) -> W {
        self.underlying
    }

    fn flush_newlines(&mut self) -> std::io::Result<()> {
        if let Some(newlines) = self.current_newline_stretch {
            for _ in 0..min(newlines, self.max_newlines) {
                writeln!(self.underlying)?;
            }
        }
        // Set the current stretch to 0 -- not to None, since we want to note here that we've written something!
        self.current_newline_stretch = Some(0);
        Ok(())
    }

    fn increment_newline_stretch(&mut self) {
        self.current_newline_stretch = Some(match self.current_newline_stretch {
            None => 0,
            Some(n) => n + 1,
        });
    }
}

impl<W: Write> Write for NewlineCollapser<W> {
    fn write(&mut self, buf: &[u8]) -> std::io::Result<usize> {
        let mut wrote = 0;
        let mut remaining = buf;
        while !remaining.is_empty() {
            match memchr::memchr(b'\n', remaining) {
                None => {
                    // No newline found, and we know there's at least one byte due to the `while` condition.
                    // So: (1) write `remaining` to the underlying, (2) set just_wrote_newline = false (since we just
                    // wrote at least one byte, and no newlines), and (3) break, since we just wrote all of remaining.
                    self.flush_newlines()?;
                    wrote += self.underlying.write(remaining)?;
                    self.current_newline_stretch = Some(0);
                    break;
                }
                Some(0) => {
                    // First byte is a newline. Increment the current stretch, and that's it.
                    // This case is the whole purpose of this struct: the `else` is what does the newline collapsing.
                    self.increment_newline_stretch();
                    wrote += 1; // We did process this byte, even if we haven't actually written it out yet
                    remaining = &remaining[1..];
                }
                Some(n) => {
                    // The first byte isn't a newline, so even if we had just written a newline previously, we can
                    // always just write out that first char. Keep writing until n. If we wrote n bytes, then we wrote
                    // the newline; otherwise, we didn't.
                    self.flush_newlines()?;
                    let underlying_wrote_n = self.underlying.write(&remaining[..n])?;
                    wrote += underlying_wrote_n;
                    if underlying_wrote_n == n {
                        self.increment_newline_stretch();
                        wrote += 1;
                    }
                    remaining = &remaining[underlying_wrote_n + 1..];
                }
            }
        }
        Ok(wrote)
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.underlying.flush()
    }
}

#[cfg(test)]
mod test {
    use crate::fmt_plain_writer::NewlineCollapser;
    use std::io::Write;

    #[test]
    fn no_newlines() {
        check(1, ["hello"], "hello");
    }

    #[test]
    fn empty() {
        check(1, [""], "");
    }

    #[test]
    fn start_with_newlines() {
        check(1, ["\nA", "\nB", "\n", "\nC", "\n", "\n", "D"], "A\nB\nC\nD");
    }

    #[test]
    fn end_with_newlines() {
        check(1, ["A\n", "B\n\n", "C\n"], "A\nB\nC");
    }

    #[test]
    fn newlines_in_middle() {
        check(1, ["A\nB", "C\n\nD"], "A\nBC\nD");
    }

    #[test]
    fn collapse_stretches_more_than_two() {
        check(2, ["A\nB\n\nC\n\n\nD"], "A\nB\n\nC\n\nD");
    }

    #[test]
    fn trailing_newlines_always_trimmed() {
        check(3, ["A\n\n\n\n\n"], "A");
    }

    fn check<const N: usize>(max_newlines: usize, inputs: [&str; N], expect: &str) {
        let input_lens: usize = inputs.iter().map(|s| s.len()).sum();

        let mut collapser = NewlineCollapser::new(Vec::with_capacity(expect.len()), max_newlines);

        let mut wrote = 0;
        for input in inputs {
            let bs = input.as_bytes();
            wrote += collapser.write(bs).expect("should have written");
        }
        let actual_str = String::from_utf8(collapser.take_underlying()).expect("utf8 encoding problem");

        assert_eq!(&actual_str, expect);
        assert_eq!(wrote, input_lens);
    }
}
