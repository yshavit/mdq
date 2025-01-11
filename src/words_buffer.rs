#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum WordBoundary {
    Whitespace,
    Never,
}

pub struct WordsBuffer {
    // immutable customer-provided state
    line_length: usize,

    // mutable customer-provided state
    boundary: WordBoundary,

    // fundamental internal state
    pending_word: String,
    chars_written_to_line: usize,
    writing_first_word: bool,

    // derived internal state
    /// a cached `buffer.chars().count()`
    pending_word_char_count: usize,
}

impl WordsBuffer {
    pub fn new(line_length: usize) -> Self {
        Self {
            line_length,

            boundary: WordBoundary::Whitespace,

            pending_word: String::with_capacity(line_length),
            chars_written_to_line: 0,
            writing_first_word: true,

            pending_word_char_count: 0,
        }
    }

    /// Returns a WordBuffer that's "disabled", in that it always just immediately emits everything it gets.
    pub fn disabled() -> Self {
        let mut wb = Self::new(0);
        wb.set_word_boundary(WordBoundary::Never);
        wb
    }

    pub fn mark_chars_were_written(&mut self, count: usize) {
        self.chars_written_to_line += count;
    }

    /// sets the word boundary, and returns the old one
    pub fn set_word_boundary(&mut self, boundary: WordBoundary) -> WordBoundary {
        let old = self.boundary;
        self.boundary = boundary;
        old
    }

    pub fn push<F>(&mut self, ch: char, mut action: F)
    where
        F: FnMut(char),
    {
        if self.char_is_boundary(ch) {
            // Ignore spaces at the start of the line.
            if self.chars_written_to_line == 0 {
                return;
            }
            // At this point, we know there's been at least one word already written, and now we have a space. We may
            // need to eventually write that space, but we may not:
            //   - This could be the end of stream (trailing spaces are trimmed) TODO need to unit test that
            //   - The next word could wrap, meaning that this space becomes a newline instead
            //   - There could be multiple spaces in a row (we need to consolidate them)
            //
            // So for now, we just want to look at the pending word. If it's non-empty, we know what to do: either print
            // it on the current line, and start it on the next line.
            //
            // If there is no pending word, mostly ignore this char. If we need a space for it eventually, that
            // invocation will write it. We do want to note that we're no longer in the first word, so that later
            // account of chars will work correctly.
            if self.pending_word_char_count > 0 {
                if self.chars_written_to_line + self.pending_word_char_count + 1 <= self.line_length {
                    // Pending word fits on the current line.
                    self.drain(action);
                    self.writing_first_word = false;
                } else {
                    // Need to start a new line, and then, since we *are* now at the start of a line, we can just action the
                    // pending word directly.
                    self.start_new_line(|ch| action(ch));
                    self.drain_without_leading_space(action);
                }
            } else {
                self.writing_first_word = false;
            }
        } else if self.writing_first_word {
            // Just action it directly
            action(ch);
            self.chars_written_to_line += 1;
        } else {
            // How many chars have we already allocated? That's (what we've written) + (what's pending) + (2).
            // The +2 is for the new char, plus the space that would have to be written before this pending word (which
            // we know isn't the first word, because that was handled above).
            let allocated_chars = self.chars_written_to_line + self.pending_word_char_count + 2;
            if allocated_chars > self.line_length {
                self.start_new_line(|ch| action(ch));
                self.drain_without_leading_space(|ch| action(ch));
                action(ch);
                self.chars_written_to_line += 1;
            } else {
                self.pending_word.push(ch);
                self.pending_word_char_count += 1;
            }
        }
    }

    pub fn drain<F>(&mut self, mut drain_action: F)
    where
        F: FnMut(char),
    {
        if self.pending_word_char_count == 0 {
            return;
        }

        // If we have a pending word, it's not the first word (that gets actioned directly). That means we need to add
        // a space first.
        drain_action(' ');
        self.chars_written_to_line += 1;

        self.drain_without_leading_space(drain_action);
    }

    fn start_new_line<F>(&mut self, mut drain_action: F)
    where
        F: FnMut(char),
    {
        drain_action('\n');
        self.chars_written_to_line = 0;
        self.writing_first_word = true;
    }

    fn drain_without_leading_space<F>(&mut self, mut drain_action: F)
    where
        F: FnMut(char),
    {
        self.pending_word.drain(..).for_each(|ch| drain_action(ch));
        self.chars_written_to_line += self.pending_word_char_count;
        self.pending_word_char_count = 0;
    }

    fn char_is_boundary(&self, ch: char) -> bool {
        match self.boundary {
            WordBoundary::Whitespace => ch.is_whitespace(),
            WordBoundary::Never => false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn wrapping_does_not_apply() {
        assert_eq!(
            WbHelper::build(50, |wbh| {
                wbh.push_str("hello hi");
            }),
            "hello hi"
        );
    }

    #[test]
    fn wrapping_applies_within_each_word() {
        assert_eq!(
            WbHelper::build(3, |wbh| {
                wbh.push_str("hello world");
            }),
            "hello\nworld"
        );
    }

    #[test]
    fn wrapping_applies_after_first_word() {
        assert_eq!(
            WbHelper::build(5, |wbh| {
                wbh.push_str("hello hi hi");
            }),
            "hello\nhi hi"
        );
    }

    #[test]
    fn wrapping_applies_after_first_space() {
        assert_eq!(
            WbHelper::build(6, |wbh| {
                wbh.push_str("hello 1A 2B");
            }),
            "hello\n1A 2B"
        );
    }

    #[test]
    fn wrapping_applies_at_start_of_second_word() {
        assert_eq!(
            WbHelper::build(7, |wbh| {
                wbh.push_str("hello hi hi");
            }),
            "hello\nhi hi"
        );
    }

    #[test]
    fn word_is_longer_than_line_length() {
        assert_eq!(
            WbHelper::build(5, |wbh| {
                wbh.push_str("a abcdefghijklmnopqrstuvwxyz z");
            }),
            "a\nabcdefghijklmnopqrstuvwxyz\nz"
        );
    }

    #[test]
    fn wrap_to_zero() {
        // degenerate case
        assert_eq!(
            WbHelper::build(0, |wbh| {
                wbh.push_str("a hello b");
            }),
            "a\nhello\nb"
        );
    }

    #[test]
    fn multiple_spaces_get_consolidated() {
        assert_eq!(
            WbHelper::build(12, |wbh| {
                wbh.push_str("hello       world");
            }),
            "hello world"
        );
    }

    #[test]
    fn leading_spaces_get_trimmed() {
        assert_eq!(
            WbHelper::build(50, |wbh| {
                wbh.push_str("     hello world");
            }),
            "hello world"
        );
    }

    #[test]
    fn trailing_spaces_get_trimmed() {
        assert_eq!(
            WbHelper::build(50, |wbh| {
                wbh.push_str("hello world     ");
            }),
            "hello world"
        );
    }

    #[test]
    fn newlines_are_whitespace() {
        assert_eq!(
            WbHelper::build(11, |wbh| {
                wbh.push_str("hello\n\nfriendly\r\n\r\t\nox");
            }),
            "hello\nfriendly ox"
        );
    }

    /// Smoke test of [WordsBuffer::set_word_boundary]. We don't actually need a ton of testing, because this only
    /// affects [WordsBuffer::char_is_boundary], which is quite simple.
    mod boundary_is_never {
        use super::*;

        #[test]
        fn whitespace_is_preserved() {
            assert_eq!(
                WbHelper::build(50, |wbh| {
                    wbh.wb.set_word_boundary(WordBoundary::Never);
                    wbh.push_str(" \t ");
                }),
                " \t "
            );
        }

        #[test]
        fn whitespace_counts_as_word() {
            assert_eq!(
                WbHelper::build(5, |wbh| {
                    wbh.push_str("AB 12"); // pending word is "12"
                    wbh.wb.set_word_boundary(WordBoundary::Never);
                    wbh.push(' '); // 3
                    wbh.push(' '); // 4
                    wbh.push(' '); // 5
                    wbh.push('C'); // 6
                    wbh.wb.set_word_boundary(WordBoundary::Whitespace);
                    wbh.push_str("D EF"); // 6
                }),
                "AB\n12   CD\nEF"
            );
        }

        #[test]
        fn never_whitespace_and_no_limit() {
            let mut wb = WordsBuffer::disabled();

            let mut last_seen = None;

            wb.push('a', |ch| last_seen = Some(ch));
            assert_eq!(last_seen, Some('a'));

            wb.push(' ', |ch| last_seen = Some(ch));
            assert_eq!(last_seen, Some(' '));

            wb.push('\n', |ch| last_seen = Some(ch));
            assert_eq!(last_seen, Some('\n'));

            last_seen = None; // we want to prove that drain is a no-op
            wb.drain(|ch| last_seen = Some(ch));
            assert_eq!(last_seen, None);
        }
    }

    struct WbHelper {
        wb: WordsBuffer,
        s: String,
    }

    impl WbHelper {
        fn build<F>(line_length: usize, mut action: F) -> String
        where
            F: FnMut(&mut WbHelper),
        {
            let mut wbh = WbHelper::new(line_length);
            action(&mut wbh);
            wbh.end()
        }

        fn new(line_length: usize) -> Self {
            Self {
                wb: WordsBuffer::new(line_length),
                s: String::new(),
            }
        }

        pub fn push(&mut self, ch: char) {
            self.wb.push(ch, |ch| {
                // put this on its own line, for ease of setting debug points
                self.s.push(ch)
            });
        }

        pub fn push_str(&mut self, text: &str) {
            text.chars().for_each(|ch| self.push(ch));
        }

        pub fn end(mut self) -> String {
            self.wb.drain(|ch| self.s.push(ch));
            self.s
        }
    }
}
