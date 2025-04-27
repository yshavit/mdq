#[derive(Copy, Clone, PartialEq, Eq, Debug)]

pub enum WordBoundary {
    AnyWhitespace,
    OnlyAtNewline,
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
    shorten_current_line_by: usize,
    pending_word_char_count: usize,
}

/// An action that writes a single char in the context of a [WordsBuffer], and returns how much to shorten the current
/// line's wrapping by. The return value is useful for reporting back extra indentation that the action wrote to its
/// underlying stream.
pub trait BufferedCharWrite: FnMut(char) -> usize {}
impl<T: FnMut(char) -> usize> BufferedCharWrite for T {}

#[must_use]
pub struct WordBoundaryRestore {
    previous: WordBoundary,
}

impl WordBoundaryRestore {
    pub fn restore_to(self, to: &mut WordsBuffer) {
        to.boundary = self.previous;
    }
}

impl WordsBuffer {
    pub fn new(line_length: usize) -> Self {
        Self {
            line_length,

            boundary: WordBoundary::AnyWhitespace,

            pending_word: String::with_capacity(line_length),
            chars_written_to_line: 0,
            writing_first_word: true,
            shorten_current_line_by: 0,
            pending_word_char_count: 0,
        }
    }

    /// Returns a WordBuffer that's "disabled", in that it always just immediately emits everything it gets.
    pub fn disabled() -> Self {
        let mut wb = Self::new(0);
        wb.boundary = WordBoundary::OnlyAtNewline;
        wb
    }

    /// sets the word boundary, and returns the old one
    pub fn set_word_boundary(&mut self, boundary: WordBoundary) -> WordBoundaryRestore {
        let previous = self.boundary;
        self.boundary = boundary;
        WordBoundaryRestore { previous }
    }

    pub fn shorten_current_line(&mut self, by: usize) {
        self.shorten_current_line_by += by;
    }

    pub fn push(&mut self, ch: char, mut action: impl BufferedCharWrite) {
        if ch == '\n' {
            self.start_new_line(action)
        } else if self.char_is_boundary(ch) {
            // Ignore spaces at the start of the line.
            if self.chars_written_to_line == 0 {
                return;
            }
            // At this point, we know there's been at least one word already written, and now we have a space. We may
            // need to eventually write that space, but we may not:
            //   - This could be the end of stream (trailing spaces are trimmed)
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
                if self.chars_written_to_line + self.pending_word_char_count < self.current_line_length() {
                    // Pending word fits on the current line.
                    self.drain_pending_word(action);
                    self.writing_first_word = false;
                } else {
                    // Need to start a new line, and then, since we *are* now at the start of a line, we can just action the
                    // pending word directly.
                    self.start_new_line(&mut action);
                    self.drain_without_leading_space(action);
                }
            } else {
                self.writing_first_word = false;
            }
        } else if self.writing_first_word {
            // Just action it directly
            self.shorten_current_line_by += action(ch);
            self.chars_written_to_line += 1;
        } else {
            // How many chars have we already allocated? That's (what we've written) + (what's pending) + (2).
            // The +2 is for the new char, plus the space that would have to be written before this pending word (which
            // we know isn't the first word, because that was handled above).
            let allocated_chars = self.chars_written_to_line + self.pending_word_char_count + 2;
            if allocated_chars > self.current_line_length() {
                self.start_new_line(&mut action);
                self.drain_without_leading_space(&mut action);
                self.shorten_current_line_by += action(ch);
                self.chars_written_to_line += 1;
            } else {
                self.pending_word.push(ch);
                self.pending_word_char_count += 1;
            }
        }
    }

    fn current_line_length(&self) -> usize {
        if self.line_length >= self.shorten_current_line_by {
            self.line_length - self.shorten_current_line_by
        } else {
            0
        }
    }

    pub fn has_pending_word(&self) -> bool {
        self.pending_word_char_count > 0
    }

    /// Drains any pending chars that [push] hadn't already actioned on.
    ///
    /// Note that newlines always get handled in `push` (whether they come as the argument to `push` or are generated by
    /// wrapping), meaning that this function will never send `\n` chars.
    pub fn drain_pending_word(&mut self, mut drain_action: impl BufferedCharWrite) {
        if !self.has_pending_word() {
            return;
        }

        // If we have a pending word, it's not the first word (that gets actioned directly). That means we need to add
        // a space first.
        self.shorten_current_line_by += drain_action(' ');
        self.chars_written_to_line += 1;

        self.drain_without_leading_space(drain_action);
    }

    pub fn reset(&mut self) {
        self.chars_written_to_line = 0;
        self.writing_first_word = true;
        self.shorten_current_line_by = 0;
    }

    fn start_new_line(&mut self, mut drain_action: impl BufferedCharWrite) {
        let shorten_line_by = drain_action('\n');
        self.reset();
        self.shorten_current_line_by += shorten_line_by;
    }

    fn drain_without_leading_space(&mut self, mut drain_action: impl BufferedCharWrite) {
        self.pending_word.drain(..).for_each(|ch| {
            let shorten_by = drain_action(ch);
            self.shorten_current_line_by += shorten_by;
        });
        self.chars_written_to_line += self.pending_word_char_count;
        self.pending_word_char_count = 0;
    }

    fn char_is_boundary(&self, ch: char) -> bool {
        match self.boundary {
            WordBoundary::AnyWhitespace => ch.is_whitespace(),
            WordBoundary::OnlyAtNewline => false, // surely newlines should still count? or maybe they're specially handled?
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
    fn crs_and_tabs_are_whitespace() {
        assert_eq!(
            WbHelper::build(11, |wbh| {
                wbh.push_str("hello\r\rfriendly\r\r\r\t\rox");
            }),
            "hello\nfriendly ox"
        );
    }

    #[test]
    fn newlines_always_reset_wrapping() {
        // If newlines were included in WordBoundary::Never, then "<hello\n>friendly ox" would wrap as
        // "<hello\nfriendly>\n<ox>".
        // Instead, the \n always resets the line, meaning that the output is "<hello><friendly ox>"
        assert_eq!(
            WbHelper::build(11, |wbh| {
                let restore_boundary = wbh.wb.set_word_boundary(WordBoundary::OnlyAtNewline);
                wbh.push_str("hello\n");
                restore_boundary.restore_to(&mut wbh.wb);

                wbh.push_str("friendly ox");
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
                    wbh.wb.boundary = WordBoundary::OnlyAtNewline;
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
                    wbh.wb.boundary = WordBoundary::OnlyAtNewline;
                    wbh.push(' '); // 3
                    wbh.push(' '); // 4
                    wbh.push(' '); // 5
                    wbh.push('C'); // 6
                    wbh.wb.boundary = WordBoundary::AnyWhitespace;
                    wbh.push_str("D EF"); // 6
                }),
                "AB\n12   CD\nEF"
            );
        }

        #[test]
        fn never_whitespace_and_no_limit() {
            let mut wb = WordsBuffer::disabled();

            let mut last_seen = None;

            wb.push('a', |ch| {
                last_seen = Some(ch);
                0
            });
            assert_eq!(last_seen, Some('a'));

            wb.push(' ', |ch| {
                last_seen = Some(ch);
                0
            });
            assert_eq!(last_seen, Some(' '));

            wb.push('\n', |ch| {
                last_seen = Some(ch);
                0
            });
            assert_eq!(last_seen, Some('\n'));

            last_seen = None; // we want to prove that drain is a no-op
            wb.drain_pending_word(|ch| {
                last_seen = Some(ch);
                0
            });
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
                self.s.push(ch);
                0
            });
        }

        pub fn push_str(&mut self, text: &str) {
            text.chars().for_each(|ch| self.push(ch));
        }

        pub fn end(mut self) -> String {
            self.wb.drain_pending_word(|ch| {
                self.s.push(ch);
                0
            });
            self.s
        }
    }
}
