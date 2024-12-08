use crate::str_utils::{CharCountingStringBuffer, WhitespaceSplit, WhitespaceSplitter};
use std::borrow::Cow;
use std::cmp::PartialEq;
use std::collections::VecDeque;
use std::ops::{Deref, Range};

pub trait SimpleWrite {
    fn write_str(&mut self, text: &str) -> std::io::Result<()>;
    fn flush(&mut self) -> std::io::Result<()>;
}

impl SimpleWrite for String {
    fn write_str(&mut self, text: &str) -> std::io::Result<()> {
        self.push_str(text);
        Ok(())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

pub struct Stream<W>(pub W);

impl<W: std::io::Write> SimpleWrite for Stream<W> {
    fn write_str(&mut self, text: &str) -> std::io::Result<()> {
        self.0.write(&text.as_bytes()).map(|_| ())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.0.flush()
    }
}

pub struct OutputOpts {
    pub text_width: Option<usize>,
}

pub struct Output<W: SimpleWrite> {
    wrap_buffer: Option<WrapBuffer>,
    stream: W,
    pre_mode: bool,
    blocks: Vec<Block>,
    pending_blocks: Vec<Block>,
    pending_newlines: usize,
    pending_padding_after_indent: usize,
    writing_state: WritingState,
    current_line_chars: CurrentLineChars,
}

struct WrapBuffer {
    wrap_at: usize,
    pending: VecDeque<String>,
    buffer: CharCountingStringBuffer,
}

impl WrapBuffer {
    fn new(wrap_at: usize) -> Self {
        Self {
            wrap_at,
            pending: VecDeque::with_capacity(4), // guess
            buffer: CharCountingStringBuffer::with_capacity(wrap_at),
        }
    }

    fn accept(&mut self, text: &str) -> Option<String> {
        for elem in WhitespaceSplitter::from(text) {
            match elem {
                WhitespaceSplit::Whitespace => {
                    self.buffer.push_whitespace();
                }
                WhitespaceSplit::Word(word, word_len) => {
                    if self.buffer.chars_len() + word_len > self.wrap_at {
                        self.pending.push_back(self.buffer.drain());
                    }
                    self.buffer.push_str(word, word_len, false);
                }
            }
        }
        self.pending.pop_front()
    }

    fn drain_all(&mut self) -> Vec<String> {
        if self.buffer.has_non_whitespace() {
            self.pending.push_back(self.buffer.drain());
        }
        self.pending.drain(..).collect()
    }
}

impl<W: SimpleWrite> SimpleWrite for Output<W> {
    fn write_str(&mut self, text: &str) -> std::io::Result<()> {
        Self::write_str(self, text);
        Ok(())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.stream.flush()
    }
}

pub struct PreWriter<'a, W: SimpleWrite> {
    output: &'a mut Output<W>,
}

#[derive(Debug, PartialEq)]
pub enum Block {
    /// A plain block; just paragraph text. The `bool` controls whether wrapping is enabled.
    ///
    /// This only matters if the output has a `text_width`. If it does not, this parameter is
    /// ignored.
    Plain(bool),
    /// A quoted block (`> `)
    Quote,
    /// A block that does *not* start with newlines, but does add indentation. It ends in a single newline.
    /// **This block affects its first child block**, in that it suppresses that block's newlines.
    ///
    /// This is useful for lists:
    ///
    /// ```md
    /// 1. §Paragraph starts here.§
    /// 2. §Paragraph starts here, and
    ///    indentation continues.§
    /// ```
    ///
    /// (where `§` signifies the start and end of an inlined paragraph)
    Inlined(usize),
}

impl<W: SimpleWrite> Output<W> {
    pub fn new_unwrapped(to: W) -> Self {
        Self::new(to, OutputOpts { text_width: None })
    }

    pub fn new(to: W, opts: OutputOpts) -> Self {
        Self {
            stream: to,
            wrap_buffer: opts.text_width.map(|wrap_at| WrapBuffer::new(wrap_at)),
            pre_mode: false,
            blocks: Vec::default(),
            pending_blocks: Vec::default(),
            pending_newlines: 0,
            pending_padding_after_indent: 0,
            writing_state: WritingState::HaveNotWrittenAnything,
            current_line_chars: CurrentLineChars::default(),
        }
    }

    pub fn replace_underlying(&mut self, new: W) -> std::io::Result<W> {
        self.stream.flush()?;
        Ok(std::mem::replace(&mut self.stream, new))
    }

    pub fn with_block<F>(&mut self, block: Block, action: F)
    where
        F: FnOnce(&mut Self),
    {
        self.push_block(block);
        action(self);
        self.pop_block();
    }

    /// Creates a new block within which newlines are honored literally, and no wrapping happens.
    /// When newlines are entered, you'll still get any quotes from previous blocks.
    ///
    /// Because you have to call this on `&mut self`, you can't access that `self` within the
    /// block. This is by design, since it doesn't make sense to add blocks within a `pre`. Instead,
    /// use the provided writer's `write_str` to write the literal text for this block.
    pub fn with_pre_block<F>(&mut self, action: F)
    where
        F: for<'a> FnOnce(&mut PreWriter<W>),
    {
        self.push_block(Block::Plain(false));
        self.pre_mode = true;
        action(&mut PreWriter { output: self });
        self.pre_mode = false;
        self.pop_block();
    }

    fn push_block(&mut self, block: Block) {
        self.pending_blocks.push(block);
    }

    fn pop_block(&mut self) {
        self.flush_pending_lines(false);
        if !self.pending_blocks.is_empty() {
            self.write_line(""); // write a blank line for whatever blocks had been enqueued
        }
        let newlines = match self.blocks.pop() {
            None => 0,
            Some(closing_block) => match closing_block {
                Block::Plain(_) => 2,
                Block::Quote => 2,
                Block::Inlined(_) => 1,
            },
        };
        self.ensure_newlines(NewlinesRequest::Exactly(newlines));
    }

    fn flush_pending_lines(&mut self, add_trailing_newline: bool) {
        if let Some(buffer) = &mut self.wrap_buffer {
            for line in buffer.drain_all() {
                self.ensure_newlines(NewlinesRequest::AtLeast(1));
                self.write_line(&line);
            }
        }
        if add_trailing_newline && self.pending_newlines > 0 {
            self.write_raw("\n");
            self.pending_newlines = 0;
        }
    }

    pub fn write_str(&mut self, text: &str) {
        let resolved_text = self.rewrap_text(text);
        let newlines_re = if self.pre_mode { "\n" } else { "\n+" };
        let lines = regex::Regex::new(newlines_re).unwrap();
        let mut lines = lines.split(&resolved_text).peekable();
        while let Some(line) = lines.next() {
            self.write_line(line);
            if !line.is_empty() {
                self.set_writing_state(WritingState::Normal);
            }
            if lines.peek().is_some() {
                self.ensure_newlines(NewlinesRequest::AtLeast(1));
            }
        }
    }

    fn rewrap_text<'a>(&mut self, text: &'a str) -> Cow<'a, str> {
        let wrap = match &self.blocks.last() {
            None // shouldn't happen outside tests, but assume normal paragraph
            | Some(Block::Plain(true))
            | Some(Block::Quote)
            => Some(&mut self.wrap_buffer),
            Some(Block::Plain(false))
            |Some(Block::Inlined(_))
            => None,
        };

        let Some(Some(wrap_buffer)) = wrap else {
            return Cow::Borrowed(text);
        };
        let wrap = wrap_buffer.wrap_at;
        let Some(text_to_unbuffer) = wrap_buffer.accept(text) else {
            return Cow::Borrowed("");
        };

        let re = regex::Regex::new(" *\n *").unwrap();
        let without_newlines = re.replace_all(&text_to_unbuffer, " ");

        // We can guess the number of newlines we'll need as (total length / line length). Add a bit
        // for a fudge factor, cause under-allocating is more expensive than over-allocating.
        let mut result = String::with_capacity((text.len() / wrap) + 16);
        let current_line_len = self.current_line_chars.len();
        let mut current_wrap = if current_line_len >= wrap {
            result.push('\n');
            0
        } else {
            wrap - current_line_len
        };
        let mut chars_written_to_line = self.current_line_chars.len();
        for word_or_whitespace in WhitespaceSplitter::from(without_newlines.deref()) {
            match word_or_whitespace {
                WhitespaceSplit::Whitespace => self.current_line_chars.pending_space = true,
                WhitespaceSplit::Word(word, word_char_len) => {
                    let mut required_available_length = word_char_len;
                    if self.current_line_chars.pending_space {
                        required_available_length += 1;
                    }

                    if chars_written_to_line > 0 {
                        if chars_written_to_line + required_available_length > current_wrap {
                            result.push('\n');
                            chars_written_to_line = 0;
                        } else if self.current_line_chars.pending_space {
                            result.push(' ');
                            chars_written_to_line += 1;
                        }
                    }
                    result.push_str(word);
                    chars_written_to_line += word_char_len;
                    current_wrap = wrap;
                }
            }
        }
        Cow::Owned(result)
    }

    pub fn write_char(&mut self, ch: char) {
        self.write_str(&String::from(ch))
    }

    // Writes text which is assumed to not have any newlines
    fn write_line(&mut self, text: &str) {
        // If we have any pending blocks, handle those first. We need to add a paragraph break, unless the first block
        // is an Inlined.
        match self.pending_blocks.first() {
            None => {}
            Some(Block::Inlined(_)) => self.set_writing_state(WritingState::IgnoringNewlines),
            Some(_) => self.ensure_newlines(NewlinesRequest::AtLeast(2)),
        }

        // print the newlines before we append the new blocks
        let need_full_indent = if self.pending_newlines > 0 {
            for _ in 0..(self.pending_newlines - 1) {
                self.write_raw("\n");
                self.write_indent(true, None);
            }
            self.write_raw("\n"); // we'll do this indent after we add the pending blocks
            self.pending_newlines = 0;
            true
        } else {
            matches!(self.writing_state, WritingState::HaveNotWrittenAnything)
        };

        // Append the new blocks, and then write the indent if we need it
        // When we write that indent, though, only write it until the first new Inlined (exclusive).
        if need_full_indent {
            let indent_end_idx = self.blocks.len()
                + self
                    .pending_blocks
                    .iter()
                    .position(|b| matches!(b, Block::Inlined(_)))
                    .unwrap_or_else(|| self.pending_blocks.len());
            self.blocks.append(&mut self.pending_blocks);
            self.write_indent(true, Some(0..indent_end_idx));
        } else {
            self.write_padding_after_indent();
            let prev_blocks_len = self.blocks.len();
            self.blocks.append(&mut self.pending_blocks);
            self.write_indent(false, Some(prev_blocks_len..self.blocks.len()));
        }

        // Finally, write the text
        if !text.is_empty() {
            self.write_padding_after_indent();
            self.write_raw(text);
        }
    }

    /// Write an indentation for a given range of indentation block. If `include_inlines` is false, `Inlined` blocks
    /// will be disregarded. Do this for the first line in which those inlines appear (and only there).
    fn write_indent(&mut self, include_inlines: bool, range: Option<Range<usize>>) {
        self.pending_padding_after_indent = 0;
        for idx in range.unwrap_or_else(|| 0..self.blocks.len()) {
            match &self.blocks[idx] {
                Block::Plain(_) => {}
                Block::Quote => {
                    self.write_padding_after_indent();
                    self.write_raw(">");
                    self.pending_padding_after_indent += 1;
                }
                Block::Inlined(size) => {
                    if include_inlines {
                        self.pending_padding_after_indent += size;
                    }
                }
            }
        }
    }

    fn write_padding_after_indent(&mut self) {
        (0..self.pending_padding_after_indent).for_each(|_| self.write_raw(" "));
        self.pending_padding_after_indent = 0;
    }

    fn ensure_newlines(&mut self, request: NewlinesRequest) {
        match self.writing_state {
            WritingState::Normal => match request {
                NewlinesRequest::Exactly(n) => self.pending_newlines = n,
                NewlinesRequest::AtLeast(n) => {
                    if self.pending_newlines < n {
                        self.pending_newlines = n;
                    }
                }
            },
            WritingState::HaveNotWrittenAnything | WritingState::IgnoringNewlines | WritingState::Error => {}
        }
    }

    fn set_writing_state(&mut self, state: WritingState) {
        if matches!(self.writing_state, WritingState::Error) {
            return;
        }
        self.writing_state = state;
    }

    fn write_raw(&mut self, text: &str) {
        if matches!(self.writing_state, WritingState::Error) {
            return;
        }
        if text.is_empty() {
            return;
        }
        if let Err(e) = self.stream.write_str(text) {
            eprintln!("error while writing output: {}", e);
            self.writing_state = WritingState::Error;
        }
        if matches!(self.writing_state, WritingState::HaveNotWrittenAnything) {
            self.writing_state = WritingState::Normal;
        }
        self.current_line_chars.see(text);
    }
}

impl<W: SimpleWrite> Output<W>
where
    W: Default,
{
    pub fn take_underlying(&mut self) -> std::io::Result<W> {
        // this is used primarily for tests, so to keep them clean, we don't want trailing newline
        // TODO maybe I should keep it, though -- just to keep things consistent.
        self.flush_pending_lines(false);
        self.stream.flush()?;
        Ok(std::mem::take(&mut self.stream))
    }
}

impl<W: SimpleWrite> Drop for Output<W> {
    fn drop(&mut self) {
        self.flush_pending_lines(true);
        if let Err(e) = self.stream.flush() {
            if WritingState::Error != self.writing_state {
                eprintln!("error while writing output: {}", e);
                self.writing_state = WritingState::Error;
            }
        }
    }
}

impl<'a, W: SimpleWrite> PreWriter<'a, W> {
    pub fn write_str(&mut self, text: &str) {
        self.output.write_str(text)
    }

    pub fn write_char(&mut self, ch: char) {
        self.output.write_char(ch);
    }
}

enum NewlinesRequest {
    Exactly(usize),
    AtLeast(usize),
}

#[derive(PartialEq, Copy, Clone)]
enum WritingState {
    /// We haven't written any text; that is, there hasn't been any invocation of [Output::write_str] or
    /// [Output::write_char]. We may have queued up some blocks.
    HaveNotWrittenAnything,

    /// Any newline requests (via [Output::ensure_newlines] will be ignored. This happens for the first line following a
    /// new [Block::Inlined].
    IgnoringNewlines,

    /// No special state. Just write the chars!
    Normal,

    /// There's been an error in writing. From here on in, everything will be a noop.
    Error,
}

#[derive(Debug, Default)]
struct CurrentLineChars {
    count: usize,
    pending_space: bool,
}

impl CurrentLineChars {
    fn len(&self) -> usize {
        self.count + if self.pending_space { 1 } else { 0 }
    }

    fn see(&mut self, text: &str) {
        // You can't get the chars without going left-to-right on the string's bytes, so we can't
        // just search right-to-left until we get the newline.
        for ch in text.chars() {
            if ch == '\n' || ch == '\r' {
                self.count = 0;
                self.pending_space = false;
            } else if ch.is_ascii_whitespace() {
                self.pending_space = true;
            } else {
                if self.pending_space {
                    self.pending_space = false;
                    self.count += 1;
                }
                self.count += 1;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use indoc::indoc;

    use super::*;

    #[test]
    fn simple_string() {
        assert_eq!(out_to_str(|out| out.write_str("hello")), indoc!("hello"),);
    }

    #[test]
    fn pop_block_when_there_is_none() {
        assert_eq!(
            out_to_str(|out| {
                out.pop_block(); // no-op
                out.write_str("hello world");
            }),
            "hello world",
        );
    }

    #[test]
    fn plain_block() {
        assert_eq!(
            out_to_str(|out| {
                out.write_str("before");
                out.with_block(Block::Plain(false), |out| {
                    out.write_str("hello\nworld");
                });
                out.write_str("after");
            }),
            indoc! {r#"
                before

                hello
                world

                after"#}
        );
    }

    #[test]
    fn quote_block() {
        assert_eq!(
            out_to_str(|out| {
                out.write_str("before");
                out.with_block(Block::Quote, |out| {
                    out.write_str("hello\nworld");
                });
                out.write_str("after");
            }),
            indoc! {r#"
                before

                > hello
                > world

                after"#}
        );
    }

    #[test]
    fn quote_block_one_char_at_a_time_as_initial_output() {
        // We haven't written anything, and then we start writing quote blocks
        assert_eq!(
            out_to_str(|out| {
                out.with_block(Block::Quote, |out| {
                    out.write_str(""); // empty prefix char
                    "hello\nworld".chars().for_each(|c| out.write_char(c));
                    out.write_str(""); // empty suffix char
                });
            }),
            indoc! {r#"
                > hello
                > world"#}
        );
    }

    #[test]
    fn inlined_block() {
        assert_eq!(
            out_to_str(|out| {
                out.write_str("before");
                out.with_block(Block::Plain(false), |out| {
                    out.with_block(Block::Inlined(3), |out| {
                        out.write_str("directly in\nthe inlined block");
                    });
                    out.with_block(Block::Inlined(3), |out| {
                        out.with_block(Block::Plain(false), |out| out.write_str("paragraph 1"));
                        out.with_block(Block::Plain(false), |out| out.write_str("paragraph 2"));
                    });
                });
            }),
            indoc! {r#"
                before

                directly in
                   the inlined block
                paragraph 1
                
                   paragraph 2"#}
        )
    }

    // This example is what we actually expect in practice, from a list item.
    #[test]
    fn inlined_block_used_like_list_item() {
        assert_eq!(
            out_to_str(|out| {
                out.write_str("1. ");
                out.with_block(Block::Inlined(3), |out| {
                    out.with_block(Block::Plain(false), |out| {
                        out.write_str("First item");
                    });
                    out.with_block(Block::Plain(false), |out| {
                        out.write_str("It has two paragraphs.");
                    });
                });
                out.write_str("2. ");
                out.with_block(Block::Inlined(3), |out| {
                    out.with_block(Block::Plain(false), |out| {
                        out.write_str("Second item.");
                    });
                });
                out.write_str("3. ");
                out.with_block(Block::Inlined(3), |out| {
                    out.with_block(Block::Plain(false), |out| {
                        out.write_str("Third item");
                    });
                });
            }),
            indoc! {r#"
                1. First item

                   It has two paragraphs.
                2. Second item.
                3. Third item"#}
        );
    }

    #[test]
    fn pre_block() {
        assert_eq!(
            out_to_str(|out| {
                out.write_str("before");
                out.with_pre_block(|out| {
                    out.write_str("hello\n\nworld");
                });
                out.write_str("after");
            }),
            indoc! {r#"
                before

                hello

                world

                after"#}
        );
    }

    #[test]
    fn pre_block_with_small_writes() {
        assert_eq!(
            out_to_str(|out| {
                out.write_str("before");
                out.with_block(Block::Quote, |out| {
                    out.with_pre_block(|out| {
                        out.write_str("```");
                        out.write_str("\n");
                        out.write_str("my code");
                        out.write_str("\n");
                        out.write_str("```");
                    });
                });
                out.write_str("after");
            }),
            indoc! {r#"
                before

                > ```
                > my code
                > ```

                after"#}
        )
    }

    #[test]
    fn nested_blocks() {
        assert_eq!(
            out_to_str(|out| {
                out.with_block(Block::Quote, |out| {
                    out.write_str("hello ");
                    out.write_str("world");

                    out.with_block(Block::Quote, |out| {
                        out.write_str("second level");
                        // no ensuring newline
                        out.with_pre_block(|out| {
                            out.write_str("```rust\nsome code\nwith\n\nline breaks\n```");
                        });
                    })
                });
                out.write_str("after");
            }),
            indoc! {r#"
                > hello world
                >
                > > second level
                > >
                > > ```rust
                > > some code
                > > with
                > >
                > > line breaks
                > > ```

                after"#}
        );
    }

    #[test]
    fn nested_blocks_jump_a_few() {
        assert_eq!(
            out_to_str(|out| {
                out.write_str("before");
                out.with_block(Block::Quote, |out| {
                    out.with_block(Block::Quote, |out| {
                        out.with_block(Block::Quote, |out| out.write_str("hello"));
                    });
                });
                out.write_str("after");
            }),
            indoc! {r#"
                before

                > > > hello

                after"#}
        );
    }

    #[test]
    fn indents_without_inner_writes() {
        assert_eq!(
            out_to_str(|out| {
                out.push_block(Block::Quote);
                out.pop_block();
            }),
            indoc! {r#">"#}
        );
    }

    #[test]
    fn surrounds_without_inner_writes() {
        assert_eq!(
            out_to_str(|out| {
                out.with_pre_block(|out| {
                    out.write_str("vvv\n");
                    out.write_str("^^^\n");
                });
            }),
            indoc! {r#"
                vvv
                ^^^
            "#}
        );
    }

    mod wrapping {
        use super::*;

        #[test]
        fn simple_wrap() {
            assert_eq!(
                out_to_str_wrapped(9, |out| {
                    out.write_str("Hello the world");
                }),
                indoc! {r#"
                Hello the
                world"#}
            );
        }

        #[test]
        fn incremental_wrap() {
            assert_eq!(
                out_to_str_wrapped(9, |out| {
                    for ch in "Hello the world".chars() {
                        out.write_char(ch)
                    }
                }),
                indoc! {r#"
                Hello the
                world"#}
            );
            todo!("how do we stop things like URLs from wrapping? they definitely shouldn't!")
            // "Markdown applications don’t agree on how to handle spaces in the middle of a URL."
            // from https://www.markdownguide.org/basic-syntax/#link-best-practices
            // Maybe we need a new block type that doesn't add any newline at all, but just prevents
            // indentation?
        }

        fn out_to_str_wrapped<F>(wrap: usize, action: F) -> String
        where
            F: FnOnce(&mut Output<String>),
        {
            let opts = OutputOpts { text_width: Some(wrap) };
            let mut out = Output::new(String::new(), opts);
            action(&mut out);
            out.take_underlying().unwrap()
        }
    }

    fn out_to_str<F>(action: F) -> String
    where
        F: FnOnce(&mut Output<String>),
    {
        let mut out = Output::new_unwrapped(String::new());
        action(&mut out);
        out.take_underlying().unwrap()
    }
}
