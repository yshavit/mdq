use crate::str_utils::{CharCountingStringBuffer, WhitespaceSplit, WhitespaceSplitter};
use std::borrow::Cow;
use std::cmp::PartialEq;
use std::collections::VecDeque;
use std::iter::once;
use std::ops::{Deref, Range};
use termsize::get;

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

#[derive(Debug)]
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

#[derive(Debug)]
struct WrapBuffer {
    mode: WrapBufferMode,
    wrap_at: usize,
    pending: VecDeque<String>,
    buffer: CharCountingStringBuffer,
}

#[derive(Debug, Copy, Clone)]
enum WrapBufferMode {
    /// Standard wrapping: just wrap as soon as you hit [WrapBuffer::wrap_at].
    AlwaysWrap,

    /// Wrap the first line if it exceeds [WrapBuffer::wrap_at], but only that first line.
    /// Use this when we first enter a [Output::without_wrapping] block.
    OnlyWrapOnce { already_wrote: usize },

    /// Disable the wrap altogether. This gets set after [OnlyWrapOnce] wraps.
    NeverWrap,
}

enum WrapBufferOutput<'a> {
    None,
    Wrapping(String),
    NotWrapping(Cow<'a, str>),
}

impl WrapBuffer {
    fn new(wrap_at: usize) -> Self {
        Self {
            wrap_at,
            mode: WrapBufferMode::AlwaysWrap,
            pending: VecDeque::with_capacity(4), // guess
            buffer: CharCountingStringBuffer::with_capacity(wrap_at),
        }
    }

    fn disable(&mut self, already_wrote: usize) {
        self.mode = if already_wrote > 0 {
            WrapBufferMode::OnlyWrapOnce { already_wrote }
        } else {
            WrapBufferMode::NeverWrap
        };
    }

    fn enable(&mut self) {
        self.mode = WrapBufferMode::AlwaysWrap;
    }

    fn accept<'a>(&mut self, text: &'a str) -> WrapBufferOutput<'a> {
        if matches!(self.mode, WrapBufferMode::NeverWrap) {
            return WrapBufferOutput::NotWrapping(Cow::Borrowed(text));
        }
        for elem in WhitespaceSplitter::from(text) {
            match elem {
                WhitespaceSplit::Whitespace => {
                    self.buffer.push_whitespace();
                }
                WhitespaceSplit::Word(word, word_len) => {
                    if self.buffer.chars_len() + word_len > self.wrap_at_len() {
                        if matches!(self.mode, WrapBufferMode::OnlyWrapOnce { .. }) {
                            // We hit this when the output had written some text, and then entered
                            // an Output::without_wrapping. The whole body of the without_wrapping
                            // effectively counts as one word. So, we first need a newline, to wrap
                            // that "word" to the next line (since we already had some text on this
                            // one). Then, we want to enqueue whatever we wrote so far (which we'll
                            // do in self.pending.push_back, in a sec), and also mark that we're now
                            // in NeverWrap mode until the user invokes self.enable().
                            self.pending.push_back("".to_string());
                            self.mode = WrapBufferMode::NeverWrap;
                        }
                        self.pending.push_back(self.buffer.drain());
                    }
                    self.buffer.push_str(word, word_len, false);
                }
            }
        }
        match self.mode {
            WrapBufferMode::OnlyWrapOnce { .. } => {
                // If we hit here, we haven't yet hit the wrapping limit on that first wrap. So,
                // nothing to report yet.
                WrapBufferOutput::None
            }
            WrapBufferMode::NeverWrap => match self.pending.pop_front() {
                None => WrapBufferOutput::None,
                Some(s) => WrapBufferOutput::NotWrapping(Cow::Owned(s)),
            },
            WrapBufferMode::AlwaysWrap => match self.pending.pop_front() {
                None => WrapBufferOutput::None,
                Some(s) => WrapBufferOutput::Wrapping(s),
            },
        }
    }

    fn wrap_at_len(&self) -> usize {
        let already_wrote = match self.mode {
            WrapBufferMode::OnlyWrapOnce { already_wrote } => already_wrote,
            WrapBufferMode::AlwaysWrap => 0,
            WrapBufferMode::NeverWrap => 0, // shouldn't ever happen
        };
        self.wrap_at - already_wrote
    }

    fn drain_all(&mut self) -> Vec<String> {
        if self.buffer.has_non_whitespace() {
            self.pending.push_back(self.buffer.drain());
        } else {
            self.buffer.reset();
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

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Block {
    /// A plain block; just paragraph text.
    Plain,

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

    pub fn without_text_wrapping(to: W) -> Self {
        Self::new(to, OutputOpts { text_width: None })
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
        self.with_block(Block::Plain, |me| {
            // TODO -- the with_block(Plain) won't correctly reset self.current_line, meaning that
            // the first line of the pre block's text will wrap if it were as long as itself plus
            // whatever was on the Plain. I need to write a test for that, and fix it.
            me.without_wrapping(|me| {
                me.pre_mode = true;
                action(&mut PreWriter { output: me });
                me.pre_mode = false;
            });
        })
    }

    pub fn without_wrapping<F>(&mut self, action: F)
    where
        F: for<'a> FnOnce(&mut Self),
    {
        self.flush_pending_lines(false);
        if let Some(wrap) = &mut self.wrap_buffer {
            wrap.disable(self.current_line_chars.count);
        }
        action(self);
        if let Some(wrap) = &mut self.wrap_buffer {
            wrap.enable();
        }
        self.flush_pending_lines(false);
    }

    fn disable_wrapping(&mut self) {
        // There may be text that's pending from the wrapping-enabled mode. Flush it before we start
        // the no-wrap block.
        if let Some(buffer) = &mut self.wrap_buffer {
            for line in buffer.drain_all() {
                self.write_line(&line);
            }
        }
        // has to be in separate if-let for the borrow checker's sake
        if let Some(buffer) = &mut self.wrap_buffer {
            buffer.disable(self.current_line_chars.count);
        }
    }

    fn enable_wrapping(&mut self) {
        if let Some(buffer) = &mut self.wrap_buffer {
            for line in buffer.drain_all() {
                self.ensure_newlines(NewlinesRequest::AtLeast(1));
                self.write_line(&line);
            }
        }
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
                Block::Plain => 2,
                Block::Quote => 2,
                Block::Inlined(_) => 1,
            },
        };
        if newlines > 0 {
            self.ensure_newlines(NewlinesRequest::Exactly(newlines));
        }
    }

    fn flush_pending_lines(&mut self, add_trailing_newline: bool) {
        if add_trailing_newline && self.pending_newlines > 0 {
            self.write_raw("\n");
            self.pending_newlines = 0;
        }
    }

    pub fn write_str(&mut self, text: &str) {
        for resolved_text in self.rewrap_text(text) {
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
    }

    fn next_block(&self) -> Option<Block> {
        self.pending_blocks.last().or_else(|| self.blocks.last()).map(|b| *b)
    }

    fn rewrap_text<'a>(&mut self, text: &'a str) -> impl Iterator<Item = Cow<'a, str>> {
        let Some(wrap_buffer) = &mut self.wrap_buffer else {
            // TODO do I want to do something purely on stack here, for efficiency?
            return vec![Cow::Borrowed(text)].into_iter();
        };
        let wrap = wrap_buffer.wrap_at;
        let text_to_unbuffer = match wrap_buffer.accept(text) {
            WrapBufferOutput::None => return vec![].into_iter(),
            WrapBufferOutput::NotWrapping(s) => return vec![s].into_iter(), // TODO stack-only here, too?
            WrapBufferOutput::Wrapping(s) => s,
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

        TODO I'm double-buffering -- both here (via result: String) and in the WrapBuffer. I should
        figure out what the hell I'm actually trying to do here, and do it just once! lol, I mean
        seriously wth.
        I think that probably means I should do all the buffering in WrapBuffer, which is anyway
        splitting on each newline.
        However, note that I don't actually need the Iterator return value, as I currently have.
        It doesn't save me any memory, since I'd be building the full Vec in memory anyway. And this
        method's approach (as opposed to WrapBuffer's) is to store the multiple lines just in a
        single string, delimited by '\n'. That makes sense, since the calling site is just going to
        split on newline anyway (and needs to regardless of whatever this method does, because of
        the non-wrapping case).

        So:
        - only buffer once
        - probably in WrapBuffer, not here
        - but get rid of this Iterator / Vec stuff (including in WrapBuffer) and just return a
          String instead.

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
        Some(Cow::Owned(result))
    }

    pub fn write_char(&mut self, ch: char) {
        self.write_str(&String::from(ch))
    }

    // Writes text which is assumed to not have any newlines
    fn write_line(&mut self, text: &str) {
        // If we have any pending blocks, handle those first. We need to add a paragraph break, unless the first block
        // is an Inlined. Block::NoWrap isn't a real block, so ignore those here.
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
                Block::Plain => {}
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
                out.with_block(Block::Plain, |out| {
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
                out.with_block(Block::Plain, |out| {
                    out.with_block(Block::Inlined(3), |out| {
                        out.write_str("directly in\nthe inlined block");
                    });
                    out.with_block(Block::Inlined(3), |out| {
                        out.with_block(Block::Plain, |out| out.write_str("paragraph 1"));
                        out.with_block(Block::Plain, |out| out.write_str("paragraph 2"));
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
                    out.with_block(Block::Plain, |out| {
                        out.write_str("First item");
                    });
                    out.with_block(Block::Plain, |out| {
                        out.write_str("It has two paragraphs.");
                    });
                });
                out.write_str("2. ");
                out.with_block(Block::Inlined(3), |out| {
                    out.with_block(Block::Plain, |out| {
                        out.write_str("Second item.");
                    });
                });
                out.write_str("3. ");
                out.with_block(Block::Inlined(3), |out| {
                    out.with_block(Block::Plain, |out| {
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
        fn disable_wrapping_after_nonempty_line() {
            assert_eq!(
                out_to_str_wrapped(9, |out| {
                    out.write_str("Hello, ");
                    out.without_wrapping(|out| {
                        out.write_str("world is a good default");
                    });
                    out.write_str(" text to use.")
                }),
                // Note that without_wrapping(~) buffers so that if it needs to wrap, it'll wrap
                // before the without_wrapping span, not after. Basically, the without_wrapping(~)
                // span counts as a single word.
                //
                /* chars counter:
                123456789 */
                indoc! {r#"
                Hello,
                world is a good default
                text to
                use.
                "#}
            );
        }

        #[test]
        fn disable_wrapping_at_start_of_line() {
            assert_eq!(
                out_to_str_wrapped(9, |out| {
                    out.without_wrapping(|out| {
                        out.write_str("Hello the world");
                    });
                }),
                "Hello the world"
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
        }

        #[test]
        fn word_is_longer_than_wrap() {
            assert_eq!(
                out_to_str_wrapped(9, |out| {
                    out.write_str("hello_the_world_1 a hello_the_world_2 b");
                }),
                indoc! {r#"
                hello_the_world_1
                a
                hello_the_world_2
                b"#}
            );
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
        let mut out = Output::without_text_wrapping(String::new());
        action(&mut out);
        out.take_underlying().unwrap()
    }
}
