use crate::util::words_buffer::{WordBoundary, WordsBuffer};
use std::cmp::PartialEq;
use std::ops::Range;

pub trait SimpleWrite {
    fn write_char(&mut self, ch: char) -> std::io::Result<()>;
    fn flush(&mut self) -> std::io::Result<()>;
}

impl SimpleWrite for String {
    fn write_char(&mut self, ch: char) -> std::io::Result<()> {
        self.push(ch);
        Ok(())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        Ok(())
    }
}

pub struct Stream<W>(pub W);

impl<W: std::io::Write> SimpleWrite for Stream<W> {
    fn write_char(&mut self, ch: char) -> std::io::Result<()> {
        std::io::Write::write_all(&mut self.0, ch.encode_utf8(&mut [0u8; 4]).as_bytes())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.0.flush()
    }
}

struct IndentHandler {
    /// whether the current block is in `<pre>` mode. See [Block] for an explanation as to why this
    /// exists, and isn't instead a `Block::Pre`.
    pre_mode: bool,
    blocks: Vec<Block>,
    pending_blocks: Vec<Block>,
    pending_newlines: usize,
}

pub struct Output<W: SimpleWrite> {
    stream: W,
    indenter: IndentHandler,
    words_buffer: WordsBuffer,
    writing_state: WritingState,
}

impl<W: SimpleWrite> SimpleWrite for Output<W> {
    fn write_char(&mut self, ch: char) -> std::io::Result<()> {
        Self::write_char(self, ch);
        Ok(())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.stream.flush()
    }
}

pub struct PreWriter<'a, W: SimpleWrite> {
    output: &'a mut Output<W>,
}

/// Various block modes.
///
/// Note that there is no `Pre`. Instead, you must use [Output::with_pre_block]. This is because
/// a standard block (ie, one represented by this enum) can have nested blocks, while `pre`s cannot.
/// The way we enforce this is to have standard blocks pushed and popped via [Output::with_block],
/// which takes a `(&mut Self)` action that can then push additional blocks; while `pre` blocks are
/// added via [Output::with_pre_block], which takes a [PreWriter] that only implements
/// [SimpleWrite].
#[derive(Debug, PartialEq, Copy, Clone)]
pub enum Block {
    /// A plain block; just paragraph text.
    Plain,
    /// A quoted block (`> `)
    Quote,
    /// A block that does *not* start with newlines, but does add indentation. It ends in a single
    /// newline. The indentation does not apply to the first line, and any opening newlines for the
    /// first line in this block are ignored.
    ///
    /// This is useful for lists:
    ///
    /// ```md
    /// 1. §Paragraph starts here.§
    /// 2. §Paragraph starts here, and
    ///    indentation continues.§
    /// ```
    ///
    /// (where `§` signifies the start and end of an indented paragraph)
    Indent(usize),
}

#[derive(Debug, PartialEq, Copy, Clone)]
enum WriteAction {
    Char(char),
    EndBlock,
    FlushPendingBlocksAndNewlines,
}

impl<W: SimpleWrite> Output<W> {
    pub fn new(to: W, text_width: Option<usize>) -> Self {
        Self {
            stream: to,
            indenter: IndentHandler::new(),
            words_buffer: text_width.map_or_else(WordsBuffer::disabled, WordsBuffer::new),
            writing_state: WritingState::HaveNotWrittenAnything,
        }
    }

    pub fn without_text_wrapping(to: W) -> Self {
        Self::new(to, None)
    }

    pub fn replace_underlying(&mut self, new: W) -> std::io::Result<W> {
        self.stream.flush()?;
        Ok(std::mem::replace(&mut self.stream, new))
    }

    pub fn with_block(&mut self, block: Block, action: impl FnOnce(&mut Self)) {
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
    pub fn with_pre_block(&mut self, action: impl FnOnce(&mut PreWriter<W>)) {
        self.push_block(Block::Plain);
        self.indenter.pre_mode = true;
        self.without_wrapping(|me| {
            action(&mut PreWriter { output: me });
        });
        self.perform_write(WriteAction::FlushPendingBlocksAndNewlines); // we want the newlines aspect of this flush
        self.indenter.pre_mode = false;
        self.pop_block();
    }

    pub fn without_wrapping(&mut self, action: impl FnOnce(&mut Self)) {
        let old_boundary_mode = self.words_buffer.set_word_boundary(WordBoundary::OnlyAtNewline);
        action(self);
        old_boundary_mode.restore_to(&mut self.words_buffer)
    }

    fn push_block(&mut self, block: Block) {
        self.indenter.pending_blocks.push(block);
    }

    fn pop_block(&mut self) {
        self.perform_write(WriteAction::EndBlock);
        if !self.indenter.pending_blocks.is_empty() {
            // write a blank line for whatever blocks had been enqueued but didn't have content
            self.perform_write(WriteAction::FlushPendingBlocksAndNewlines);
        }
        let newlines = match self.indenter.blocks.pop() {
            None => 0,
            Some(closing_block) => match closing_block {
                Block::Plain => 2,
                Block::Quote => 2,
                Block::Indent(_) => 1,
            },
        };
        self.indenter
            .ensure_newlines(self.writing_state, NewlinesRequest::Exactly(newlines));
    }

    pub fn write_str(&mut self, text: &str) {
        text.chars().for_each(|ch| self.perform_write(WriteAction::Char(ch)));
    }

    pub fn write_char(&mut self, ch: char) {
        self.perform_write(WriteAction::Char(ch));
    }

    /// Writes a char, along with some magic.
    ///
    /// - `Some('\n')` is translated to a [Self::ensure_newlines].
    /// - `None` basically flushes pending blocks, but doesn't write anything else; we use this in [Self::pop_block],
    ///   and it's particularly useful for empty quote blocks (`">"`) and trailing newlines in `pre` blocks.
    fn perform_write(&mut self, write: WriteAction) {
        match write {
            WriteAction::Char(ch) => {
                let indentation = self.indenter.get_indentation_info(Some(ch), self.writing_state);
                let indent_len = indentation.pre_write(&mut self.writing_state, true, &mut self.stream);
                self.words_buffer.shorten_current_line(indent_len);
                if ch != '\n' {
                    // get_indentation_info would have already handled ch if it's a newline
                    self.words_buffer.push(ch, |ch| {
                        indentation.write(&mut self.writing_state, &mut self.stream, ch)
                    });
                }
            }
            WriteAction::EndBlock => {
                if self.words_buffer.has_pending_word() {
                    let indentation = self.indenter.get_indentation_info(None, self.writing_state);
                    indentation.pre_write(&mut self.writing_state, true, &mut self.stream);
                    self.words_buffer.drain_pending_word(|flushed| {
                        indentation.write(&mut self.writing_state, &mut self.stream, flushed);
                        0
                    });
                }
                self.words_buffer.reset();
            }
            WriteAction::FlushPendingBlocksAndNewlines => {
                self.indenter.get_indentation_info(None, self.writing_state).pre_write(
                    &mut self.writing_state,
                    self.words_buffer.has_pending_word(),
                    &mut self.stream,
                );
            }
        };
    }
}

impl<W: SimpleWrite + Default> Output<W> {
    pub fn take_underlying(&mut self) -> std::io::Result<W> {
        self.stream.flush()?;
        Ok(std::mem::take(&mut self.stream))
    }
}

impl<W: SimpleWrite> Drop for Output<W> {
    fn drop(&mut self) {
        if self.indenter.pending_newlines > 0 {
            self.writing_state.write('\n', &mut self.stream);
            self.indenter.pending_newlines = 0;
        }
        if let Err(e) = self.stream.flush() {
            if WritingState::Error != self.writing_state {
                eprintln!("error while writing output: {}", e);
                self.writing_state = WritingState::Error;
            }
        }
    }
}

#[derive(Debug, Default)]
struct IndentationRange {
    /// Whether to include [Block::Indent]] blocks. This will be false for the first line in which we see those blocks.
    include_indent_blocks: bool,
    block_range: Range<usize>,
}

impl IndentationRange {
    fn new(include_indents: bool, block_range: Range<usize>) -> Self {
        Self {
            include_indent_blocks: include_indents,
            block_range,
        }
    }

    fn write(
        &self,
        writing_state: &mut WritingState,
        blocks: &[Block],
        trailing_padding: bool,
        out: &mut impl SimpleWrite,
    ) -> usize {
        let mut wrote = 0;
        // see https://github.com/rust-lang/rust/pull/27186 for why we have to build the range manually
        let mut pending_padding = 0;

        // this form is easier to read than take(.end).skip(.start)
        #[allow(clippy::needless_range_loop)]
        for idx in self.block_range.start..self.block_range.end {
            match blocks[idx] {
                Block::Plain => {}
                Block::Quote => {
                    wrote += pending_padding + 1; // +1 for the '>'
                    (0..pending_padding).for_each(|_| writing_state.write(' ', out));
                    pending_padding = 0;
                    writing_state.write('>', out);
                    pending_padding += 1;
                }
                Block::Indent(size) => {
                    if self.include_indent_blocks {
                        pending_padding += size
                    }
                }
            }
        }
        if trailing_padding {
            (0..pending_padding).for_each(|_| writing_state.write(' ', out));
            wrote += pending_padding;
        }
        wrote
    }
}

impl IndentInfo<'_> {
    fn pre_write(&self, writing_state: &mut WritingState, trailing_padding: bool, out: &mut impl SimpleWrite) -> usize {
        if self.static_info.newlines > 0 {
            for _ in 0..(self.static_info.newlines - 1) {
                writing_state.write('\n', out);
                self.static_info
                    .indents_between_newlines
                    .write(writing_state, self.blocks, false, out);
            }
            writing_state.write('\n', out);
        }
        self.static_info
            .last_indent
            .write(writing_state, self.blocks, trailing_padding, out)
    }

    fn write(&self, writing_state: &mut WritingState, out: &mut impl SimpleWrite, ch: char) -> usize {
        if ch == '\n' {
            writing_state.write('\n', out);
            let indentation = IndentationRange::new(true, 0..self.blocks.len());
            indentation.write(writing_state, self.blocks, true, out)
        } else {
            writing_state.write(ch, out);
            writing_state.update_to(WritingState::Normal);
            0
        }
    }
}

struct StaticIndentInfo {
    writing_state: WritingState,
    newlines: usize,
    indents_between_newlines: IndentationRange,
    last_indent: IndentationRange,
}

impl StaticIndentInfo {
    fn new(writing_state: WritingState) -> Self {
        Self {
            writing_state,
            newlines: 0,
            indents_between_newlines: IndentationRange::default(),
            last_indent: IndentationRange::default(),
        }
    }

    fn build(self, blocks: &[Block]) -> IndentInfo {
        IndentInfo {
            blocks,
            static_info: self,
        }
    }
}

struct IndentInfo<'a> {
    blocks: &'a [Block],
    static_info: StaticIndentInfo,
}

impl IndentHandler {
    fn new() -> Self {
        Self {
            pre_mode: false,
            blocks: Vec::default(),
            pending_blocks: Vec::default(),
            pending_newlines: 0,
        }
    }

    fn get_indentation_info(&mut self, ch: Option<char>, state: WritingState) -> IndentInfo {
        // #199: I have a number of branches here. Can I nest some of them, so that in the happy path of a non-newline
        //       char, I just have a single check?
        let mut indent_builder = StaticIndentInfo::new(state);

        // If we have any pending blocks, handle those first. We need to add a paragraph break, unless the first block
        // is an Indent.
        match self.pending_blocks.first() {
            None => {}
            Some(Block::Indent(_)) => {
                indent_builder.writing_state.update_to(WritingState::IgnoringNewlines);
            }
            Some(_) => self.ensure_newlines(indent_builder.writing_state, NewlinesRequest::AtLeast(2)),
        }

        // Translate a newline char into an ensure_newlines request
        if matches!(ch, Some('\n')) {
            self.ensure_newlines(indent_builder.writing_state, NewlinesRequest::Exactly(1));
            return indent_builder.build(&self.blocks);
        }

        // need_full_indent tells us whether Block::Indent's indentation gets included in write_indent. It's false for
        // the first line that includes a new Indent, and then true for subsequent lines in that block.
        let need_full_indent = if self.pending_newlines > 0 {
            indent_builder.newlines = self.pending_newlines;
            self.pending_newlines = 0;
            let range = 0..self.blocks.len();
            indent_builder.indents_between_newlines = IndentationRange::new(true, range);
            true
        } else {
            matches!(state, WritingState::HaveNotWrittenAnything)
        };

        // Append the new blocks, and then write the indent if we need it.
        // When we write that indent, though, only write it until the first new Indent (exclusive).
        // (#199 that doesn't seem to actually how this code is organized. See the #199 comment above.)
        let range = if need_full_indent {
            let indent_end_idx = self.blocks.len()
                + self
                    .pending_blocks
                    .iter()
                    .position(|b| matches!(b, Block::Indent(_)))
                    .unwrap_or(self.pending_blocks.len());
            self.blocks.append(&mut self.pending_blocks);
            0..indent_end_idx
        } else {
            let prev_blocks_len = self.blocks.len();
            self.blocks.append(&mut self.pending_blocks);
            prev_blocks_len..self.blocks.len()
        };
        indent_builder.last_indent = IndentationRange::new(need_full_indent, range);

        indent_builder.build(&self.blocks)
    }

    fn ensure_newlines(&mut self, writing_state: WritingState, request: NewlinesRequest) {
        match writing_state {
            WritingState::Normal => match request {
                NewlinesRequest::Exactly(n) => {
                    if self.pre_mode {
                        self.pending_newlines += n;
                    } else {
                        self.pending_newlines = n;
                    }
                }
                NewlinesRequest::AtLeast(n) => {
                    if self.pending_newlines < n {
                        self.pending_newlines = n;
                    }
                }
            },
            WritingState::HaveNotWrittenAnything | WritingState::IgnoringNewlines | WritingState::Error => {}
        }
    }
}

impl<W: SimpleWrite> PreWriter<'_, W> {
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

#[derive(PartialEq, Copy, Clone, Debug)]
enum WritingState {
    /// We haven't written any text; that is, there hasn't been any invocation of [Output::write_str] or
    /// [Output::write_char]. We may have queued up some blocks.
    HaveNotWrittenAnything,

    /// Any newline requests (via [Output::ensure_newlines] will be ignored. This happens for the first line following a
    /// new [Block::Indent].
    IgnoringNewlines,

    /// No special state. Just write the chars!
    Normal,

    /// There's been an error in writing. From here on in, everything will be a noop.
    Error,
}

impl WritingState {
    fn update_to(&mut self, new_state: WritingState) {
        if matches!(self, WritingState::Error) {
            return;
        }
        *self = new_state;
    }

    fn write(&mut self, ch: char, out: &mut impl SimpleWrite) {
        if matches!(*self, WritingState::Error) {
            return;
        }
        if let Err(e) = out.write_char(ch) {
            eprintln!("error while writing output: {}", e);
            *self = WritingState::Error;
        }
        if matches!(*self, WritingState::HaveNotWrittenAnything) {
            *self = WritingState::Normal;
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
    fn plain_block_with_multiple_newlines() {
        assert_eq!(
            out_to_str(|out| {
                out.with_block(Block::Plain, |out| {
                    out.write_str("hello\nworld\n\nagain");
                });
            }),
            indoc! {r#"
                hello
                world
                again"#}
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
    fn indent_block() {
        assert_eq!(
            out_to_str(|out| {
                out.write_str("before");
                out.with_block(Block::Plain, |out| {
                    out.with_block(Block::Indent(1), |out| {
                        out.write_str("directly in\nthe indent block");
                    });
                    out.with_block(Block::Indent(3), |out| {
                        out.with_block(Block::Plain, |out| out.write_str("paragraph 1"));
                        out.with_block(Block::Plain, |out| out.write_str("paragraph 2"));
                    });
                });
            }),
            indoc! {r#"
                before

                directly in
                 the indent block
                paragraph 1
                
                   paragraph 2"#}
        )
    }

    #[test]
    fn indent_block_suppresses_opening_newline_in_first_block() {
        assert_eq!(
            out_to_str(|out| {
                out.write_str("[before-1]");
                out.with_block(Block::Indent(2), |out| {
                    out.with_block(Block::Plain, |out| {
                        out.write_str("\nFirst paragraph\n\nhas newlines");
                        out.with_block(Block::Plain, |out| {
                            out.write_str("Inner paragraph\nhas a newline");
                        });
                    });
                });
                out.write_str("[after-1]");

                // even works in pre_mode! Note that this has what look like extra newlines, due to how blocks get
                // newlines. This can't happen in practice, since you can't have blocks inside a `pre` due to how we
                // do with_pre_block vs with_block. See the docs on Block for more.
                out.indenter.pre_mode = true;
                out.write_str("[before-2]");
                out.with_block(Block::Indent(2), |out| {
                    out.with_block(Block::Plain, |out| {
                        out.write_str("\nPre\nparagraph\n\nhas newlines");
                    });
                });
                out.write_str("[after-2]");
            }),
            indoc! {r#"
                [before-1]First paragraph
                  has newlines
                
                  Inner paragraph
                  has a newline
                [after-1][before-2]Pre
                  paragraph
                
                  has newlines


                [after-2]"#}
        )
    }

    // This example is what we actually expect in practice, from a list item.
    #[test]
    fn indent_block_used_like_list_item() {
        assert_eq!(
            out_to_str(|out| {
                out.write_str("1. ");
                out.with_block(Block::Indent(3), |out| {
                    out.with_block(Block::Plain, |out| {
                        out.write_str("First item");
                    });
                    out.with_block(Block::Plain, |out| {
                        out.write_str("It has two paragraphs.");
                    });
                });
                out.write_str("2. ");
                out.with_block(Block::Indent(3), |out| {
                    out.with_block(Block::Plain, |out| {
                        out.write_str("Second item.");
                    });
                });
                out.write_str("3. ");
                out.with_block(Block::Indent(3), |out| {
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
    fn nested_indents() {
        assert_eq!(
            out_to_str(|out| {
                out.write_str("1. ");
                out.with_block(Block::Indent(3), |out| {
                    out.with_block(Block::Plain, |out| {
                        out.write_str("First item");
                    });
                    out.with_block(Block::Plain, |out| {
                        out.write_str("It has two paragraphs.");
                    });

                    out.write_str("A. ");
                    out.with_block(Block::Indent(3), |out| {
                        out.with_block(Block::Plain, |out| {
                            out.write_str("Inner item");
                        });
                        out.with_block(Block::Plain, |out| {
                            out.write_str("It also has two paragraphs.");
                        });
                    });
                });
            }),
            indoc! {r#"
                1. First item

                   It has two paragraphs.

                   A. Inner item

                      It also has two paragraphs."#}
        );
    }

    #[test]
    fn indent_plain_then_pre() {
        assert_eq!(
            out_to_str(|out| {
                out.with_block(Block::Indent(2), |out| {
                    out.with_block(Block::Plain, |out| {
                        out.write_str("A");
                    });
                    out.with_pre_block(|out| {
                        out.write_str("B");
                    });
                });
            }),
            indoc! {r#"
                A

                  B"#}
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
    fn pre_block_ends_with_newline() {
        assert_eq!(
            out_to_str(|out| {
                out.with_pre_block(|out| {
                    out.write_str("A\n");
                });
            }),
            indoc! {r#"
                A
            "#}
        );
    }

    #[test]
    fn pre_block_ends_with_three_newlines() {
        assert_eq!(
            out_to_str(|out| {
                out.with_pre_block(|out| {
                    out.write_str("A\n\n\n");
                });
            }),
            indoc! {r#"
                A
                
                
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
                use."#}
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

        #[test]
        fn wrapping_respects_quote_blocks() {
            assert_eq!(
                // line length of 11 is just enough for "hello world". So:
                // - as a plain paragraph, it should fit without wrapping
                // - but with quoting, the extra two chars of the "> " will cause it to wrap
                out_to_str_wrapped(11, |out| {
                    out.with_block(Block::Plain, |out| out.write_str("hello world"));
                    out.with_block(Block::Quote, |out| out.write_str("hello world"));
                }),
                indoc! {r#"
                hello world
                
                > hello
                > world"#}
            );
        }

        #[test]
        fn wrapping_respects_indentation_blocks() {
            assert_eq!(
                // line length of 11 is just enough for "hello world". So:
                // - as a plain paragraph, it should fit without wrapping
                // - but with quoting, the extra two chars of the "> " will cause it to wrap
                out_to_str_wrapped(11, |out| {
                    out.with_block(Block::Plain, |out| out.write_str("hello world"));

                    // both write_str calls just barely fit
                    out.with_block(Block::Indent(2), |out| {
                        out.write_str("1. hi world "); // 11 chars; "world" will just barely fit
                        out.write_str("next line"); // "line" will just barely fit
                    });

                    // first write_str call just barely fits; second just barely wraps
                    out.with_block(Block::Indent(2), |out| {
                        out.write_str("2. hi world "); // 11 chars; "world" will just barely fit
                        out.write_str("next lines"); // "line" will just barely not fit
                    });

                    // first write_str call just barely wraps; second just barely fits
                    out.with_block(Block::Indent(2), |out| {
                        out.write_str("3. hi "); // 11 chars; "worlds" will just barely not fit
                        out.write_str("  worlds hi"); // "hi" will just barely fit, including the 2-char indent
                    });

                    // both write_str calls just barely wrap
                    out.with_block(Block::Indent(2), |out| {
                        out.write_str("4. hi "); // 11 chars; "worlds" will just barely not fit
                        out.write_str("  worlds hey"); // "hey" will just not barely fit, with the 2-char indent
                    });
                }),
                indoc! {r#"
                hello world
                
                1. hi world
                  next line
                2. hi world
                  next
                  lines
                3. hi
                  worlds hi
                4. hi
                  worlds
                  hey"#}
            );
        }

        #[test]
        fn wrapping_ignores_pre() {
            assert_eq!(
                // line length of 3 should cause "hello world" to wrap, but that's disabled within `pre`
                out_to_str_wrapped(3, |out| {
                    out.with_pre_block(|out| out.write_str("hello world"));
                }),
                "hello world",
            );
        }

        fn out_to_str_wrapped(wrap: usize, action: impl FnOnce(&mut Output<String>)) -> String {
            let mut out = Output::new(String::new(), Some(wrap));
            action(&mut out);
            out.take_underlying().unwrap()
        }
    }

    mod stream_simple_write {
        use super::*;

        #[test]
        fn single_byte_char() -> std::io::Result<()> {
            let mut bb = Vec::new();
            let mut stream = Stream(&mut bb);
            SimpleWrite::write_char(&mut stream, 'a')?;
            assert_eq!(&bb, b"a");
            Ok(())
        }

        #[test]
        fn multi_byte_char() -> std::io::Result<()> {
            let mut bb = Vec::new();
            let mut stream = Stream(&mut bb);
            SimpleWrite::write_char(&mut stream, '☃')?;
            let expected: &[u8] = &[0xE2, 0x98, 0x83];
            assert_eq!(&bb, expected);
            Ok(())
        }

        #[test]
        fn four_byte_char() -> std::io::Result<()> {
            let mut bb = Vec::new();
            let mut stream = Stream(&mut bb);
            SimpleWrite::write_char(&mut stream, '𝄞')?;
            let expected: &[u8] = &[0xF0, 0x9D, 0x84, 0x9E];
            assert_eq!(&bb, expected);
            Ok(())
        }
    }

    fn out_to_str(action: impl FnOnce(&mut Output<String>)) -> String {
        let mut out = Output::without_text_wrapping(String::new());
        action(&mut out);
        out.take_underlying().unwrap()
    }
}
