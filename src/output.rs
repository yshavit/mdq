use crate::words_buffer::{WordBoundary, WordsBuffer};
use std::cmp::PartialEq;
use std::ops::Range;

pub trait SimpleWrite {
    // TODO should these take a non-mut self, and the impl-for is on the mut?
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
        self.0.write(ch.encode_utf8(&mut [0u8; 4]).as_bytes())?;
        Ok(())
    }

    fn flush(&mut self) -> std::io::Result<()> {
        self.0.flush()
    }
}

#[derive(Debug)]
pub struct OutputOpts {
    pub text_width: Option<usize>,
}

struct CharsWriter {
    /// whether the current block is in `<pre>` mode. See [Block] for an explanation as to why this
    /// exists, and isn't instead a `Block::Pre`.
    pre_mode: bool,
    blocks: Vec<Block>,
    pending_blocks: Vec<Block>,
    pending_newlines: usize,
    pending_padding_after_indent: usize,
}

pub struct Output<W: SimpleWrite> {
    stream: W,
    writer: CharsWriter,
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
    FlushChars,
    FlushPendingBlocksAndNewlines,
}

impl<W: SimpleWrite> Output<W> {
    pub fn new(to: W, opts: OutputOpts) -> Self {
        Self {
            stream: to,
            writer: CharsWriter::new(),
            words_buffer: opts
                .text_width
                .map_or_else(WordsBuffer::disabled, |width| WordsBuffer::new(width)),
            writing_state: WritingState::HaveNotWrittenAnything,
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
        self.push_block(Block::Plain);
        self.writer.pre_mode = true;
        self.without_wrapping(|me| {
            action(&mut PreWriter { output: me });
        });
        self.perform_write(WriteAction::FlushPendingBlocksAndNewlines); // we want the newlines aspect of this flush
        self.writer.pre_mode = false;
        self.pop_block();
    }

    pub fn without_wrapping<F>(&mut self, action: F)
    where
        F: for<'a> FnOnce(&mut Self), // TODO change this to a PreWriter, so we don't have reentry issues
                                      //  (that requires more changes downstream than I want to do right now).
    {
        let old_boundary_mode = self.words_buffer.set_word_boundary(WordBoundary::Never);
        action(self);
        self.words_buffer.set_word_boundary(old_boundary_mode);
    }

    fn push_block(&mut self, block: Block) {
        self.writer.pending_blocks.push(block);
    }

    fn pop_block(&mut self) {
        self.perform_write(WriteAction::FlushChars);
        if !self.writer.pending_blocks.is_empty() {
            // write a blank line for whatever blocks had been enqueued but didn't have content
            self.perform_write(WriteAction::FlushPendingBlocksAndNewlines);
        }
        let newlines = match self.writer.blocks.pop() {
            None => 0,
            Some(closing_block) => match closing_block {
                Block::Plain => 2,
                Block::Quote => 2,
                Block::Indent(_) => 1,
            },
        };
        self.writer
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
    ///    and it's particularly useful for empty quote blocks (`">"`) and trailing newlines in `pre` blocks.
    fn perform_write(&mut self, write: WriteAction) {
        let new_state = match write {
            WriteAction::Char(ch) => {
                self.writer
                    .write_single_optional_char(Some(ch), self.writing_state, |ch| {
                        self.words_buffer.push(ch, |ch| {
                            // (comment just for formatting)
                            self.writing_state.write_raw(ch, &mut self.stream);
                        });
                    })
            }
            WriteAction::FlushChars => {
                let mut new_state = self.writing_state;
                self.words_buffer.drain(|flushed| {
                    new_state = self
                        .writer
                        .write_single_optional_char(Some(flushed), self.writing_state, |ch| {
                            // (comment just for formatting)
                            self.writing_state.write_raw(ch, &mut self.stream);
                        })
                });
                new_state
            }
            WriteAction::FlushPendingBlocksAndNewlines => {
                let mut new_state = self.writing_state;
                // In all the call sites today, the words_buffer should be empty by the time we get here. But, as a
                // belt-and-suspenders approach, let's flush the words_buffer anyway.
                self.words_buffer.drain(|flushed| {
                    new_state = self
                        .writer
                        .write_single_optional_char(Some(flushed), self.writing_state, |ch| {
                            // (comment just for formatting)
                            self.writing_state.write_raw(ch, &mut self.stream);
                        });
                });
                new_state.update_to(self.writer.write_single_optional_char(None, self.writing_state, |ch| {
                    self.words_buffer.push(ch, |ch| {
                        // (comment just for formatting)
                        self.writing_state.write_raw(ch, &mut self.stream);
                    });
                }));
                new_state
            }
        };
        self.writing_state.update_to(new_state);
    }
}

impl<W: SimpleWrite> Output<W>
where
    W: Default,
{
    pub fn take_underlying(&mut self) -> std::io::Result<W> {
        self.stream.flush()?;
        Ok(std::mem::take(&mut self.stream))
    }
}

impl<W: SimpleWrite> Drop for Output<W> {
    fn drop(&mut self) {
        self.words_buffer.drain(|flushed| {
            let new_state = self
                .writer
                .write_single_optional_char(Some(flushed), self.writing_state, |ch| {
                    // (comment just for formatting)
                    self.writing_state.write_raw(ch, &mut self.stream);
                });
            self.writing_state.update_to(new_state);
        });
        if self.writer.pending_newlines > 0 {
            self.writing_state.write_raw('\n', &mut self.stream);
            self.writer.pending_newlines = 0;
        }
        if let Err(e) = self.stream.flush() {
            if WritingState::Error != self.writing_state {
                eprintln!("error while writing output: {}", e);
                self.writing_state = WritingState::Error;
            }
        }
    }
}

impl CharsWriter {
    fn new() -> Self {
        Self {
            pre_mode: false,
            blocks: Vec::default(),
            pending_blocks: Vec::default(),
            pending_newlines: 0,
            pending_padding_after_indent: 0,
        }
    }

    fn write_single_optional_char<F>(&mut self, ch: Option<char>, state: WritingState, mut action: F) -> WritingState
    where
        F: FnMut(char),
    {
        // #199: I have a number of branches here. Can I nest some of them, so that in the happy path of a non-newline
        //       char, I just have a single check?
        let mut resulting_state = state;

        // If we have any pending blocks, handle those first. We need to add a paragraph break, unless the first block
        // is an Indent.
        match self.pending_blocks.first() {
            None => {}
            Some(Block::Indent(_)) => {
                resulting_state.update_to(WritingState::IgnoringNewlines);
            }
            Some(_) => self.ensure_newlines(resulting_state, NewlinesRequest::AtLeast(2)),
        }

        // Translate a newline char into an ensure_newlines request
        if matches!(ch, Some('\n')) {
            self.ensure_newlines(resulting_state, NewlinesRequest::Exactly(1));
            return resulting_state;
        }

        // print pending newlines before we append any new blocks; also note whether we need to write the full indent
        // (#199: what actually is "need full indent"?)
        // Oh, I see -- it's a flag that toggles whether Block::Indent's indentation gets included in write_indent
        let need_full_indent = if self.pending_newlines > 0 {
            for _ in 0..(self.pending_newlines - 1) {
                action('\n');
                self.write_indent(true, None, |ch| action(ch));
            }
            action('\n'); // we'll do this indent after we add the pending blocks
            self.pending_newlines = 0;
            true
        } else {
            matches!(state, WritingState::HaveNotWrittenAnything)
        };

        // Append the new blocks, and then write the indent if we need it.
        // When we write that indent, though, only write it until the first new Indent (exclusive).
        // (#199 that doesn't seem to actually how this code is organized. See the #199 comment above.)
        if need_full_indent {
            let indent_end_idx = self.blocks.len()
                + self
                    .pending_blocks
                    .iter()
                    .position(|b| matches!(b, Block::Indent(_)))
                    .unwrap_or_else(|| self.pending_blocks.len());
            self.blocks.append(&mut self.pending_blocks);
            self.write_indent(true, Some(0..indent_end_idx), |ch| action(ch));
        } else {
            self.write_padding_after_indent(|ch| action(ch));
            let prev_blocks_len = self.blocks.len();
            self.blocks.append(&mut self.pending_blocks);
            self.write_indent(false, Some(prev_blocks_len..self.blocks.len()), |ch| action(ch));
        }

        // Finally, write the text
        if let Some(ch) = ch {
            self.write_padding_after_indent(|ch| action(ch));
            action(ch); // will not be a '\n', since that got translated to ensure_newlines above.
            resulting_state.update_to(WritingState::Normal);
        }
        self.pending_padding_after_indent = 0; // #199 this is redundant if we called self.write_padding_after_indent()
        resulting_state
    }

    /// Write an indentation for a given range of indentation block. If `include_indents` is false, `Indent` blocks
    /// will be disregarded. Do this for the first line in which those indents appear (and only there).
    fn write_indent<F>(&mut self, include_indents: bool, range: Option<Range<usize>>, mut action: F)
    where
        F: FnMut(char),
    {
        self.pending_padding_after_indent = 0;
        for idx in range.unwrap_or_else(|| 0..self.blocks.len()) {
            match &self.blocks[idx] {
                Block::Plain => {}
                Block::Quote => {
                    self.write_padding_after_indent(|ch| action(ch));
                    action('>');
                    self.pending_padding_after_indent += 1;
                }
                Block::Indent(size) => {
                    if include_indents {
                        self.pending_padding_after_indent += size;
                    }
                }
            }
        }
    }

    fn write_padding_after_indent<F>(&mut self, mut action: F)
    where
        F: FnMut(char),
    {
        (0..self.pending_padding_after_indent).for_each(|_| action(' '));
        self.pending_padding_after_indent = 0;
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

    fn write_raw<W: SimpleWrite>(&mut self, ch: char, out: &mut W) {
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
                // do with_pre_block vs with_block. See the docs on Block for mor.
                out.writer.pre_mode = true;
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
                > world
                "#}
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
