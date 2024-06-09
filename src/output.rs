use std::cmp::PartialEq;
use std::io::Write;

pub struct Output<W: Write> {
    stream: W,
    pre_mode: bool,
    blocks: Vec<Block>, // TODO do we need this? why not just always add the block, and always pop it?
    pending_blocks: Vec<Block>,
    pending_newlines: usize,
    writing_state: WritingState,
}

pub struct PreWriter<'a, W: Write> {
    output: &'a mut Output<W>,
}

#[derive(Debug, PartialEq)]
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
    ///     1. §Paragraph starts here.§
    ///     2. §Paragraph starts here, and
    ///        indentation continues.§
    ///
    /// (where `§` signifies the start and end of an inlined paragraph)
    Inlined(usize),
}

impl<W: Write> Output<W> {
    pub fn new(to: W) -> Self {
        Self {
            stream: to, // TODO use io::BufWriter?
            pre_mode: false,
            blocks: Vec::default(),
            pending_blocks: Vec::default(),
            pending_newlines: 0,
            writing_state: WritingState::HaveNotWrittenAnything,
        }
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
        self.pre_mode = true;
        action(&mut PreWriter { output: self });
        self.pre_mode = false;
        self.pop_block();
    }

    pub fn push_block(&mut self, block: Block) {
        self.pending_blocks.push(block);
    }

    pub fn pop_block(&mut self) {
        if !self.pending_blocks.is_empty() {
            self.write_str(""); // write a blank line for whatever blocks had been enqueued
        }
        let newlines = match self.blocks.pop() {
            None => 0,
            Some(closing_block) => match closing_block {
                Block::Plain => 2,
                Block::Quote => 2,
                Block::Inlined(_) => 1,
            },
        };
        self.ensure_newlines(newlines);
    }

    pub fn write_str(&mut self, text: &str) {
        let newlines_re = if self.pre_mode { "\n" } else { "\n+" };
        let lines = regex::Regex::new(newlines_re).unwrap();
        let mut lines = lines.split(text).peekable();
        while let Some(line) = lines.next() {
            self.write_line(line);
            if let Some(_) = lines.peek() {
                self.ensure_newlines(1);
            }
        }
    }

    pub fn write_char(&mut self, ch: char) {
        // TODO make this better if I need to; for now, I'm just establishing the API.
        // On the other hand, it results in extra allocations, and it's always for literals; so maybe I should rm it?
        // (It's a micro-optimization either way!)
        self.write_str(&String::from(ch))
    }

    // Writes text which is assumed to not have any newlines
    fn write_line(&mut self, text: &str) {
        // If we have any pending blocks, handle those first. We need to add a paragraph break, unless the first block
        // is an Inlined.
        match self.pending_blocks.first() {
            None | Some(Block::Inlined(_)) => {}
            Some(_) => {
                self.ensure_newlines(2);
            }
        }

        // print the newlines before we append the new blocks
        let need_indent = if self.pending_newlines > 0 {
            for _ in 0..(self.pending_newlines - 1) {
                self.write_raw("\n");
                self.write_indent();
            }
            self.write_raw("\n"); // we'll do this indent after we add the pending blocks
            self.pending_newlines = 0;
            true
        } else {
            matches!(self.writing_state, WritingState::HaveNotWrittenAnything)
        };

        // Append the new blocks, and then write the indent if we need it
        self.blocks.append(&mut self.pending_blocks);
        if need_indent {
            self.write_indent();
        }

        match self.writing_state {
            WritingState::DirectlyAfterBlockQuote => {
                if !text.is_empty() {
                    // Otherwise, save the DirectlyAfterBlockQuote state for the next invocation
                    self.set_writing_state(WritingState::Normal);
                    self.write_raw(" ");
                }
            }
            WritingState::HaveNotWrittenAnything => self.set_writing_state(WritingState::Normal),
            WritingState::Normal | WritingState::Error => {}
        }

        // Finally, write the text
        self.write_raw(text);
    }

    fn write_indent(&mut self) {
        let mut directly_after_block_quote = false;
        for idx in 0..self.blocks.len() {
            match &self.blocks[idx] {
                Block::Plain => {}
                Block::Quote => {
                    if directly_after_block_quote {
                        self.write_raw(" ");
                    }
                    self.write_raw(">");
                    directly_after_block_quote = true;
                }
                Block::Inlined(size) => {
                    (0..*size).for_each(|_| self.write_raw(" "));
                    directly_after_block_quote = false;
                }
            }
        }
        self.set_writing_state(if directly_after_block_quote {
            WritingState::DirectlyAfterBlockQuote
        } else {
            WritingState::Normal
        });
    }

    fn ensure_newlines(&mut self, count: usize) {
        match self.writing_state {
            WritingState::DirectlyAfterBlockQuote | WritingState::Normal => {
                if self.pending_newlines < count {
                    self.pending_newlines = count;
                }
            }
            WritingState::HaveNotWrittenAnything | WritingState::Error => {}
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
        if let Err(e) = self.stream.write(text.as_bytes()) {
            eprintln!("error while writing output: {}", e);
            self.writing_state = WritingState::Error;
        }
    }
}

impl<W: Write> Output<W>
where
    W: Default,
{
    pub fn take_underlying(&mut self) -> std::io::Result<W> {
        self.stream.flush()?;
        Ok(std::mem::take(&mut self.stream))
    }
}

impl<W: Write> Drop for Output<W> {
    fn drop(&mut self) {
        if let Err(e) = self.stream.flush() {
            if WritingState::Error != self.writing_state {
                eprintln!("error while writing output: {}", e);
                self.writing_state = WritingState::Error;
            }
        }
    }
}

impl<'a, W: Write> PreWriter<'a, W> {
    pub fn write_str(&mut self, text: &str) {
        self.output.write_str(text)
    }

    pub fn write_char(&mut self, ch: char) {
        self.output.write_char(ch);
    }
}

#[derive(PartialEq, Copy, Clone)]
enum WritingState {
    /// We haven't written any text; that is, there hasn't been any invocation of [Output::write_str] or
    /// [Output::write_char]. We may have queued up some blocks.
    HaveNotWrittenAnything,
    /// We just wrote the indent
    DirectlyAfterBlockQuote,
    /// No special state. Just write the chars!
    Normal,
    /// There's been an error in writing. From here on in, everything will be a noop.
    Error,
}

#[cfg(test)]
mod tests {
    use std::fmt::Error;

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
            indoc! {r#">"#} // TODO the extra space actually shouldn't be there. I think I need a new state, "Right after indent".
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

    fn out_to_str<F>(action: F) -> String
    where
        F: FnOnce(&mut Output<Vec<u8>>),
    {
        let mut out = Output::new(vec![]);
        action(&mut out);
        let vec = out.take_underlying().unwrap();
        String::from_utf8(vec).map_err(|_| Error).unwrap()
    }
}
