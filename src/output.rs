use std::cmp::PartialEq;
use std::io::Write;

pub struct Output<W: Write> {
    stream: W,
    pre_mode: bool,
    blocks: Vec<BlockClose>, // TODO do we need this? why not just always add the block, and always pop it?
    indents: Vec<Block>,
    pending_indents: Vec<Block>,
    pending_newlines: usize,
    writing_state: WritingState,
}

pub struct PreWriter<'a, W: Write> {
    output: &'a mut Output<W>,
}

#[derive(Debug,PartialEq)]
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
    Inlined(usize)
}

impl<W: Write> Output<W> {
    pub fn new(to: W) -> Self {
        Self {
            stream: to, // TODO use io::BufWriter?
            pre_mode: false,
            blocks: Vec::default(),
            indents: Vec::default(),
            pending_indents: Vec::default(),
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
        let prev_is_inline = self.indents.last().map(|b| matches!(b, Block::Inlined(_))).unwrap_or(false);
        let (block_close, add_newlines) = match block {
            Block::Plain => (BlockClose::NoAction, true),
            b @ Block::Quote => {
                self.pending_indents.push(b);
                (BlockClose::EndQuote, true)
            }
            b @ Block::Inlined(_) => {
                self.pending_indents.push(b);
                (BlockClose::EndInlined, false)
            }
        };
        if add_newlines && prev_is_inline {
            self.ensure_newlines(2);
        }
        self.blocks.push(block_close);
    }

    pub fn pop_block(&mut self) {
        if !self.pending_indents.is_empty() {
            self.write_str(""); // flush the indents
        }
        let newlines = match self.blocks.pop() {
            None => 2,
            Some(block_close) => match block_close {
                BlockClose::NoAction => 2,
                BlockClose::EndQuote => {
                    self.indents.pop();
                    2
                }
                BlockClose::EndInlined => {
                    self.indents.pop();
                    1
                }
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

    fn write_indent(&mut self) {
        for idx in 0..self.indents.len() {
            match &self.indents[idx] {
                Block::Plain => {},
                Block::Quote => { self.write_raw(">")},
                Block::Inlined(size) => {(0..*size).for_each(|_| self.write_raw(" "))},
            }
        }
    }

    fn write_line(&mut self, text: &str) {
        if let WritingState::HaveNotWrittenAnything = self.writing_state {
            if self.pending_indents.is_empty() && text.is_empty() {
                // ie, we're not about to write anything (including not any indents)
                return;
            }
        }
        // If we have any newlines that we need, do those first. But also record whether we need
        // to print the indent once we add any pending indents.
        let should_print_indent = match self.writing_state {
            WritingState::HaveNotWrittenAnything => {
                true // first line of text, but if we have indents, we need to write those first
            }
            WritingState::WroteSome => {
                // The first newline is just a newline, because it ended whatever was there previously.
                // But after that, we also want to write our indents.
                if self.pending_newlines > 0 {
                    self.write_raw("\n");
                    for _ in 1..self.pending_newlines {
                        // from 1 because we already wrote the first
                        self.write_indent();
                        self.write_raw("\n");
                    }
                    true // we wrote a newline, so we need to print the new indent
                } else {
                    false
                }
            }
            WritingState::Error => {
                return;
            }
        };
        self.pending_newlines = 0;

        self.indents.append(&mut self.pending_indents);

        if should_print_indent && !self.indents.is_empty() {
            if !text.is_empty() {
                self.write_raw(" ");
            }
        }

        self.write_raw(text);
    }

    fn ensure_newlines(&mut self, count: usize) {
        if let WritingState::WroteSome = self.writing_state {
            if self.pending_newlines < count {
                self.pending_newlines = count;
            }
        }
    }

    fn write_raw(&mut self, text: &str) {
        match self.writing_state {
            WritingState::HaveNotWrittenAnything => self.writing_state = WritingState::WroteSome,
            WritingState::WroteSome => {}
            WritingState::Error => {
                return;
            }
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

#[derive(PartialEq)]
enum WritingState {
    HaveNotWrittenAnything,
    WroteSome,
    Error,
}

enum BlockClose {
    NoAction,
    EndQuote,
    EndInlined,
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fmt::{Display, Error};

    #[test]
    fn simple_string() {
        let mut out = Output::new(vec![]);

        out.write_str("hello");

        assert_eq!("hello", out.to_string());
    }

    #[test]
    fn pop_block_when_there_is_none() {
        let mut out = Output::new(vec![]);

        out.pop_block(); // no-op
        out.write_str("hello world");

        assert_eq!("hello world", out.to_string());
    }

    #[test]
    fn plain_block() {
        let mut out = Output::new(vec![]);

        write_test_block(&mut out, Block::Plain);

        assert_eq!(
            ["before", "", "hello", "world", "", "after",].join("\n"),
            out.to_string()
        );
    }

    #[test]
    fn indent_block() {
        let mut out = Output::new(vec![]);

        write_test_block(&mut out, Block::Quote);

        assert_eq!(
            ["before", "", "> hello", "> world", "", "after",].join("\n"),
            out.to_string()
        );
    }

    #[test]
    fn pre_block() {
        let mut out = Output::new(vec![]);

        out.write_str("before");
        out.with_pre_block(|out| {
            out.write_str("hello\n\nworld");
        });
        out.write_str("after");

        assert_eq!(
            ["before", "", "hello", "", "world", "", "after",].join("\n"),
            out.to_string()
        );
    }

    #[test]
    fn pre_block_with_small_writes() {
        let mut out = Output::new(vec![]);

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

        assert_eq!(
            ["before", "", "> ```", "> my code", "> ```", "", "after",].join("\n"),
            out.to_string()
        );
    }

    #[test]
    fn nested_blocks() {
        let mut out = Output::new(vec![]);

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

        assert_eq!(
            [
                "> hello world",
                ">",
                ">> second level",
                ">>",
                ">> ```rust",
                ">> some code",
                ">> with",
                ">>",
                ">> line breaks",
                ">> ```",
                "",
                "after",
            ]
            .join("\n"),
            out.to_string()
        );
    }

    #[test]
    fn nested_blocks_jump_a_few() {
        let mut out = Output::new(vec![]);

        out.write_str("before");
        out.with_block(Block::Quote, |out| {
            out.with_block(Block::Quote, |out| {
                out.with_block(Block::Quote, |out| out.write_str("hello"));
            });
        });
        out.write_str("after");

        assert_eq!(
            ["before", "", ">>> hello", "", "after",].join("\n"),
            out.to_string()
        );
    }

    #[test]
    fn indents_without_inner_writes() {
        let mut out = Output::new(vec![]);

        out.push_block(Block::Quote);
        out.pop_block();

        assert_eq!([">",].join("\n"), out.to_string());
    }

    #[test]
    fn surrounds_without_inner_writes() {
        let mut out = Output::new(vec![]);

        out.with_pre_block(|out| {
            out.write_str("vvv\n");
            out.write_str("^^^\n");
        });

        assert_eq!(["vvv", "^^^", ""].join("\n"), out.to_string());
    }

    fn write_test_block(out: &mut Output<Vec<u8>>, block: Block) {
        out.write_str("before");
        out.with_block(block, |out| out.write_str("hello\n\nworld"));
        out.write_str("after");
    }

    impl Display for Output<Vec<u8>> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            let bs = &self.stream;
            let s = String::from_utf8(bs.clone()).map_err(|_| Error)?;
            f.write_str(&s)?;
            Ok(())
        }
    }
}
