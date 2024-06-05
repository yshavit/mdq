use std::io::Write;

pub struct Output<W: Write> {
    stream: W,
    blocks: Vec<BlockClose>,
    indents: Vec<String>,
    pending_indents: Vec<String>,
    pending_newlines: usize,
    writing_state: WritingState,
}

impl<W: Write> Output<W> {
    pub fn new(to: W) -> Self {
        Self {
            stream: to,
            blocks: Vec::default(),
            indents: Vec::default(),
            pending_indents: Vec::default(),
            pending_newlines: 0,
            writing_state: WritingState::HaveNotWrittenAnything,
        }
    }

    pub fn with_block<F>(&mut self, block: Block, action: F)
        where F: FnOnce(&mut Self)
    {
        self.push_block(block);
        action(self);
        self.pop_block();
    }

    pub fn push_block(&mut self, block: Block) {
        self.ensure_newlines(2);
        let block_close = match block {
            Block::Plain => BlockClose::NoAction,
            Block::Indent(text) => {
                self.pending_indents.push(text);
                BlockClose::RemoveIndent
            }
            Block::Surround(start, end) => {
                self.ensure_newlines(1);
                self.write_str(&start);
                self.ensure_newlines(1);
                BlockClose::EndSurround(end)
            }
        };
        self.blocks.push(block_close);
    }

    pub fn pop_block(&mut self) {
        if !self.pending_indents.is_empty() {
            self.write_str(""); // flush the indents
        }
        match self.blocks.pop() {
            None => {}
            Some(block_close) => {
                match block_close {
                    BlockClose::NoAction => {}
                    BlockClose::RemoveIndent => {
                        self.indents.pop();
                    }
                    BlockClose::EndSurround(end_text) => {
                        self.ensure_newlines(1);
                        self.write_str(&end_text);
                    }
                }
            }
        }
        self.ensure_newlines(2);
    }

    pub fn write_str(&mut self, text: &str) {
        let lines = regex::Regex::new("\n+").unwrap();
        let mut lines = lines.split(text).peekable();
        while let Some(line) = lines.next() {
            self.write_line(line);
            if let Some(_) = lines.peek() {
                self.ensure_newlines(1);
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
        // Get our (previous) indent, not counting any pending indents
        let mut indent = self.indents.join("");

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
                    for _ in 1..self.pending_newlines { // from 1 because we already wrote the first
                        self.write_raw(&indent);
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

        for new_indent in self.pending_indents.drain(..) {
            indent.push_str(&new_indent);
            self.indents.push(new_indent);
        }

        if should_print_indent && !indent.is_empty() {
            self.write_raw(&indent);
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
            WritingState::HaveNotWrittenAnything => { self.writing_state = WritingState::WroteSome }
            WritingState::WroteSome => {}
            WritingState::Error => { return; }
        }
        if let Err(e) = self.stream.write(text.as_bytes()) {
            eprintln!("error while writing output: {}", e);
            self.writing_state = WritingState::Error;
        }
    }
}

pub enum Block {
    Plain,
    Indent(String),
    Surround(String, String),
}

enum WritingState {
    HaveNotWrittenAnything,
    WroteSome,
    Error,
}

enum BlockClose {
    NoAction,
    RemoveIndent,
    EndSurround(String),
}

mod tests {
    use std::fmt::{Display, Error};
    use super::*;

    #[test]
    fn simple_string() {
        let mut out = Output::new(vec!());

        out.write_str("hello");

        assert_eq!("hello", out.to_string());
    }

    #[test]
    fn pop_block_when_there_is_none() {
        let mut out = Output::new(vec!());

        out.pop_block(); // no-op
        out.write_str("hello world");

        assert_eq!("hello world", out.to_string());
    }

    #[test]
    fn plain_block() {
        let mut out = Output::new(vec!());

        write_test_block(&mut out, Block::Plain);

        assert_eq!(
            [
                "before",
                "",
                "hello world",
                "",
                "after",
            ].join("\n"),
            out.to_string());
    }

    #[test]
    fn indent_block() {
        let mut out = Output::new(vec!());

        write_test_block(&mut out, Block::Indent(">".to_string()));

        assert_eq!(
            [
                "before",
                "",
                "> hello world",
                "",
                "after",
            ].join("\n"),
            out.to_string());
    }

    #[test]
    fn surround_block() {
        let mut out = Output::new(vec!());

        write_test_block(&mut out, Block::Surround("vvv".to_string(), "^^^".to_string()));

        assert_eq!(
            [
                "before",
                "",
                "vvv",
                "hello world",
                "^^^",
                "",
                "after",
            ].join("\n"),
            out.to_string());
    }

    #[test]
    fn nested_blocks() {
        let mut out = Output::new(vec!());

        out.with_block(Block::Indent(">".to_string()), |out| {
            out.write_str("hello ");
            out.write_str("world");

            out.with_block(Block::Indent(">".to_string()), |out| {
                out.write_str("second level");
                // no ensuring newline
                out.with_block(Block::Surround("```".to_string(), "```".to_string()), |out| {
                    out.write_str("some code")
                })
            })
        });
        out.write_str("after");

        assert_eq!(
            [
                "> hello world",
                ">",
                ">> second level",
                ">>",
                ">> ```",
                ">> some code",
                ">> ```",
                "",
                "after",
            ].join("\n"),
            out.to_string());
    }

    #[test]
    fn nested_blocks_jump_a_few() {
        let mut out = Output::new(vec!());

        out.write_str("before");
        out.with_block(Block::Indent(">".to_string()), |out| {
            out.with_block(Block::Indent("!".to_string()), |out| {
                out.with_block(Block::Indent("%".to_string()), |out| {
                    out.write_str("hello")
                });
            });
        });
        out.write_str("after");

        assert_eq!(
            [
                "before",
                "",
                ">!% hello",
                "",
                "after",
            ].join("\n"),
            out.to_string());
    }

    #[test]
    fn indents_without_inner_writes() {
        let mut out = Output::new(vec!());

        out.push_block(Block::Indent(">".to_string()));
        out.pop_block();

        assert_eq!(
            [
                ">",
            ].join("\n"),
            out.to_string());
    }

    #[test]
    fn surrounds_without_inner_writes() {
        let mut out = Output::new(vec!());

        out.push_block(Block::Surround("vvv".to_string(), "^^^".to_string()));
        out.pop_block();

        assert_eq!(
            [
                "vvv",
                "^^^",
            ].join("\n"),
            out.to_string());
    }

    fn write_test_block(out: &mut Output<Vec<u8>>, block: Block) {
        out.write_str("before");
        out.with_block(block, |out| out.write_str("hello world"));
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