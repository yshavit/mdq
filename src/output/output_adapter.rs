use crate::md_elem::{MdContext, MdElem};
use crate::output::{write_md, MdWriterOptions};
use crate::util::output::{Output, SimpleWrite};
use std::{fmt, io};

pub struct MdWriter {
    options: MdWriterOptions,
}

impl MdWriter {
    pub fn with_options(options: MdWriterOptions) -> Self {
        Self { options }
    }

    pub fn write<'md, I, W>(&self, ctx: &'md MdContext, nodes: I, out: &mut W)
    where
        I: IntoIterator<Item = &'md MdElem>,
        W: fmt::Write,
    {
        write_md(
            self.options,
            &mut Output::new(Adapter(out), self.options.text_width),
            ctx,
            nodes.into_iter(),
        )
    }
}

pub fn io_to_fmt(writer: impl io::Write) -> impl fmt::Write {
    Adapter(writer)
}

struct Adapter<W>(W);

impl<W: fmt::Write> SimpleWrite for Adapter<W> {
    fn write_char(&mut self, ch: char) -> io::Result<()> {
        self.0
            .write_char(ch)
            .map_err(|_| io::Error::new(io::ErrorKind::Other, "while writing char"))
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

impl<W: io::Write> fmt::Write for Adapter<W> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.0.write_all(s.as_bytes()).map_err(|_| fmt::Error)
    }
}
