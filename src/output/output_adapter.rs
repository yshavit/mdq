use crate::md_elem::{MdContext, MdElem};
use crate::output::{write_md, MdOptions};
use crate::util::output::{Output, OutputOptions, SimpleWrite};
use std::{fmt, io};

pub struct MdWriter {
    md_options: MdOptions,
    output_options: OutputOptions,
}

impl MdWriter {
    pub fn with_options(md: MdOptions, output: OutputOptions) -> Self {
        Self {
            md_options: md,
            output_options: output,
        }
    }

    pub fn write<'md, I, W>(&self, ctx: &'md MdContext, nodes: I, out: &mut W)
    where
        I: IntoIterator<Item = &'md MdElem>,
        W: fmt::Write,
    {
        write_md(
            self.md_options,
            &mut Output::new(Adapter(out), self.output_options),
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
