use crate::md_elem::{MdContext, MdElem};
use crate::output::tree_ref_serde::MdSerde;
use crate::output::{write_md, InlineElemOptions, MdWriterOptions};
use crate::util::output::{Output, SimpleWrite};
use serde::Serialize;
use std::{fmt, io};

/// A struct for writing [MdElem]s as Markdown (as per `--output markdown`).
pub struct MdWriter {
    options: MdWriterOptions,
}

impl MdWriter {
    /// Creates a new [MdWriter] with the given options.
    pub fn with_options(options: MdWriterOptions) -> Self {
        Self { options }
    }

    /// Writes the given nodes to the given writer.
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

/// Utility for translating an [`std::io::Write`] into a [`std::fmt::Write`].
///
/// [`std::io::Write`]: io::Write
/// [`std::fmt::Write`]: fmt::Write
pub fn io_to_fmt(writer: impl io::Write) -> impl fmt::Write {
    Adapter(writer)
}

/// Transform [`MdElem`]s into a [`Serialize`].
pub fn md_to_serialize<'a>(
    elems: &'a [MdElem],
    ctx: &'a MdContext,
    inline_options: InlineElemOptions,
) -> impl Serialize + 'a {
    MdSerde::new(&elems, &ctx, inline_options)
}

struct Adapter<W>(W);

impl<W: fmt::Write> SimpleWrite for Adapter<W> {
    fn write_char(&mut self, ch: char) -> io::Result<()> {
        self.0
            .write_char(ch)
            .map_err(|err| io::Error::new(io::ErrorKind::Other, format!("while writing char: {}", err)))
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
