use crate::md_elem::{MdContext, MdElem};
use crate::output::{write_md, MdWriterOptions};
use crate::util::output::{Output, SimpleWrite};
use std::{fmt, io};

/// A struct for writing [MdElem]s as Markdown (as per `--output markdown`).
#[derive(Copy, Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
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
            &mut Output::new(IoAdapter(out), self.options.text_width),
            ctx,
            nodes.into_iter(),
        )
    }
}

/// Adapter to convert between I/O types.
///
/// To use, wrap the source type in the `IoAdapter`, and use that adapter as the target type. For example, to convert
/// a [`std::io::Write`] into a [`std::fmt::Write`]:
///
/// ```
/// use mdq::output::IoAdapter;
///
/// fn example(input: impl std::io::Write) -> impl std::fmt::Write {
///     IoAdapter(input)
/// }
/// ```
///
/// [`std::io::Write`]: io::Write
/// [`std::fmt::Write`]: fmt::Write
#[derive(Copy, Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct IoAdapter<W>(pub W);

impl<W> From<W> for IoAdapter<W> {
    fn from(value: W) -> Self {
        Self(value)
    }
}

impl<W: fmt::Write> SimpleWrite for IoAdapter<W> {
    fn write_char(&mut self, ch: char) -> io::Result<()> {
        self.0
            .write_char(ch)
            .map_err(|err| io::Error::new(io::ErrorKind::Other, format!("while writing char: {}", err)))
    }

    fn flush(&mut self) -> io::Result<()> {
        Ok(())
    }
}

impl<W: io::Write> fmt::Write for IoAdapter<W> {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        self.0.write_all(s.as_bytes()).map_err(|_| fmt::Error)
    }
}
