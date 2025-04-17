use crate::md_elem::{InvalidMd, MdDoc, MdElem, ParseOptions};
use crate::output::MdWriter;
use crate::query::ParseError;
use crate::run::cli::OutputFormat;
use crate::run::RunOptions;
use crate::select::{Selector, SelectorAdapter};
use crate::{md_elem, output, query};
use pest::error::ErrorVariant;
use pest::Span;
use std::fmt::{Display, Formatter};
use std::io::Write;
use std::{env, io};

/// The run's overall possible error.
#[derive(Debug)]
pub enum Error {
    /// User provided an invalid selector string.
    ///
    /// This comes from [`Selector`'s `TryFrom::<&str>`][Selector#impl-TryFrom<%26str>-for-Selector].
    QueryParse(QueryParseError),

    /// The Markdown file failed to parse.
    ///
    /// This comes from [`md_elem::parse`]..
    MarkdownParse(InvalidMd),

    /// Couldn't read an input file.
    FileReadError(Input, io::Error),
}

/// Returned when the selector string is not valid.
#[derive(Debug)]
pub struct QueryParseError {
    query_string: String,
    error: ParseError,
}

impl Display for QueryParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self.error {
            ParseError::Pest(err) => {
                write!(f, "{err}")
            }
            ParseError::Other(span, message) => {
                let Some(full_span) = Span::new(&self.query_string, span.start, span.end) else {
                    // not expected to happen, but just in case!
                    return write!(
                        f,
                        "parse error at byte {} of {:?}: {}",
                        span.start, self.query_string, message
                    );
                };
                let pest_err = query::Error::new_from_span(
                    ErrorVariant::CustomError {
                        message: message.to_string(),
                    },
                    full_span,
                );
                write!(f, "{pest_err}")
            }
        }
    }
}

/// Stdin or an input file by path.
#[derive(Debug)]
pub enum Input {
    Stdin,
    FilePath(String),
}

impl Error {
    pub fn from_io_error(error: io::Error, file: Input) -> Self {
        Error::FileReadError(file, error)
    }
}

impl Display for Input {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Input::Stdin => f.write_str("stdin"),
            Input::FilePath(file) => write!(f, "file {file:?}"),
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::QueryParse(err) => {
                writeln!(f, "Syntax error in select specifier:")?;
                writeln!(f, "{err}")
            }
            Error::MarkdownParse(err) => {
                writeln!(f, "Markdown parse error:")?;
                writeln!(f, "{err}")
            }
            Error::FileReadError(file, err) => {
                if env::var("MDQ_PORTABLE_ERRORS").unwrap_or(String::new()).is_empty() {
                    writeln!(f, "{err} while reading {file}")
                } else {
                    writeln!(f, "{} while reading {file}", err.kind())
                }
            }
        }
    }
}

/// A simple facade for handling I/O.
///
/// This trait lets you do "I/O-y stuff" like mocking out stdin or reading files. The [`run`] method uses it.
pub trait OsFacade {
    /// Read stdin (or your mock of it) to a `String`.
    fn read_stdin(&self) -> io::Result<String>;

    /// Read a file path (or your mock of one) to a `String`.
    fn read_file(&self, path: &str) -> io::Result<String>;

    /// Get a writer for stdout (or your mock of it).
    fn get_stdout(&mut self) -> impl Write;

    /// Handle an error.
    fn write_error(&mut self, err: Error);

    /// Read a slice of file paths into a single, concatenated `String`.
    ///
    /// The default implementation (which you should feel free to use) treats the file path `"-"` as stdin. The first
    /// `"-"` reads all of stdin (via [`Self::read_stdin`]), and subsequent `"-"`s get silently ignored.
    fn read_all(&self, markdown_file_paths: &[String]) -> Result<String, Error> {
        if markdown_file_paths.is_empty() {
            return self.read_stdin().map_err(|err| Error::from_io_error(err, Input::Stdin));
        }
        let mut contents = String::new();
        let mut have_read_stdin = false;
        for path in markdown_file_paths {
            if path == "-" {
                if !have_read_stdin {
                    contents.push_str(
                        &self
                            .read_stdin()
                            .map_err(|err| Error::from_io_error(err, Input::Stdin))?,
                    );
                    have_read_stdin = true
                }
            } else {
                let path_contents = self
                    .read_file(path)
                    .map_err(|err| Error::from_io_error(err, Input::FilePath(path.to_string())))?;
                contents.push_str(&path_contents);
            }
            contents.push('\n');
        }
        Ok(contents)
    }
}

/// Runs mdq end to end.
///
/// This uses the provided [RunOptions] and [OsFacade] to read files into [`MdElem`], filters them via the selector
/// string in [`RunOptions::selectors`], and then writes them to the given [`OsFacade`] in the format specified by
/// [`RunOptions::output`].
pub fn run(cli: &RunOptions, os: &mut impl OsFacade) -> bool {
    match run_or_error(cli, os) {
        Ok(ok) => ok,
        Err(err) => {
            os.write_error(err);
            false
        }
    }
}

fn run_or_error(cli: &RunOptions, os: &mut impl OsFacade) -> Result<bool, Error> {
    let contents_str = os.read_all(&cli.markdown_file_paths)?;
    let mut options = ParseOptions::gfm();
    options.allow_unknown_markdown = cli.allow_unknown_markdown;
    let MdDoc { roots, ctx } = match md_elem::parse(&contents_str, &options).map_err(|e| e.into()) {
        Ok(mdqs) => mdqs,
        Err(err) => {
            return Err(Error::MarkdownParse(err));
        }
    };

    let selectors_str = &cli.selectors;
    let selectors: Selector = match selectors_str.try_into() {
        Ok(selectors) => selectors,
        Err(error) => {
            return Err(Error::QueryParse(QueryParseError {
                query_string: selectors_str.to_string(),
                error,
            }));
        }
    };

    let selector_adapter = SelectorAdapter::from(selectors);
    let pipeline_nodes = selector_adapter.find_nodes(&ctx, vec![MdElem::Doc(roots)]);

    // TODO: turn this into an impl From
    let md_options = output::MdWriterOptions {
        link_reference_placement: cli.link_pos,
        footnote_reference_placement: cli.footnote_pos.unwrap_or(cli.link_pos),
        inline_options: output::InlineElemOptions {
            link_format: cli.link_format,
            renumber_footnotes: cli.renumber_footnotes,
        },
        include_thematic_breaks: cli.should_add_breaks(),
        text_width: cli.wrap_width,
    };

    let found_any = !pipeline_nodes.is_empty();

    if !cli.quiet {
        let mut stdout = os.get_stdout();
        match cli.output {
            OutputFormat::Markdown | OutputFormat::Md => {
                MdWriter::with_options(md_options).write(&ctx, &pipeline_nodes, &mut output::io_to_fmt(&mut stdout));
            }
            OutputFormat::Json => {
                serde_json::to_writer(
                    &mut stdout,
                    &output::md_to_serialize(&pipeline_nodes, &ctx, md_options.inline_options),
                )
                .unwrap();
            }
            OutputFormat::Plain => {
                output::PlainWriter::with_options(output::PlainWriterOptions {
                    include_breaks: cli.should_add_breaks(),
                })
                .write(&pipeline_nodes, &mut stdout);
            }
        }
    }

    Ok(found_any)
}
