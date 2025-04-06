use crate::md_elem::{InvalidMd, MdDoc, MdElemRef, MdSerde, ParseOptions};
use crate::output::fmt_md::MdOptions;
use crate::output::fmt_md_inlines::MdInlinesWriterOptions;
use crate::output::fmt_plain::PlainOutputOpts;
use crate::output::{fmt_md, fmt_plain};
use crate::query::ParseError;
use crate::run::cli::{Cli, OutputFormat};
use crate::select::{SelectorAdapter, SelectorChain};
use crate::util::output::Output;
use crate::util::output::{OutputOpts, Stream};
use crate::{md_elem, query};
use pest::error::ErrorVariant;
use pest::Span;
use std::borrow::Cow;
use std::fmt::{Display, Formatter};
use std::io::Write;
use std::ops::Deref;
use std::{env, io};

#[derive(Debug)]
pub enum Error {
    QueryParse { query_string: String, error: ParseError },
    MarkdownParse(InvalidMd),
    FileReadError(Input, io::Error),
}

#[derive(Debug)]
pub enum Input {
    Stdin,
    File(String),
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
            Input::File(file) => write!(f, "file {file:?}"),
        }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::QueryParse { query_string, error } => {
                let pest_err = match error {
                    ParseError::Pest(err) => Cow::Borrowed(err),
                    ParseError::Other(span, message) => {
                        let full_span = Span::new(query_string, span.start, span.end);
                        Cow::Owned(query::Error::new_from_span(
                            ErrorVariant::CustomError {
                                message: message.to_string(),
                            },
                            full_span.unwrap(),
                        ))
                    }
                };

                writeln!(f, "Syntax error in select specifier:")?;
                writeln!(f, "{pest_err}")
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

pub trait OsFacade {
    fn read_stdin(&self) -> io::Result<String>;
    fn read_file(&self, path: &str) -> io::Result<String>;
    fn get_stdout(&mut self) -> impl Write;
    fn write_error(&mut self, err: Error);

    fn read_all(&self, markdown_file_paths: &Vec<String>) -> Result<String, Error> {
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
                    .map_err(|err| Error::from_io_error(err, Input::File(path.to_string())))?;
                contents.push_str(&path_contents);
            }
            contents.push('\n');
        }
        Ok(contents)
    }
}

pub fn run(cli: &Cli, os: &mut impl OsFacade) -> bool {
    if !cli.extra_validation() {
        return false;
    }
    match run_or_error(cli, os) {
        Ok(ok) => ok,
        Err(err) => {
            os.write_error(err);
            false
        }
    }
}

fn run_or_error(cli: &Cli, os: &mut impl OsFacade) -> Result<bool, Error> {
    let contents_str = os.read_all(&cli.markdown_file_paths)?;
    let mut options = ParseOptions::gfm();
    options.allow_unknown_markdown = cli.allow_unknown_markdown;
    let MdDoc { roots, ctx } = match md_elem::parse(&contents_str, &options).map_err(|e| e.into()) {
        Ok(mdqs) => mdqs,
        Err(err) => {
            return Err(Error::MarkdownParse(err));
        }
    };

    let selectors_str = cli.selector_string();
    let selectors: SelectorChain = match selectors_str.deref().try_into() {
        Ok(selectors) => selectors,
        Err(error) => {
            return Err(Error::QueryParse {
                query_string: selectors_str.into_owned(),
                error,
            });
        }
    };

    let mut pipeline_nodes = vec![MdElemRef::Doc(&roots)];
    let selector_adapters = SelectorAdapter::from_chain(selectors);
    for selector in selector_adapters {
        let new_pipeline = selector.find_nodes(&ctx, pipeline_nodes);
        pipeline_nodes = new_pipeline;
    }

    let md_options = MdOptions {
        link_reference_placement: cli.link_pos,
        footnote_reference_placement: cli.footnote_pos.unwrap_or(cli.link_pos),
        inline_options: MdInlinesWriterOptions {
            link_format: cli.link_format,
            renumber_footnotes: cli.renumber_footnotes,
        },
        include_thematic_breaks: cli.should_add_breaks(),
    };

    let found_any = !pipeline_nodes.is_empty();

    if !cli.quiet {
        let mut stdout = os.get_stdout();
        match cli.output {
            OutputFormat::Markdown | OutputFormat::Md => {
                let output_opts = OutputOpts {
                    text_width: cli.wrap_width,
                };
                let mut out = Output::new(Stream(&mut stdout), output_opts);
                fmt_md::write_md(&md_options, &mut out, &ctx, pipeline_nodes.into_iter());
            }
            OutputFormat::Json => {
                serde_json::to_writer(
                    &mut stdout,
                    &MdSerde::new(&pipeline_nodes, &ctx, md_options.inline_options),
                )
                .unwrap();
            }
            OutputFormat::Plain => {
                let output_opts = PlainOutputOpts {
                    include_breaks: cli.should_add_breaks(),
                };
                fmt_plain::write_plain(&mut stdout, output_opts, pipeline_nodes.into_iter());
            }
        }
    }

    Ok(found_any)
}
