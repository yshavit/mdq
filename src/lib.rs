use crate::fmt_md::MdOptions;
use crate::fmt_md_inlines::MdInlinesWriterOptions;
use crate::output::{OutputOpts, Stream};
use crate::select::{ParseError, SelectorAdapter};
use crate::tree::{InvalidMd, MdDoc, ReadOptions};
use crate::tree_ref::MdElemRef;
use crate::tree_ref_serde::SerdeDoc;
use cli::{Cli, OutputFormat};
use output::Output;
use pest::error::ErrorVariant;
use pest::Span;
use std::borrow::Cow;
use std::fmt::{Display, Formatter};
use std::io;
use std::io::{stdin, Read, Write};

pub mod cli;
mod fmt_md;
mod fmt_md_inlines;
mod fmt_plain;
mod fmt_plain_writer;
mod fmt_str;
mod footnote_transform;
mod link_transform;
mod matcher;
mod output;
mod query;
mod select;
mod str_utils;
mod tree;
mod tree_ref;
mod tree_ref_serde;
mod tree_test_utils;
mod utils_for_test;
mod vec_utils;
mod words_buffer;

pub enum Error {
    QueryParse { query_string: String, error: ParseError },
    MarkdownParse(InvalidMd),
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
        }
    }
}

pub fn run_in_memory(cli: &Cli, contents: &str) -> Result<(bool, String), Error> {
    let mut out = Vec::with_capacity(256); // just a guess

    let result = run(&cli, contents.to_string(), || &mut out)?;
    let out_str = String::from_utf8(out).map_err(|_| "UTF-8 decode error").unwrap();
    Ok((result, out_str))
}

pub fn run_stdio(cli: &Cli) -> bool {
    if !cli.extra_validation() {
        return false;
    }
    let mut contents = String::new();
    stdin().read_to_string(&mut contents).expect("invalid input (not utf8)");
    run(&cli, contents, || io::stdout().lock()).unwrap_or_else(|err| {
        eprintln!("{err}");
        false
    })
}

fn run<W, F>(cli: &Cli, contents: String, get_out: F) -> Result<bool, Error>
where
    F: FnOnce() -> W,
    W: Write,
{
    let ast = markdown::to_mdast(&contents, &markdown::ParseOptions::gfm()).unwrap();
    let read_options = ReadOptions {
        validate_no_conflicting_links: false,
        allow_unknown_markdown: cli.allow_unknown_markdown,
    };
    let MdDoc { roots, ctx } = match MdDoc::read(ast, &read_options) {
        Ok(mdqs) => mdqs,
        Err(err) => {
            return Err(Error::MarkdownParse(err));
        }
    };

    let selectors_str = cli.selector_string();
    let selectors = match SelectorAdapter::parse(&selectors_str) {
        Ok(selectors) => selectors,
        Err(error) => {
            return Err(Error::QueryParse {
                query_string: selectors_str.into_owned(),
                error,
            });
        }
    };

    let mut pipeline_nodes = vec![MdElemRef::Doc(&roots)];
    for selector in selectors {
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
    };

    let found_any = !pipeline_nodes.is_empty();

    if !cli.quiet {
        let mut stdout = get_out();
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
                    &SerdeDoc::new(&pipeline_nodes, &ctx, md_options.inline_options),
                )
                .unwrap();
            }
            OutputFormat::Plain => {
                fmt_plain::write_plain(&mut stdout, pipeline_nodes.into_iter());
            }
        }
    }

    Ok(found_any)
}
