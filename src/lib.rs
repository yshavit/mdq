use crate::fmt_md::MdOptions;
use crate::fmt_md_inlines::MdInlinesWriterOptions;
use crate::output::Stream;
use crate::select::ParseError;
use crate::tree::{MdDoc, ReadOptions};
use crate::tree_ref::MdElemRef;
use crate::tree_ref_serde::SerdeDoc;
use clap::CommandFactory;
use cli::{Cli, OutputFormat};
use output::Output;
use select::MdqRefSelector;
use std::io;
use std::io::{stdin, Read, Write};

pub mod cli;
mod fmt_md;
mod fmt_md_inlines;
mod fmt_str;
mod footnote_transform;
mod link_transform;
mod matcher;
mod output;
mod parse_common;
mod parsing_iter;
mod select;
mod str_utils;
mod tree;
mod tree_ref;
mod tree_ref_serde;
mod tree_test_utils;
mod utils_for_test;
mod vec_utils;

pub fn run_in_memory(cli: &Cli, contents: &str) -> (bool, String) {
    let mut out = Vec::with_capacity(256); // just a guess

    let result = run(&cli, contents.to_string(), || &mut out);
    let out_str = String::from_utf8(out).map_err(|_| "UTF-8 decode error").unwrap();
    (result, out_str)
}

pub fn run_stdio(cli: &Cli) -> bool {
    if !cli.extra_validation() {
        return false;
    }
    let mut contents = String::new();
    stdin().read_to_string(&mut contents).expect("invalid input (not utf8)");
    run(&cli, contents, || io::stdout().lock())
}

fn run<W, F>(cli: &Cli, contents: String, get_out: F) -> bool
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
            eprintln!("error: {}", err);
            return false;
        }
    };

    let selectors_str = cli.selector_string();
    let selectors = match MdqRefSelector::parse(&selectors_str) {
        Ok(selectors) => selectors,
        Err(err) => {
            print_select_parse_error(&selectors_str, err);
            return false;
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
                let mut out = Output::new(Stream(&mut stdout));
                if let some @ Some(_) = cli.wrap_width {
                    out.text_width = some;
                }
                fmt_md::write_md(&md_options, &mut out, &ctx, pipeline_nodes.into_iter());
            }
            OutputFormat::Json => {
                serde_json::to_writer(
                    &mut stdout,
                    &SerdeDoc::new(&pipeline_nodes, &ctx, md_options.inline_options),
                )
                .unwrap();
            }
        }
    }

    found_any
}

fn print_select_parse_error(original_string: &str, err: ParseError) {
    eprintln!("Syntax error in select specifier:");
    for (line_num, line) in original_string.split('\n').enumerate() {
        if line_num == err.position.line {
            eprintln!("┃ {}", line);
            eprint!("┃ ");
            // Parsers typically throw errors after chars.next(), which advances the stream such that it's looking at
            // the char after the failure. So, subtract 1 so that we're pointing at the right one.
            for _ in 0..(err.position.column - 1) {
                eprint!(" ");
            }
            eprint!("↑ {}", err.reason);
            eprintln!();
        } else {
            eprintln!("⸾ {}", line)
        }
    }
}
