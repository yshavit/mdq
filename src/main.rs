use clap::Parser;
use std::io;
use std::io::{stdin, Read};
use std::process::ExitCode;

use crate::fmt_md::{MdOptions, ReferencePlacement};
use crate::fmt_md_inlines::MdInlinesWriterOptions;
use crate::link_transform::LinkTransform;
use crate::output::Stream;
use crate::select::ParseError;
use crate::tree::{MdElem, ReadOptions};
use crate::tree_ref::MdElemRef;
use crate::tree_ref_serde::SerdeDoc;
use select::MdqRefSelector;

mod fmt_md;
mod fmt_md_inlines;
mod fmt_str;
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

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Where to put link references.
    ///
    /// For links and images of style `[description][1]`, this flag controls where to put the `[1]: https://example.com`
    /// definition.
    #[arg(long, value_enum, default_value_t=ReferencePlacement::Section)]
    link_pos: ReferencePlacement,

    /// Where to put footnote references. Defaults to be same as --link-pos
    #[arg(long, value_enum)]
    footnote_pos: Option<ReferencePlacement>,

    #[arg(long, short, value_enum, default_value_t=LinkTransform::Reference)]
    link_canonicalization: LinkTransform,

    /// The selector string
    selectors: Option<String>,

    #[arg(long, short, default_value_t = false)]
    json: bool,
}

fn main() -> ExitCode {
    let cli = Cli::parse();

    let mut contents = String::new();
    stdin().read_to_string(&mut contents).expect("invalid input (not utf8)");
    let ast = markdown::to_mdast(&mut contents, &markdown::ParseOptions::gfm()).unwrap();
    let mdqs = MdElem::read(ast, &ReadOptions::default()).unwrap();

    let selectors_str = &cli.selectors.unwrap_or_default();
    let selectors = match MdqRefSelector::parse(selectors_str) {
        Ok(selectors) => selectors,
        Err(err) => {
            print_select_parse_error(selectors_str, err);
            return ExitCode::FAILURE;
        }
    };

    let mut pipeline_nodes = vec![MdElemRef::Doc(&mdqs)];
    for selector in selectors {
        let new_pipeline = selector.find_nodes(pipeline_nodes);
        pipeline_nodes = new_pipeline;
    }

    let md_options = MdOptions {
        link_reference_placement: cli.link_pos,
        footnote_reference_placement: cli.footnote_pos.unwrap_or(cli.link_pos),
        inline_options: MdInlinesWriterOptions {
            link_canonicalization: cli.link_canonicalization,
        },
    };

    if cli.json {
        serde_json::to_writer(io::stdout(), &SerdeDoc::new(&pipeline_nodes, md_options.inline_options)).unwrap();
    } else {
        let mut out = output::Output::new(Stream(io::stdout()));
        fmt_md::write_md(&md_options, &mut out, pipeline_nodes.into_iter());
        out.write_str("\n");
    }

    ExitCode::SUCCESS
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn verify_cli() {
        use clap::CommandFactory;
        Cli::command().debug_assert();
    }
}
