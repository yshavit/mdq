use std::io::{stdin, Read};
use std::process::ExitCode;
use std::{env, io};

use crate::fmt_md::MdOptions;
use crate::output::Stream;
use crate::select::ParseError;
use crate::tree::{MdElem, ReadOptions};
use crate::tree_ref::MdElemRef;
use select::MdqRefSelector;

mod fmt_md;
mod fmt_str;
mod matcher;
mod output;
mod parse_common;
mod parsing_iter;
mod select;
mod str_utils;
mod tree;
mod tree_ref;
mod tree_test_utils;
mod utils_for_test;

fn main() -> ExitCode {
    let mut contents = String::new();
    stdin().read_to_string(&mut contents).expect("invalid input (not utf8)");
    let ast = markdown::to_mdast(&mut contents, &markdown::ParseOptions::gfm()).unwrap();
    let mdqs = MdElem::read(ast, &ReadOptions::default()).unwrap();
    let mut out = output::Output::new(Stream(io::stdout()));

    let selectors_str = env::args().nth(1).unwrap_or("".to_string());
    let selectors = match MdqRefSelector::parse(&selectors_str) {
        Ok(selectors) => selectors,
        Err(err) => {
            print_select_parse_error(&selectors_str, err);
            return ExitCode::FAILURE;
        }
    };

    let mut pipeline_nodes = MdElemRef::wrap_vec(&mdqs);
    for selector in selectors {
        let new_pipeline = selector.find_nodes(pipeline_nodes);
        pipeline_nodes = new_pipeline;
    }

    fmt_md::write_md(&MdOptions::default(), &mut out, pipeline_nodes.into_iter());
    out.write_str("\n");

    ExitCode::SUCCESS
}

fn print_select_parse_error(original_string: &str, err: ParseError) {
    eprintln!("Syntax error in select specifier:");
    for (line_num, line) in original_string.split('\n').enumerate() {
        if line_num == err.position.line {
            eprintln!("┃ {}", line);
            eprint!("┃ ");
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
