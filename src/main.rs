use std::io::{stdin, Read};
use std::{env, io};

use crate::fmt_md::MdOptions;
use crate::output::Stream;
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

fn main() {
    let mut contents = String::new();
    stdin().read_to_string(&mut contents).expect("invalid input (not utf8)");
    let ast = markdown::to_mdast(&mut contents, &markdown::ParseOptions::gfm()).unwrap();
    let mdqs = MdElem::read(ast, &ReadOptions::default()).unwrap();
    let mut out = output::Output::new(Stream(io::stdout()));

    let selectors_str = env::args().nth(1).unwrap_or("".to_string());
    let selectors = MdqRefSelector::parse(&selectors_str).expect("failed to parse selector");

    let mut pipeline_nodes = MdElemRef::wrap_vec(&mdqs);
    for selector in selectors {
        let new_pipeline = selector.find_nodes(pipeline_nodes);
        pipeline_nodes = new_pipeline;
    }

    fmt_md::write_md(&MdOptions::default(), &mut out, pipeline_nodes.into_iter());
    out.write_str("\n");
}
