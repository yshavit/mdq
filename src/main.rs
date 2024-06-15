use std::io;
use std::io::{stdin, Read};
use std::string::ToString;

use crate::fmt_json::TextOnly;
use crate::fmt_md::MdOptions;
use crate::output::Stream;
use crate::tree::MdqNode;

mod fmt_json;
mod fmt_md;
mod output;
mod select;
mod str_utils;
mod tree;
mod tree_test_utils;

fn main() {
    let mut contents = String::new();
    stdin().read_to_string(&mut contents).expect("invalid input (not utf8)");
    let ast = markdown::to_mdast(&mut contents, &markdown::ParseOptions::gfm()).unwrap();
    let mdq: MdqNode = ast.try_into().unwrap();

    let mut out = output::Output::new(Stream(io::stdout()));

    let selector = select::Selector::Heading(select::Matcher::Substring {
        look_for: "Hello".to_string(),
        anchored_left: true,
        anchored_right: false,
    });

    let found = selector.find(&mdq);

    let jsons = fmt_json::nodes_to_json::<_, TextOnly>(&found);
    println!("{}", jsons);
    out.write_str("\n\n=======================================\n\n");
    fmt_md::write_md(&MdOptions::default(), &mut out, &found);
    out.write_str("\n");
}
