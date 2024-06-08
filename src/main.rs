mod fmt_md;
mod fmt_str;
mod output;
mod select;
mod tree;
mod tree_to_json;

use std::io;
use std::io::{stdin, Read};
use std::string::ToString;

use crate::tree::MdqNode;
use crate::tree_to_json::TextOnly;

fn main() {
    let mut contents = String::new();
    stdin()
        .read_to_string(&mut contents)
        .expect("invalid input (not utf8)");
    let ast = markdown::to_mdast(&mut contents, &markdown::ParseOptions::gfm()).unwrap();
    let mdq: MdqNode = ast.try_into().unwrap();

    let mut out = output::Output::new(io::stdout());

    let selector = select::Selector::Heading(select::Matcher::Substring {
        look_for: "Hello".to_string(),
        anchored_left: true,
        anchored_right: false,
    });

    let found = selector.find(&mdq);

    let jsons = tree_to_json::nodes_to_json::<_, TextOnly>(&found);
    println!("{}", jsons);
    out.write_str("\n\n=======================================\n\n");
    fmt_md::write_md(&mut out, &found);
    out.write_str("\n");
}
