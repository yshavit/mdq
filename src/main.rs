mod output;
mod tree;
mod tree_to_json;
mod select;

use std::borrow::Borrow;
use std::io;
use std::io::{stdin, Read, Write};
use std::string::ToString;

use serde_json::Value;

use crate::tree::MdqNode;
use crate::tree_to_json::{InlineResolver, TextOnly};

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
    out.write_str("\n");
}



