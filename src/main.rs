mod md_to_yaml_debug;
mod output;
mod normalized_ast;

use std::borrow::Borrow;
use std::io;
use std::io::{stdin, Read, Write};
use std::string::ToString;

use crate::output::Block;
use markdown::mdast::Node;

use crate::Resolver::{Current, Next, Until};

fn main() {
    let mut contents = String::new();
    stdin()
        .read_to_string(&mut contents)
        .expect("invalid input (not utf8)");
    let ast = markdown::to_mdast(&mut contents, &markdown::ParseOptions::gfm()).unwrap();

    let mut out = output::Output::new(io::stdout());

    let selector = Selector::Heading(Matcher::Substring {
        look_for: "Hello".to_string(),
        anchored_left: true,
        anchored_right: false,
    });

    match selector.find(&ast) {
        None => out.write_str("(no match)"),
        Some(found) => {
            write_md(&found, &mut out);
        }
    }
    out.write_str("\n");
}

fn write_md<'a, N, W>(nodes: &[N], out: &mut output::Output<W>)
where
    N: Borrow<Node>,
    W: Write,
{
    for node in nodes {
        match node.borrow() {
            Node::BlockQuote(node) => out.with_block(Block::Quote, |out| {
                write_md(&node.children, out);
            }),
            Node::Break(_) => {
                out.write_str("\n");
            }
            Node::Code(node) => {
                out.with_pre_block(|out| {
                    out.write_str("```");
                    if let Some(lang) = &node.lang {
                        out.write_str(&lang);
                        if let Some(meta) = &node.meta {
                            // as far as I can tell, meta is only ever Some if lang is Some
                            out.write_str(" ");
                            out.write_str(&meta);
                        }
                    }
                    out.write_str("\n");
                    out.write_str(&node.value);
                    out.write_str("\n```");
                })
            }
            Node::Definition(_) => {
                // TODO
            }
            Node::Delete(node) => {
                out.write_str("~~");
                write_md(&node.children, out);
                out.write_str("~~");
            }
            Node::Emphasis(node) => {
                out.write_str("*");
                write_md(&node.children, out);
                out.write_str("*");
            }
            Node::FootnoteDefinition(_) => {}
            Node::FootnoteReference(_) => {}
            Node::Heading(node) => out.with_block(Block::Plain, |out| {
                out.write_str(&"#".repeat(node.depth as usize));
                out.write_str(" ");
                write_md(&node.children, out);
            }),
            Node::Html(node) => {
                out.write_str(&node.value);
            }
            Node::Image(node) => {
                out.write_str("![");
                out.write_str(&node.alt);
                out.write_str("](");
                out.write_str(&node.url);
                out.write_str(")");
            }
            Node::ImageReference(_) => {
                // TODO
            }
            Node::InlineCode(node) => {
                out.write_str("`");
                out.write_str(&node.value);
                out.write_str("`");
            }
            Node::InlineMath(node) => {
                out.write_str("$");
                out.write_str(&node.value);
                out.write_str("$");
            }
            Node::Link(node) => {
                out.write_str("[");
                write_md(&node.children, out);
                out.write_str("](");
                out.write_str(&node.url);
                out.write_str(")");
            }
            Node::LinkReference(_) => {
                // TODO
            }
            Node::List(node) => {
                out.write_str("\n");

                for (idx, li) in node.children.iter().enumerate() {
                    let Some(li_nodes) = li.children() else {
                        continue;
                    };
                    let counter = if node.ordered {
                        format!("{}.", idx + 1) // TODO align when we have different number of digits
                    } else if let Node::ListItem(li_details) = li {
                        (match li_details.checked {
                            None => "- ",
                            Some(true) => "- [x]",
                            Some(false) => "- [ ]",
                        })
                        .to_string() // TODO  remove need for to_string() here and in the else below.
                    } else {
                        "- ".to_string()
                    };
                    out.write_str(&*counter); // what the heck is even this
                                              // TODO set up indent
                    write_md(&li_nodes, out);
                }
                out.write_str("\n");
            }
            Node::ListItem(_) => {
                panic!("internal error") // should already have been handled
            }
            Node::Math(node) => {
                out.with_pre_block(|out| {
                    out.write_str("$$\n");
                    out.write_str(&node.value);
                    out.write_str("\n$$");
                })
            }
            Node::MdxFlowExpression(node) => {
                out.write_str("{");
                out.write_str(&node.value);
                out.write_str("}");
            }
            Node::MdxJsxFlowElement(_) => {}
            Node::MdxJsxTextElement(_) => {}
            Node::MdxTextExpression(_) => {}
            Node::MdxjsEsm(_) => {}
            Node::Paragraph(node) => out.with_block(Block::Plain, |out| {
                write_md(&node.children, out);
            }),
            Node::Root(node) => {
                write_md(&node.children, out);
            }
            Node::Strong(node) => {
                out.write_str("**");
                write_md(&node.children, out);
                out.write_str("**");

            }
            Node::Table(_) => {}
            // Node::TableCell(_) => {}
            // Node::TableRow(_) => {}
            Node::Text(node) => {
                out.write_str(&node.value);
            }
            // Node::ThematicBreak(_) => {}
            // Node::Toml(_) => {}
            // Node::Yaml(_) => {}
            _ => {} // TODO remove this
        };
    }
}

type Selected = Option<bool>;

#[allow(dead_code)]
enum ListType {
    Ordered,
    Unordered,
}

#[allow(dead_code)]
enum Selector {
    BlockQuote(Matcher),
    List(Matcher, ListType, Selected),
    Image { text: Matcher, href: Matcher },
    Link { text: Matcher, href: Matcher },
    CodeBlock(Matcher),
    Heading(Matcher),
    // TODO I need an "any", or maybe just a "paragraph", or maybe both
}

#[allow(dead_code)]
enum Resolver {
    // TODO need better name
    Current,
    Next,
    Until(fn(&Node, &Node) -> bool),
}

impl Resolver {
    /// Tries to resolve a breadcrumb-style path of nodes to a single node. The breadcrumbs are
    /// implemented as a stack, with the "current" node as the last element, and the root as the
    /// first.
    fn resolve<'a>(&self, root_node: &'a Node, path: &Vec<usize>) -> Option<Vec<&'a Node>> {
        // first, find the parent
        let mut parent = root_node;
        for &idx in &path[..path.len() - 1] {
            match parent.children() {
                None => {
                    return None;
                }
                Some(children) => match children.get(idx) {
                    None => {
                        return None;
                    }
                    Some(child) => {
                        parent = child;
                    }
                },
            }
        }
        // Now, resolve based on that parent
        let Some(&my_sibling_idx) = path.last() else {
            return None;
        };
        let Some(siblings) = parent.children() else {
            return None;
        };
        match self {
            Current => siblings.get(my_sibling_idx).map(|s| vec![s]),
            Next => siblings.get(my_sibling_idx + 1).map(|s| vec![s]),
            Until(predicate) => {
                let Some(current_node) = siblings.get(my_sibling_idx) else {
                    return None;
                };
                let later_siblings = &siblings[my_sibling_idx + 1..];
                let mut result = Vec::new();
                for sibling in later_siblings {
                    if predicate(current_node, sibling) {
                        break;
                    }
                    result.push(sibling);
                }
                return Some(result);
            }
        }
    }
}

impl Selector {
    pub fn find<'a>(&'a self, node: &'a Node) -> Option<Vec<&Node>> {
        let mut children = Vec::new();
        return self.find_0(node, node, &mut children);
    }

    fn find_0<'a>(
        &self,
        root: &'a Node,
        node: &'a Node,
        children_path: &mut Vec<usize>,
    ) -> Option<Vec<&'a Node>> {
        // TODO this needs to be able to return a Vec<Node>, not just a single Node.
        // That's because a header's result are basically all of its siblings until the next header.
        // Alternatively, I could just restructure it as a pre-pass.
        if let Some(resolver) = self.find_at_node_exactly(node) {
            return resolver.resolve(root, children_path);
        }
        match node.children() {
            None => None,
            Some(children) => {
                for (idx, child) in children.iter().enumerate() {
                    children_path.push(idx);
                    if let result @ Some(_) = self.find_0(root, child, children_path) {
                        return result;
                    }
                    children_path.pop();
                }
                None
            }
        }
    }

    fn find_at_node_exactly<'a>(&'a self, node: &'a Node) -> Option<Resolver> {
        match (self, node) {
            (Selector::BlockQuote(matcher), Node::BlockQuote(_)) => {
                if matcher.matches(node.to_string()) { Some(Current) } else { None }
            }
            (Selector::Heading(matcher), Node::Heading(_)) => {
                if matcher.matches(node.to_string()) {
                    Some(Until(Self::smaller_header))
                } else {
                    None
                }
            }
            (Selector::List(_matcher, _list_type, _selected), Node::List(_list)) => {
                None // TODO
            }
            (Selector::Image { /*text, href*/ .. }, Node::Image(_img)) => {
                // TODO see note on "Link" below
                None // TODO
            }
            (Selector::Link { /*text, href*/ .. }, Node::Link(_link)) => {
                // TODO I should normalize these such that link references are just inline links.
                // In other words, this:
                //
                //  [foo](https://example.com)
                //
                // should be equivalent to this:
                //
                //  [foo][1]
                //  [1]: https://example.com
                None // TODO
            }
            (Selector::CodeBlock(_matcher), Node::Code(_code)) => {
                None // TODO
            }

            (_, _) => None,
        }
    }

    fn smaller_header(target_node: &Node, check_node: &Node) -> bool {
        match (target_node, check_node) {
            (Node::Heading(target_heading), Node::Heading(check_heading)) => {
                target_heading.depth >= check_heading.depth
            }
            (_, _) => false,
        }
    }
}

#[allow(dead_code)]
enum Matcher {
    Any,
    Substring {
        look_for: String,
        anchored_left: bool,
        anchored_right: bool,
    },
    Regex(regex::Regex),
}

impl Matcher {
    fn matches(&self, text: String) -> bool {
        match self {
            Matcher::Any => true,
            Matcher::Substring {
                look_for,
                anchored_left,
                anchored_right,
            } => {
                // TODO we can do this more efficiently, but this is good for now.
                // In the future, we could take an approach of comparing each char of look_for to
                // the next char in the &strs list.
                match (anchored_left, anchored_right) {
                    (false, false) => text.contains(look_for),
                    (true, false) => text.starts_with(look_for),
                    (false, true) => text.ends_with(look_for),
                    (true, true) => text.eq(look_for),
                }
            }
            Matcher::Regex(pattern) => pattern.is_match(&text),
        }
    }
}
