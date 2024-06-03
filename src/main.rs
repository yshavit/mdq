use std::io::{Read, stdin};

use markdown::mdast::Node;
use serde_yaml::Value;

use crate::Resolver::{Current, Next, Until};

fn main() {
    let mut contents = String::new();
    stdin()
        .read_to_string(&mut contents)
        .expect("invalid input (not utf8)");
    let ast = markdown::to_mdast(&mut contents, &markdown::ParseOptions::gfm()).unwrap();
    let parsed = to_json(&ast);
    let y = serde_yaml::to_string(&parsed).expect("err");
    println!("{}", y);
    println!();
    let selector = Selector::Heading(Matcher::Substring { look_for: "Hello".to_string(), anchored_left: true, anchored_right: false });
    match selector.find(&ast) {
        None => { println!("no match") }
        Some(found) => {
            println!("found:");
            for node in found {
                println!();
                let j = to_json(node);
                let y = serde_yaml::to_string(&j).expect("err");
                println!("{}", y);
            }
        }
    }
}

type Selected = Option<bool>;

enum ListType {
    Ordered,
    Unordered,
}

enum Selector {
    BlockQuote(Matcher),
    List(Matcher, ListType, Selected),
    Image { text: Matcher, href: Matcher },
    Link { text: Matcher, href: Matcher },
    CodeBlock(Matcher),
    Heading(Matcher),
    // TODO I need an "any", or maybe just a "paragraph", or maybe both
}

enum Resolver { // TODO need better name
    Current,
    Next,
    Until(fn(&Node, &Node) -> bool)
}

impl Resolver {
    /// Tries to resolve a breadcrumb-style path of nodes to a single node. The breadcrumbs are
    /// implemented as a stack, with the "current" node as the last element, and the root as the
    /// first.
    fn resolve<'a>(&self, root_node: &'a Node, path: &Vec<usize>) -> Option<Vec<&'a Node>> {
        // first, find the parent
        let mut parent = root_node;
        for &idx in &path[..path.len()-1] {
            match parent.children() {
                None => {
                    return None;
                }
                Some(children) => {
                    match children.get(idx) {
                        None => {
                            return None;
                        }
                        Some(child) => {
                            parent = child;
                        }
                    }
                }
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
            Current => siblings.get(my_sibling_idx).map(|s| vec!(s)),
            Next => siblings.get(my_sibling_idx + 1).map(|s| vec!(s)),
            Until(predicate) => {
                let Some(current_node) = siblings.get(my_sibling_idx) else {
                    return None
                };
                let later_siblings = &siblings[my_sibling_idx+1..];
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

    fn find_0<'a>(&self, root: &'a Node, node: &'a Node, children_path: &mut Vec<usize>) -> Option<Vec<&'a Node>> {
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
                        return result
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
            (Selector::Heading(matcher), Node::Heading(h)) => {
                if matcher.matches(node.to_string()) {
                    let my_depth = h.depth;
                    Some(Until(Self::smaller_header))
                } else {
                    None
                }
            }
            (Selector::List(_matcher, _list_type, _selected), Node::List(_list)) => {
                None // TODO
            }
            (Selector::Image{ /*text, href*/ .. }, Node::Image(_img)) => {
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

    fn smaller_header(target_node: &Node, check_node: &Node ) -> bool {
        match (target_node, check_node) {
            (Node::Heading(target_heading), Node::Heading(check_heading)) => {
                target_heading.depth >= check_heading.depth
            }
            (_, _) => false
        }
    }
}

enum Matcher {
    Any,
    Substring { look_for: String, anchored_left: bool, anchored_right: bool },
    Regex(regex::Regex),
}

impl Matcher {
    fn matches(&self, text: String) -> bool {
        match self {
            Matcher::Any => true,
            Matcher::Substring { look_for, anchored_left, anchored_right } => {
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
            Matcher::Regex(pattern) => {
                pattern.is_match(&text)
            }
        }
    }
}

fn yaml_str(s: &str) -> Value {
    Value::String(s.to_string())
}

fn to_json(md_node: &Node) -> Value {
    let (node_type, node_details) = match md_node {
        Node::Root(_) => ("root", None),
        Node::BlockQuote(_) => ("block_quote", None),
        Node::FootnoteDefinition(val) => {
            let mut details = serde_yaml::Mapping::new();
            details.insert(yaml_str("id"), yaml_str(&val.identifier));
            if let Some(label) = &val.label {
                details.insert(yaml_str("label"), yaml_str(label));
            }
            ("footnote_definition", Some(Value::Mapping(details)))
        }
        Node::List(val) => {
            let list_type = if val.ordered { "ordered" } else { "unordered" };
            ("list", Some(yaml_str(list_type)))
        }
        Node::MdxJsxFlowElement(_)
        | Node::MdxjsEsm(_)
        | Node::MdxTextExpression(_)
        | Node::MdxJsxTextElement(_)
        | Node::MdxFlowExpression(_) => ("mdx", None),
        Node::Toml(_) => ("toml", None),
        Node::Yaml(_) => ("yaml", None),
        Node::Break(_) => ("break", None),
        Node::InlineCode(val) => ("inline_code", Some(yaml_str(&val.value))),
        Node::InlineMath(val) => ("math:inline", Some(yaml_str(&val.value))),
        Node::Delete(_) => ("delete", None),
        Node::Emphasis(_) => ("em", None),
        Node::FootnoteReference(val) => ("footnote_ref", Some(yaml_str(&val.identifier))),
        Node::Html(val) => ("html", Some(yaml_str(&val.value))),
        Node::Image(val) => ("img", Some(yaml_str(&val.url))),
        Node::ImageReference(val) => ("img_ref", Some(yaml_str(&val.identifier))),
        Node::Link(val) => {
            let mut obj = serde_yaml::Mapping::new();
            obj.insert(yaml_str("url"), yaml_str(&val.url));
            ("link", Some(obj.into()))
        }
        Node::LinkReference(val) => ("link_ref", Some(yaml_str(&val.identifier))),
        Node::Strong(_) => ("strong", None),
        Node::Text(val) => ("text", Some(yaml_str(&val.value))),
        Node::Code(val) => ("code_block", Some(yaml_str(&val.value))),
        Node::Math(val) => ("math_block", Some(yaml_str(&val.value))),
        Node::Heading(val) => ("heading", Some(Value::Number(val.depth.into()))),
        Node::Table(_) => ("table", None),
        Node::ThematicBreak(_) => ("break", None),
        Node::TableRow(_) => ("tr", None),
        Node::TableCell(_) => ("td", None),
        Node::ListItem(val) => {
            let desc = match &val.checked {
                Some(c) if *c => "checked",
                Some(_) => "unchecked",
                None => "li",
            };
            (desc, None)
        }
        Node::Definition(val) => ("dev", Some(yaml_str(&val.identifier))),
        Node::Paragraph(_) => ("paragraph", None),
    };

    let mut obj = serde_yaml::Mapping::new();
    obj.insert(
        Value::String(node_type.to_string()),
        node_details.unwrap_or_else(|| Value::String("_".to_string())),
    );
    if let Some(children) = md_node.children() {
        let children_jsons = children.iter().map(|c| to_json(c)).collect();
        obj.insert(Value::String("children".to_string()), children_jsons);
    }
    Value::Mapping(obj)
}
