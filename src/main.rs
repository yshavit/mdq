mod output;

use std::borrow::Cow;
use std::io;
use std::io::{Read, stdin, Write};
use std::ops::Deref;

use markdown::mdast::Node;
use markdown::mdast::Node::ListItem;
use serde_yaml::Value;
use crate::MdSpec::{Skip, Unknown};

use crate::Resolver::{Current, Next, Until};

fn main() {
    let mut contents = String::new();
    stdin()
        .read_to_string(&mut contents)
        .expect("invalid input (not utf8)");
    let ast = markdown::to_mdast(&mut contents, &markdown::ParseOptions::gfm()).unwrap();
    let parsed = to_json(&ast);
    let y = serde_yaml::to_string(&parsed).expect("err");

    let mut out = output::Output::new(io::stdout());

    eprintln!("{}", y);
    eprintln!();
    let selector = Selector::Heading(Matcher::Substring { look_for: "Hello".to_string(), anchored_left: true, anchored_right: false });
    match selector.find(&ast) {
        None => { println!("no match") }
        Some(found) => {
            out.push_block(output::Block::Plain);
            out.write_str("Found the following:");
            out.pop_block();
            let mut out = StrWriter::new(io::stdout());
            write_md(&found, &mut out);
        }
    }
}

struct StrWriter<W>
    where W: Write
{
    indents: Vec<String>,
    out: W,
    at_new_line: bool,
}

impl<W> StrWriter<W>
    where W: Write {
    pub fn new(out: W) -> Self {
        Self {
            out,
            indents: Vec::new(),
            at_new_line: true,
        }
    }

    pub fn write<'a, S>(&mut self, s: S)
    where S: Into<&'a str>{
        let write_str = s.into();
        let mut iter = write_str.split("\n").peekable();
        while let Some(line) = iter.next() {
            if !line.is_empty() {
                self.write_indents();
                self.write_raw(line)
            }
            if let Some(_) = iter.peek() {
                self.write_raw("\n");
            }
        }
    }

    pub fn writeln(&mut self) {
        self.write_raw("\n");
    }

    pub fn push_indent<S>(&mut self, indent: S)
        where S: ToString
    {
        self.indents.push(indent.to_string());
    }

    pub fn pop_indent(&mut self) {
        self.indents.pop();
    }

    fn write_indents(&mut self) {
        if self.at_new_line {
            for s in &self.indents {
                // TODO de-dupe with write_raw; borrow mutability errors
                if let Err(err) = self.out.write(s.as_bytes()) {
                    eprintln!("error: {}", err);
                }
                self.at_new_line = s.ends_with("\n");
            }
        }
    }

    fn write_raw(&mut self, s: &str) {
        if let Err(err) = self.out.write(s.as_bytes()) {
            eprintln!("error: {}", err);
        }
        self.at_new_line = s.ends_with("\n");
    }
}

struct MdWrite<'a> {
    indent: Cow<'a, str>,
    literal: Cow<'a, str>,
    before_children: Cow<'a, str>,
    before_each: fn(child_idx: usize) -> String,
    after_children: Cow<'a, str>,
}

enum MdSpec<'a> {
    Write(MdWrite<'a>),
    Skip,
    Unknown(&'a str),
}

impl<'a> MdSpec<'a> {
    fn children<F>(f: F) -> Self
        where F: FnOnce(&mut MdWrite)
    {
        let mut write = MdWrite {
            indent: Cow::default(),
            literal: Cow::default(),
            before_children: Cow::default(),
            before_each: (|x| "".to_string()),
            after_children: Cow::default(),
        };
        f(&mut write);
        return Self::Write(write);
    }
}

impl<'a> MdWrite<'a> {
    pub fn write<S>(&mut self, value: S) -> &mut Self
        where S: Into<Cow<'a, str>>
    {
        self.literal = Cow::from(value.into());
        self
    }

    pub fn indent<S>(&mut self, value: S) -> &mut Self
        where S: Into<Cow<'a, str>>
    {
        self.indent = Cow::from(value.into());
        self
    }

    pub fn surround<S>(&mut self, surround_children: S) -> &mut Self
        where S: Into<Cow<'a, str>>
    {
        let str_cow = surround_children.into();

        self.before_children = str_cow.clone();
        self.after_children = str_cow;
        self
    }
}

fn write_md<'a, W>(nodes: &Vec<&Node>, out: &mut StrWriter<W>)
    where W: Write,
{
    for node in nodes {
        // TODO I don't reall like this MdSpec thing. I think I should either find the real pattern that everything uses,
        // or else just do everything inline (with a helper that's similar to MdSpec::children)
        let md_spec = match node {
            Node::BlockQuote(_) => MdSpec::children(|cs| {cs.surround("\n```\n");}),
            Node::Break(_) => MdSpec::children(|cs| {cs.write("\n");}),
            Node::Code(_) => MdSpec::children(|cs| {cs.surround("\n```\n");}),
            Node::Definition(_) => MdSpec::Unknown("Definition"),
            Node::Delete(_) => MdSpec::children(|cs| {cs.surround("~~");}),
            Node::Emphasis(_) => MdSpec::children(|cs| {cs.surround("*");}),
            Node::FootnoteDefinition(_) => MdSpec::Unknown("FootnoteDefinition"),
            Node::FootnoteReference(_) => MdSpec::Unknown("FootnoteReference"),
            Node::Heading(h) => MdSpec::children(|cs| {
                cs.surround("\n").indent("#".repeat(h.depth as usize));
            }),
            Node::Html(h) => {
                out.write(h.value.deref());
                MdSpec::Skip
            }
            Node::Image(i) => {
                out.write("![");
                out.write(i.alt.deref());
                out.write("](");
                out.write(i.url.deref());
                out.write(")");
                MdSpec::Skip
            }
            Node::ImageReference(_) => MdSpec::Unknown("ImageRef"),
            Node::InlineCode(c) => {
                out.write(c.value.deref());
                MdSpec::Skip
            }
            Node::InlineMath(_) => MdSpec::Unknown("InlineMath"),
            Node::Link(i) => {
                out.write("[");
                let children_borrows = i.children.iter().map(|c| c).collect();
                write_md(&children_borrows, out);
                out.write("](");
                out.write(i.url.deref());
                out.write(")");
                MdSpec::Skip
            }
            Node::LinkReference(_) => MdSpec::Unknown("LinkRef"),
            Node::List(list) => {
                out.write("\n");

                for (idx, li) in list.children.iter().enumerate() {
                    let Some(li_nodes) = li.children() else {
                        continue;
                    };
                    let counter = if list.ordered {
                        format!("{}.", idx + 1) // TODO align when we have different number of digits
                    } else if let ListItem(li_details) = li {
                        (match li_details.checked {
                            None => "- ",
                            Some(true) => "- [x]",
                            Some(false) => "- [ ]",
                        }).to_string()// TODO  remove need for to_string() here and in the else below.
                    } else {
                        "- ".to_string()
                    };
                    out.write(&*counter); // what the heck is even this
                    out.push_indent(" ".repeat(counter.len()));
                    write_md(&li_nodes.iter().map(|n| n).collect(), out);
                    out.pop_indent();
                }

                out.write("\n");
                Skip
            }
            Node::ListItem(i) => Unknown("internal error"),
            // Node::Math(_) => {}
            // Node::MdxFlowExpression(_) => {}
            // Node::MdxJsxFlowElement(_) => {}
            // Node::MdxJsxTextElement(_) => {}
            // Node::MdxTextExpression(_) => {}
            // Node::MdxjsEsm(_) => {}
            Node::Paragraph(p) => MdSpec::children(|cs| {cs.after_children = Cow::Borrowed("\n")} ),
            Node::Root(_) => Skip,
            // Node::Strong(_) => {}
            // Node::Table(_) => {}
            // Node::TableCell(_) => {}
            // Node::TableRow(_) => {}
            Node::Text(t) => {
                out.write(t.value.deref());
                Skip
            }
            // Node::ThematicBreak(_) => {}
            // Node::Toml(_) => {}
            // Node::Yaml(_) => {}
            _ => Skip, // TODO remove thi
        };
        match md_spec{
            MdSpec::Write(spec) => {
                out.push_indent(spec.indent);
                out.write(spec.literal.deref()); // all these derefs feel wrong! I think I have the wrong generic on out.write.
                out.write(spec.before_children.deref());
                if let Some(children) = node.children() {
                    write_md(&children.iter().map(|c| c).collect(), out); // TODO this stupid Node -> &Node trick
                }
                out.write(spec.after_children.deref());
                out.pop_indent();
            }
            Skip => {}
            Unknown(msg) => {eprintln!("unknown node: {}", msg)}
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
            (Selector::Heading(matcher), Node::Heading(h)) => {
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
