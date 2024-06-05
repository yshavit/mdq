use std::borrow::Borrow;
use markdown::mdast::Node;
use serde_yaml::Value;

pub fn nodes_to_yaml<N>(node: &Node) -> String {
    let parsed = to_json(&node);
    serde_yaml::to_string(&parsed).expect("err")
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
