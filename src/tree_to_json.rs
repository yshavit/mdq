use crate::tree::{CodeOpts, CodeVariant, Inline, MdqNode};
use markdown::mdast::AlignKind;
use serde_json::{json, Map, Value};
use std::borrow::Borrow;

const BODY_KEY: &str = "body";

pub fn nodes_to_json<N, R>(nodes: &[N]) -> Value
where
    N: Borrow<MdqNode>,
    R: InlineResolver,
{
    Value::Array(
        nodes
            .iter()
            .map(|n| node_to_json::<R>(n.borrow()))
            .collect(),
    )
}

pub fn node_to_json<R: InlineResolver>(node: &MdqNode) -> Value {
    match node {
        MdqNode::Root { body } => to_jsons::<R>(body),
        MdqNode::Header {
            depth,
            title,
            body: contents,
        } => json!({
            "header": json!({
                    "title": R::inlines_to_value(title),
                    "depth": json!(depth),
                    BODY_KEY: to_jsons::<R>(contents),
            })
        }),
        MdqNode::Paragraph { body } => json!({"paragraph": R::inlines_to_value(body)}),
        MdqNode::BlockQuote { body } => json!({"block_quote": to_jsons::<R>(body)}),
        MdqNode::List {
            starting_index,
            items,
        } => {
            let mut as_map = Map::new();
            let is_ordered = match starting_index {
                Some(start_idx) => {
                    as_map.insert("type".to_string(), json!("ordered"));
                    as_map.insert("start_at".to_string(), json!(start_idx));
                    true
                }
                None => {
                    as_map.insert("type".to_string(), json!("unordered"));
                    false
                }
            };
            let items = items
                .iter()
                .map(|li| {
                    if is_ordered {
                        json!({
                            "item": to_jsons::<R>(&li.children),
                        })
                    } else {
                        json!({
                            "checked": li.checked.clone(),
                            "item": to_jsons::<R>(&li.children),
                        })
                    }
                })
                .collect();
            as_map.insert("items".to_string(), Value::Array(items));

            json!({"list": Value::Object(as_map)})
        }
        MdqNode::Table {
            alignments: align,
            rows,
        } => {
            let aligns: Vec<Option<&str>> = align
                .iter()
                .map(|a| match a {
                    AlignKind::Left => Some("left"),
                    AlignKind::Right => Some("right"),
                    AlignKind::Center => Some("center"),
                    AlignKind::None => None,
                })
                .collect();
            json!({
                "table": {
                    "column_alignments": json!(aligns),
                    "rows": rows.iter().map(|row| {
                        row.iter().map(|col|
                            R::inlines_to_value(col)
                        ).collect::<Vec<_>>()
                    }).collect::<Vec<_>>()
                }
            })
        }
        MdqNode::ThematicBreak => {
            json!({"thematic_break": Value::Null})
        }
        MdqNode::CodeBlock { variant, value } => match variant {
            CodeVariant::Code(opts) => {
                let (lang, meta) = match opts {
                    None => (None, None),
                    Some(CodeOpts { language, metadata }) => {
                        (Some(language.to_string()), metadata.to_owned())
                    }
                };
                json!({
                    "code":json!({
                        "language": lang,
                        "metadata": meta,
                        BODY_KEY: json!(value),
                    })
                })
            }
            CodeVariant::Math { metadata } => json!({
                "math": json!({
                    "metadata": metadata,
                    BODY_KEY: json!(value),
                })
            }),
            CodeVariant::Toml => json!({"toml": json!(value)}),
            CodeVariant::Yaml => json!({"yaml": json!(value)}),
        },
        MdqNode::Inline(inline) => R::inlines_to_value(&[inline]),
    }
}

pub struct TextOnly {
    _private: bool,
}

impl TextOnly {
    pub fn line_to_string<I>(line: &[I]) -> String
    where
        I: Borrow<Inline>,
    {
        let mut str = String::new();
        for i in line {
            Self::build_string(&mut str, i.borrow())
        }
        str
    }

    fn build_string(out: &mut String, elem: &Inline) {
        match elem {
            Inline::Span { children, .. } => {
                for child in children {
                    Self::build_string(out, child);
                }
            }
            Inline::Text { value, .. } => out.push_str(&value.replace("\n", " ")),
        }
    }
}

impl InlineResolver for TextOnly {
    fn inlines_to_value<I: Borrow<Inline>>(inlines: &[I]) -> Value {
        Value::String(Self::line_to_string(inlines))
    }
}

pub trait InlineResolver {
    fn inlines_to_value<I: Borrow<Inline>>(inlines: &[I]) -> Value;
}

fn to_jsons<R: InlineResolver>(nodes: &[MdqNode]) -> Value {
    Value::Array(nodes.iter().map(|n| node_to_json::<R>(n)).collect())
}
