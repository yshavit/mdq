use crate::fmt_md_inlines::{MdInlinesWriter, MdInlinesWriterOptions, UrlAndTitle};
use crate::link_transform::LinkLabel;
use crate::md_elem::elem::*;
use crate::md_elem::*;
use crate::util::output::Output;
use markdown::mdast::AlignKind;
use serde::{Serialize, Serializer};
use std::borrow::{Borrow, Cow};
use std::collections::HashMap;

#[derive(Serialize)]
pub struct MdSerde<'md> {
    items: Vec<SerdeElem<'md>>,
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    links: HashMap<Cow<'md, str>, UrlAndTitle<'md>>,
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    footnotes: HashMap<String, Vec<SerdeElem<'md>>>,
}

#[derive(Serialize)]
#[serde(rename_all = "snake_case")]
pub enum SerdeElem<'md> {
    Document(Vec<SerdeElem<'md>>),
    BlockQuote(Vec<SerdeElem<'md>>),
    CodeBlock {
        code: &'md String,

        #[serde(rename = "type")]
        code_type: CodeBlockType,

        #[serde(skip_serializing_if = "Option::is_none")]
        language: Option<&'md String>,

        #[serde(skip_serializing_if = "Option::is_none")]
        metadata: Option<&'md String>,
    },
    Paragraph(String),
    Link {
        display: String,
        #[serde(flatten)]
        link: LinkSerde<'md>,
    },
    Image {
        alt: &'md String,
        #[serde(flatten)]
        link: LinkSerde<'md>,
    },
    List(Vec<LiSerde<'md>>),
    ListItem(LiSerde<'md>),
    Section {
        depth: u8,
        title: String,
        body: Vec<SerdeElem<'md>>,
    },
    #[serde(serialize_with = "serialize_thematic_break")]
    ThematicBreak,
    Table {
        alignments: Vec<AlignSerde>,
        rows: Vec<Vec<String>>,
    },
    Html {
        value: &'md String,
    },
}

fn serialize_thematic_break<S: Serializer>(ser: S) -> Result<S::Ok, S::Error> {
    ser.serialize_none()
}

#[derive(Serialize)]
pub struct LinkSerde<'md> {
    url: &'md String,

    #[serde(skip_serializing_if = "Option::is_none")]
    title: &'md Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    reference: Option<Cow<'md, String>>,

    #[serde(skip_serializing_if = "Option::is_none")]
    reference_style: Option<LinkCollapseStyle>,
}

impl<'md> From<&'md LinkDefinition> for LinkSerde<'md> {
    fn from(value: &'md LinkDefinition) -> Self {
        let LinkDefinition { url, title, reference } = value;

        let (reference, reference_style) = match reference {
            LinkReference::Inline => (None, None),
            LinkReference::Full(reference) => (Some(Cow::Borrowed(reference)), None),
            LinkReference::Collapsed => (None, Some(LinkCollapseStyle::Collapsed)),
            LinkReference::Shortcut => (None, Some(LinkCollapseStyle::Shortcut)),
        };
        Self {
            url,
            title,
            reference,
            reference_style,
        }
    }
}

#[derive(Serialize)]
#[serde(rename_all = "snake_case")]
pub enum AlignSerde {
    Left,
    Right,
    Center,
    None,
}

impl From<&AlignKind> for AlignSerde {
    fn from(value: &AlignKind) -> Self {
        match value {
            AlignKind::Left => Self::Left,
            AlignKind::Right => Self::Right,
            AlignKind::Center => Self::Center,
            AlignKind::None => Self::None,
        }
    }
}

#[derive(Serialize)]
pub struct LiSerde<'md> {
    item: Vec<SerdeElem<'md>>,

    #[serde(skip_serializing_if = "Option::is_none")]
    index: Option<u32>,

    #[serde(skip_serializing_if = "Option::is_none")]
    checked: &'md Option<bool>,
}

#[derive(Serialize)]
#[serde(rename_all = "snake_case")]
pub enum LinkCollapseStyle {
    Collapsed,
    Shortcut,
}

#[derive(Serialize)]
#[serde(rename_all = "snake_case")]
pub enum CodeBlockType {
    Code,
    Math,
    Toml,
    Yaml,
}

impl<'md> MdSerde<'md> {
    pub fn new(elems: &[MdElemRef<'md>], ctx: &'md MdContext, opts: MdInlinesWriterOptions) -> Self {
        let mut inlines_writer = MdInlinesWriter::new(ctx, opts);
        const DEFAULT_CAPACITY: usize = 16; // we could compute these, but it's not really worth it
        let mut result = MdSerde {
            items: Vec::with_capacity(elems.len()),
            links: HashMap::with_capacity(DEFAULT_CAPACITY),
            footnotes: HashMap::with_capacity(DEFAULT_CAPACITY),
        };
        for elem in elems {
            let top = SerdeElem::build(elem.to_owned(), &mut inlines_writer);
            result.items.push(top);
        }
        for (link_label, url) in inlines_writer.drain_pending_links() {
            let link_to_str = match link_label {
                LinkLabel::Text(text) => text,
                LinkLabel::Inline(inlines) => Cow::Owned(inlines_to_string(inlines, &mut inlines_writer)),
            };
            result.links.insert(link_to_str, url);
        }
        for (footnote_name, footnote_contents) in inlines_writer.drain_pending_footnotes() {
            result.footnotes.insert(
                footnote_name,
                SerdeElem::build_multi(footnote_contents, &mut inlines_writer),
            );
        }
        result
    }
}

impl<'md> SerdeElem<'md> {
    fn build_multi<M>(elems: &'md [M], inlines_writer: &mut MdInlinesWriter<'md>) -> Vec<Self>
    where
        M: Borrow<MdElem>,
    {
        let mut result = Vec::with_capacity(elems.len());
        for elem in elems {
            result.push(Self::build(elem.borrow().into(), inlines_writer));
        }
        result
    }

    fn build(elem: MdElemRef<'md>, inlines_writer: &mut MdInlinesWriter<'md>) -> Self {
        match elem {
            MdElemRef::Doc(doc) => Self::Document(Self::build_multi(doc, inlines_writer)),
            MdElemRef::BlockQuote(bq) => Self::BlockQuote(Self::build_multi(&bq.body, inlines_writer)),
            MdElemRef::CodeBlock(cb) => {
                let CodeBlock { variant, value } = cb;
                let (code_type, metadata, language) = match variant {
                    CodeVariant::Code(None) => (CodeBlockType::Code, None, None),
                    CodeVariant::Code(Some(code_opts)) => (
                        CodeBlockType::Code,
                        code_opts.metadata.as_ref(),
                        Some(&code_opts.language),
                    ),
                    CodeVariant::Math { metadata } => (CodeBlockType::Math, metadata.as_ref(), None),
                    CodeVariant::Toml => (CodeBlockType::Toml, None, None),
                    CodeVariant::Yaml => (CodeBlockType::Yaml, None, None),
                };
                Self::CodeBlock {
                    code: value,
                    code_type,
                    metadata,
                    language,
                }
            }
            MdElemRef::Inline(inline) => {
                let as_string = inlines_to_string([inline], inlines_writer);
                Self::Paragraph(as_string)
            }
            MdElemRef::List(list) => {
                let mut starting = list.starting_index;
                let mut li_refs = Vec::with_capacity(list.items.len());
                for li in &list.items {
                    let index = match starting.as_mut() {
                        None => None,
                        Some(value) => {
                            let old = *value;
                            *value += 1;
                            Some(old)
                        }
                    };
                    li_refs.push(LiSerde {
                        item: Self::build_multi(&li.item, inlines_writer),
                        checked: &li.checked,
                        index,
                    })
                }
                Self::List(li_refs)
            }
            MdElemRef::Paragraph(p) => {
                let as_string = inlines_to_string(&p.body, inlines_writer);
                Self::Paragraph(as_string)
            }
            MdElemRef::Section(section) => {
                let Section { depth, title, body } = section;
                let depth = *depth;
                let title = inlines_to_string(title, inlines_writer);
                let body = Self::build_multi(body, inlines_writer);
                Self::Section { depth, title, body }
            }
            MdElemRef::Table(table) => Self::build(MdElemRef::TableSlice(table.into()), inlines_writer),
            MdElemRef::TableSlice(table) => {
                let mut rendered_rows = Vec::with_capacity(table.rows().len());
                for row in table.rows() {
                    let mut rendered_cells = Vec::with_capacity(row.len());
                    for maybe_cell in row {
                        let rendered_cell = match maybe_cell {
                            Some(cell) => inlines_to_string(*cell, inlines_writer),
                            None => "".to_string(),
                        };
                        rendered_cells.push(rendered_cell)
                    }
                    rendered_rows.push(rendered_cells);
                }
                Self::Table {
                    alignments: table.alignments().iter().map(|a| a.into()).collect(),
                    rows: rendered_rows,
                }
            }
            MdElemRef::ThematicBreak => Self::ThematicBreak,
            MdElemRef::ListItem(li) => {
                let index = match li.0 {
                    idx => idx,
                };
                Self::ListItem(LiSerde {
                    item: Self::build_multi(&li.1.item, inlines_writer),
                    checked: &li.1.checked,
                    index,
                })
            }
            MdElemRef::Link(link) => Self::Link {
                display: inlines_to_string(&link.text, inlines_writer),
                link: (&link.link_definition).into(),
            },
            MdElemRef::Image(img) => Self::Image {
                alt: &img.alt,
                link: (&img.link).into(),
            },
            MdElemRef::Html(value) => Self::Html { value: value.0 },
        }
    }
}

fn inlines_to_string<'md, I>(inlines: I, writer: &mut MdInlinesWriter<'md>) -> String
where
    I: IntoIterator<Item = &'md Inline>,
{
    let mut output = Output::without_text_wrapping(String::with_capacity(16)); // guess
    writer.write_line(&mut output, inlines);
    output.take_underlying().unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::fmt_md_inlines::MdInlinesWriterOptions;
    use crate::link_transform::LinkTransform;
    use crate::md_elem::elem_ref::*;
    use crate::md_elem::tree_test_utils::*;
    use crate::util::utils_for_test::*;

    variants_checker!(CHECKER = MdElemRef {
        Doc(_),

        BlockQuote(_),
        CodeBlock(_),
        Inline(_),
        List(_),
        Paragraph(_),
        Section(_),
        Table(_),
        TableSlice(_),
        ThematicBreak,
        Html(_),

        ListItem(_),
        Link(_),
        Image(_),
    });

    macro_rules! json_kvs {
        () => {};
        ($key:literal : $value:tt) => {
            concat!(stringify!($key), ":", json_str!($value))
        };
        ($key:literal : $value:tt, $($rest:tt)+) => {
            concat!(stringify!($key), ":", json_str!($value), ",", json_kvs!($($rest)*))
        };
    }

    macro_rules! json_seq {
        () => {""};
        ($value:tt) => {
            json_str!($value)
        };
        ($value:tt, $($rest:tt)+) => {
            concat!(json_str!($value), ",", json_seq!($($rest),*))
        };
    }

    /// Builds a json literal out of a string. Unlike [serde_json::json], this macro preserves the order of map
    /// (object) entries.
    macro_rules! json_str {
        () => {""};
        (null) => {"null"};
        ($one:literal) => {stringify!($one)};
        ({$($kvs:tt)*}) => {
            concat!(
                "{",
                json_kvs!($($kvs)*),
                "}"
            )
        };
        ([$($values:tt)*]) => {
            concat!(
                "[",
                json_seq!($($values)*),
                "]"
            )
        };
    }

    mod doc {
        use super::*;

        #[test]
        fn doc() {
            let paragraphs = md_elems!("alpha", "bravo");
            let elems_ref = MdElemRef::Doc(&paragraphs);
            check_md_ref(
                elems_ref,
                json_str!(
                    {"items": [
                        {"document": [
                            {"paragraph": "alpha"},
                            {"paragraph": "bravo"}
                        ]}
                    ]}
                ),
            );
        }
    }

    #[test]
    fn block_quote() {
        check(
            MdElem::BlockQuote(BlockQuote {
                body: md_elems!("alpha"),
            }),
            json_str!(
                {"items": [
                    {"block_quote": [
                        {"paragraph": "alpha"}
                    ]}
                ]}
            ),
        );
    }

    #[test]
    fn code_block_simple() {
        check(
            MdElem::CodeBlock(CodeBlock {
                variant: CodeVariant::Code(None),
                value: "my code".to_string(),
            }),
            json_str!(
                {"items": [
                    {"code_block": {
                        "code": "my code",
                        "type": "code"
                    }}
                ]}
            ),
        );
    }

    #[test]
    fn code_block_full() {
        check(
            MdElem::CodeBlock(CodeBlock {
                variant: CodeVariant::Code(Some(CodeOpts {
                    language: "rust".to_string(),
                    metadata: Some("my-metadata".to_string()),
                })),
                value: "my code".to_string(),
            }),
            json_str!(
                {"items": [
                    {"code_block": {
                        "code": "my code",
                        "type": "code",
                        "language": "rust",
                        "metadata": "my-metadata"
                    }}
                ]}
            ),
        );
    }

    #[test]
    fn image() {
        check(
            md_elem!(Inline::Image {
                alt: "the alt text".to_string(),
                link: LinkDefinition {
                    url: "https://example.com/image.png".to_string(),
                    title: None,
                    reference: LinkReference::Inline,
                }
            }),
            json_str!(
                {"items": [
                    {"paragraph": "![the alt text](https://example.com/image.png)"
                }]}
            ),
        );
    }

    #[test]
    fn image_ref() {
        check_md_ref(
            MdElemRef::Image(&Image {
                alt: "the alt text".to_string(),
                link: LinkDefinition {
                    url: "https://example.com/image.png".to_string(),
                    title: None,
                    reference: LinkReference::Inline,
                },
            }),
            json_str!(
                {"items": [
                    {"image": {
                        "alt": "the alt text",
                        "url": "https://example.com/image.png"
                    }}
                ]}
            ),
        );
    }

    #[test]
    fn link_with_reference() {
        check(
            md_elem!(Inline::Link {
                text: vec![mdq_inline!("alpha")],
                link_definition: LinkDefinition {
                    url: "https://example.com/a".to_string(),
                    title: None,
                    reference: LinkReference::Full("a".to_string()),
                }
            }),
            json_str!(
                {
                    "items": [
                        {"paragraph": "[alpha][a]"}
                    ],
                    "links": {
                        "a": {
                            "url":"https://example.com/a"
                        }
                    }
                }
            ),
        );
    }

    #[test]
    fn link_ref_with_reference() {
        check_md_ref(
            MdElemRef::Link(&Link {
                text: vec![mdq_inline!("hello")],
                link_definition: LinkDefinition {
                    url: "https://example.com/hi.html".to_string(),
                    title: None,
                    reference: LinkReference::Collapsed,
                },
            }),
            json_str!(
                {"items": [
                    {"link": {
                        "display": "hello",
                        "url": "https://example.com/hi.html",
                        "reference_style": "collapsed"
                    }}
                ]}
            ),
        );
    }

    #[test]
    fn thematic_break() {
        check(
            MdElem::ThematicBreak,
            json_str!(
                {"items": [
                    {"thematic_break":null}
                ]}
            ),
        );
    }

    #[test]
    fn list() {
        check(
            md_elem!(List {
                starting_index: Some(1),
                items: vec![
                    ListItem {
                        checked: None,
                        item: md_elems!("one"),
                    },
                    ListItem {
                        checked: Some(false),
                        item: md_elems!("two"),
                    }
                ]
            }),
            json_str!(
                {"items": [
                    {"list": [
                        {"item": [{"paragraph":"one"}], "index":1},
                        {"item": [{"paragraph":"two"}], "index":2, "checked":false}
                    ]}
                ]}
            ),
        );
    }

    #[test]
    fn list_item() {
        check_md_ref(
            MdElemRef::ListItem(ListItemRef(
                None,
                &ListItem {
                    checked: None,
                    item: md_elems!("hello, world"),
                },
            )),
            json_str!(
                {"items":[
                    {"list_item": {
                        "item": [{"paragraph":"hello, world"}]}
                    }
                ]}
            ),
        );
    }

    #[test]
    fn paragraph() {
        check(
            md_elem!("the text"),
            json_str!(
                {"items": [
                    {"paragraph":"the text"}
                ]}
            ),
        );
    }

    #[test]
    fn section() {
        check(
            md_elem!(Section {
                depth: 2,
                title: vec![mdq_inline!("section title")],
                body: md_elems!["alpha", "bravo"]
            }),
            json_str!(
                {"items":[
                    {"section":{
                        "depth":2,
                        "title":"section title",
                        "body":[
                            {"paragraph":"alpha"},
                            {"paragraph":"bravo"}
                        ]
                    }}
                ]}
            ),
        );
    }

    #[test]
    fn table() {
        check(
            md_elem!(Table {
                alignments: vec![AlignKind::Left, AlignKind::None],
                rows: vec![
                    vec![vec![mdq_inline!("R1C1")], vec![mdq_inline!("R1C2")]],
                    vec![vec![mdq_inline!("R2C1")], vec![mdq_inline!("R2C2")]],
                ]
            }),
            json_str!(
                {"items":[
                    {"table":{
                        "alignments": ["left", "none"],
                        "rows": [
                            [ "R1C1", "R1C2" ],
                            [ "R2C1", "R2C2" ]
                        ]
                    }}
                ]}
            ),
        );
    }

    #[test]
    fn table_slice() {
        let table = Table {
            alignments: vec![AlignKind::Left, AlignKind::None],
            rows: vec![
                vec![vec![mdq_inline!("R1C1")], vec![mdq_inline!("R1C2")]],
                vec![vec![mdq_inline!("R2C1")], vec![mdq_inline!("R2C2")]],
            ],
        };

        check_md_ref(
            MdElemRef::TableSlice((&table).into()),
            json_str!(
                {"items":[
                    {"table":{
                        "alignments": ["left", "none"],
                        "rows": [
                            [ "R1C1", "R1C2" ],
                            [ "R2C1", "R2C2" ]
                        ]
                    }}
                ]}
            ),
        );
    }

    #[test]
    fn block_html() {
        check(
            MdElem::BlockHtml("<div />".to_string()),
            json_str!(
                {"items":[
                    {"html":{
                        "value": "<div />"
                    }}
                ]}
            ),
        );
    }

    fn check(given: MdElem, expect: &str) {
        let opts = MdInlinesWriterOptions {
            link_format: LinkTransform::Keep,
            renumber_footnotes: false,
        };
        check_with(opts, MdElemRef::from(&given), expect);
    }

    fn check_md_ref(given: MdElemRef, expect: &str) {
        let opts = MdInlinesWriterOptions {
            link_format: LinkTransform::Keep,
            renumber_footnotes: false,
        };
        check_with(opts, given, expect);
    }

    fn check_with(opts: MdInlinesWriterOptions, elem_ref: MdElemRef, expect: &str) {
        CHECKER.see(&elem_ref);
        let mut actual_bytes = Vec::with_capacity(32);
        serde_json::to_writer(&mut actual_bytes, &MdSerde::new(&[elem_ref], &MdContext::empty(), opts)).unwrap();
        let actual_string = String::from_utf8(actual_bytes).unwrap();
        assert_eq!(actual_string, expect);
    }
}
