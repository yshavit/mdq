use crate::fmt_md;
use crate::fmt_md_inlines::{MdInlinesWriter, MdInlinesWriterOptions, UrlAndTitle};
use crate::link_transform::LinkLabel;
use crate::output::Output;
use crate::tree::{CodeBlock, CodeVariant, Inline, LinkDefinition, LinkReference, MdElem, Section};
use crate::tree_ref::MdElemRef;
use markdown::mdast::AlignKind;
use serde::Serialize;
use std::borrow::{Borrow, Cow};
use std::collections::HashMap;

#[derive(Serialize)]
pub struct SerdeDoc<'a> {
    items: Vec<SerdeElem<'a>>,
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    links: HashMap<Cow<'a, str>, UrlAndTitle<'a>>,
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    footnotes: HashMap<&'a String, Vec<SerdeElem<'a>>>,
}

#[derive(Serialize)]
#[serde(rename_all = "snake_case")]
pub enum SerdeElem<'a> {
    Document(Vec<SerdeElem<'a>>),
    BlockQuote(Vec<SerdeElem<'a>>),
    CodeBlock {
        code: &'a String,
        #[serde(rename = "type")]
        code_type: CodeBlockType,
        metadata: Option<&'a String>,
        language: Option<&'a String>,
    },
    Paragraph(String),
    Link {
        display: String,
        #[serde(flatten)]
        link: LinkSerde<'a>,
    },
    Image {
        alt: &'a String,
        #[serde(flatten)]
        link: LinkSerde<'a>,
    },
    List(Vec<LiSerde<'a>>),
    ListItem(LiSerde<'a>),
    Section {
        depth: u8,
        title: String,
        body: Vec<SerdeElem<'a>>,
    },
    ThematicBreak,
    Table {
        alignments: Vec<AlignSerde>,
        rows: Vec<Vec<String>>,
    },
}

#[derive(Serialize)]
pub struct LinkSerde<'a> {
    url: &'a String,

    #[serde(skip_serializing_if = "Option::is_none")]
    title: &'a Option<String>,

    #[serde(skip_serializing_if = "Option::is_none")]
    reference: Option<Cow<'a, String>>,

    #[serde(skip_serializing_if = "Option::is_none")]
    reference_style: Option<LinkCollapseStyle>,
}

impl<'a> From<&'a LinkDefinition> for LinkSerde<'a> {
    fn from(value: &'a LinkDefinition) -> Self {
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
pub struct LiSerde<'a> {
    item: Vec<SerdeElem<'a>>,
    #[serde(rename = "type")]
    li_type: ListItemType,
    #[serde(skip_serializing_if = "Option::is_none")]
    checked: &'a Option<bool>,
    #[serde(skip_serializing_if = "Option::is_none")]
    index: Option<u32>,
}

#[derive(Serialize)]
#[serde(rename_all = "snake_case")]
pub enum LinkCollapseStyle {
    Collapsed,
    Shortcut,
}

#[derive(Serialize)]
pub enum ListItemType {
    Ordered,
    Unordered,
}

#[derive(Serialize)]
pub enum CodeBlockType {
    Code,
    Math,
    Toml,
    Yaml,
}

impl<'a> SerdeDoc<'a> {
    pub fn new(elems: &Vec<MdElemRef<'a>>, opts: MdInlinesWriterOptions) -> Self {
        let mut inlines_writer = MdInlinesWriter::new(opts);
        const DEFAULT_CAPACITY: usize = 16; // todo we can actually compute these if we want
        let mut result = Self {
            items: Vec::with_capacity(DEFAULT_CAPACITY),
            links: HashMap::with_capacity(DEFAULT_CAPACITY),
            footnotes: HashMap::with_capacity(DEFAULT_CAPACITY),
        };
        for elem in elems {
            let top = SerdeElem::build(*elem, &mut inlines_writer);
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

impl<'a> SerdeElem<'a> {
    fn build_multi<M>(elems: &'a [M], inlines_writer: &mut MdInlinesWriter<'a>) -> Vec<Self>
    where
        M: Borrow<MdElem>,
    {
        let mut result = Vec::with_capacity(elems.len());
        for elem in elems {
            result.push(Self::build(elem.borrow().into(), inlines_writer));
        }
        result
    }

    fn build(elem: MdElemRef<'a>, inlines_writer: &mut MdInlinesWriter<'a>) -> Self {
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
                    let (li_type, index) = match starting.as_mut() {
                        None => (ListItemType::Unordered, None),
                        Some(value) => {
                            let old = *value;
                            *value += 1;
                            (ListItemType::Ordered, Some(old))
                        }
                    };
                    li_refs.push(LiSerde {
                        item: Self::build_multi(&li.item, inlines_writer),
                        checked: &li.checked,
                        li_type,
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
            MdElemRef::Table(table) => {
                let mut rendered_rows = Vec::with_capacity(table.rows.len());
                for row in &table.rows {
                    let mut rendered_cells = Vec::with_capacity(row.len());
                    for cell in row {
                        rendered_cells.push(inlines_to_string(cell, inlines_writer));
                    }
                    rendered_rows.push(rendered_cells);
                }
                Self::Table {
                    alignments: table.alignments.iter().map(|a| a.into()).collect(),
                    rows: rendered_rows,
                }
            }
            MdElemRef::ThematicBreak => Self::ThematicBreak,
            MdElemRef::ListItem(li) => {
                let (li_type, index) = match li.0 {
                    None => (ListItemType::Unordered, None),
                    idx => (ListItemType::Ordered, idx),
                };
                Self::ListItem(LiSerde {
                    item: Self::build_multi(&li.1.item, inlines_writer),
                    checked: &li.1.checked,
                    li_type,
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
        }
    }
}

fn inlines_to_string<'a, I>(inlines: I, writer: &mut MdInlinesWriter<'a>) -> String
where
    I: IntoIterator<Item = &'a Inline>,
{
    let md: Vec<_> = inlines.into_iter().map(|inline| MdElemRef::Inline(inline)).collect();
    let mut output = Output::new(String::with_capacity(16)); // guess
    fmt_md::write_md_inlines(&mut output, md.into_iter().map(|e| e), writer);
    output.take_underlying().unwrap()
}
