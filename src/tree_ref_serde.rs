use crate::fmt_md_inlines::{MdInlinesWriter, MdInlinesWriterOptions, UrlAndTitle};
use crate::link_transform::LinkLabel;
use crate::output::Output;
use crate::tree::{CodeBlock, CodeVariant, Inline, LinkDefinition, LinkReference, MdElem, Section};
use crate::tree_ref::MdElemRef;
use crate::{fmt_md, tree};
use serde::Serialize;
use std::borrow::{Borrow, Cow};
use std::collections::HashMap;
use tree::Link;

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
        url: &'a String,

        #[serde(skip_serializing_if = "Option::is_none")]
        title: &'a Option<String>,

        #[serde(skip_serializing_if = "Option::is_none")]
        reference: Option<Cow<'a, String>>,

        #[serde(skip_serializing_if = "Option::is_none")]
        reference_style: Option<LinkCollapseStyle>,
    },
    List(Vec<LiSerde<'a>>),
    Section {
        depth: u8,
        title: String,
        body: Vec<SerdeElem<'a>>,
    },
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

#[derive(Serialize)]
pub struct SerdeDoc<'a> {
    items: Vec<SerdeElem<'a>>,
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    links: HashMap<Cow<'a, str>, UrlAndTitle<'a>>,
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    footnotes: HashMap<&'a String, SerdeElem<'a>>,
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
            // TODO
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
            // MdElemRef::Table(_) => {}
            // MdElemRef::ThematicBreak => {}
            // MdElemRef::ListItem(li) => {}
            MdElemRef::Link(link) => {
                let Link { text, link_definition } = link;
                let LinkDefinition { url, title, reference } = link_definition;

                let display = inlines_to_string(text, inlines_writer);
                let (reference, reference_style) = match reference {
                    LinkReference::Inline => (None, None),
                    LinkReference::Full(reference) => (Some(Cow::Borrowed(reference)), None),
                    LinkReference::Collapsed => (None, Some(LinkCollapseStyle::Collapsed)),
                    LinkReference::Shortcut => (None, Some(LinkCollapseStyle::Shortcut)),
                };
                Self::Link {
                    display,
                    url,
                    title,
                    reference,
                    reference_style,
                }
            }
            // MdElemRef::Image(_) => {}
            _ => todo!(),
        }
    }
}

fn inlines_to_string<'a, I>(inlines: I, writer: &mut MdInlinesWriter<'a>) -> String
where
    I: IntoIterator<Item = &'a Inline>,
{
    md_to_string(
        &inlines.into_iter().map(|inline| MdElemRef::Inline(inline)).collect(),
        writer,
    )
}

fn md_to_string<'a>(md: &Vec<MdElemRef<'a>>, writer: &mut MdInlinesWriter<'a>) -> String {
    let mut output = Output::new(String::with_capacity(16)); // guess
    fmt_md::write_md_inlines(&mut output, md.iter().map(|e| *e), writer);

    output.take_underlying().unwrap()
}

// impl<'md, 's> Serialize for MdElemsStream<'md, 's> {
//     fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
//     where
//         S: Serializer,
//     {
//         todo
//         // TODO: create Inlines holder, use it to serialize something of form:
//         // {
//         //     items: <vec>,
//         //     references: <vec>
//         // }
//         // // or maybe:
//         // {
//         //     items: []items
//         // }
//         // where:
//         //     item: {
//         //         item: <contents>
//         //         references: []refs
//         //     }
//     }
// }
