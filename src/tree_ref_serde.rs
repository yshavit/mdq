use crate::fmt_md_inlines::{MdInlinesWriter, MdInlinesWriterOptions, UrlAndTitle};
use crate::link_transform::LinkLabel;
use crate::output::Output;
use crate::tree::Inline;
use crate::tree_ref::MdElemRef;
use serde::ser::SerializeSeq;
use serde::{Serialize, Serializer};
use std::borrow::Cow;
use std::collections::HashMap;

#[derive(Serialize)]
pub enum ElemType {
    Document,
    Section,
}

#[derive(Serialize)]
#[serde(untagged)]
pub enum SerdeElem<'a> {
    Node {
        #[serde(rename = "type")]
        elem_type: ElemType,
        contents: &'a SerdeElem<'a>,
    },
    Inline(String),
}

#[derive(Serialize)]
pub struct SerdeDoc<'a> {
    items: Vec<SerdeElem<'a>>,
    links: HashMap<Cow<'a, str>, UrlAndTitle<'a>>,
    footnotes: HashMap<&'a String, SerdeElem<'a>>,

    #[serde(skip_serializing)]
    stored_items: Holder<SerdeElem<'a>>,
}

struct Holder<E>(Vec<E>);

impl<E> Holder<E> {
    fn with_capacity(size: usize) -> Self {
        Self(Vec::with_capacity(size))
    }

    fn store(&mut self, item: E) -> &E {
        self.0.push(item);
        &self.0[&self.0.len() - 1]
    }
}

impl<'a> SerdeDoc<'a> {
    pub fn new(elems: &Vec<MdElemRef>, opts: MdInlinesWriterOptions) -> Self {
        let mut inlines_writer = MdInlinesWriter::new(opts);
        const DEFAULT_CAPACITY: usize = 16; // todo we can actually compute these if we want
        let mut result = Self {
            items: Vec::with_capacity(DEFAULT_CAPACITY),
            links: HashMap::with_capacity(DEFAULT_CAPACITY),
            footnotes: HashMap::with_capacity(DEFAULT_CAPACITY),
            stored_items: Holder::with_capacity(DEFAULT_CAPACITY),
        };
        for elem in elems {
            let top = SerdeDoc::build(*elem, &mut result.stored_items, &mut inlines_writer);
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
    fn build(elem: MdElemRef<'a>, storage: &mut Holder<Self>, inlines_writer: &mut MdInlinesWriter<'a>) {
        match elem {
            MdElemRef::Doc(doc) => {
                for elem in doc {
                    let elem_serde = Self::build(elem.into(), storage, inlines_writer);
                }
            }
            MdElemRef::BlockQuote(_) => {}
            MdElemRef::CodeBlock(_) => {}
            MdElemRef::Inline(_) => {}
            MdElemRef::List(_) => {}
            MdElemRef::Paragraph(_) => {}
            MdElemRef::Section(_) => {}
            MdElemRef::Table(_) => {}
            MdElemRef::ThematicBreak => {}
            MdElemRef::ListItem(_) => {}
            MdElemRef::Link(_) => {}
            MdElemRef::Image(_) => {}
        }
    }
}

fn inlines_to_string(inlines: &Vec<Inline>, writer: &mut MdInlinesWriter) -> String {
    md_to_string(inlines.iter().map(|inline| MdElemRef::Inline(inline)).collect())
}

fn md_to_string(md: &Vec<MdElemRef>, writer: &mut MdInlinesWriter) -> String {
    let mut output = Output::new(String::with_capacity(16)); // guess

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
