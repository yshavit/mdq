use crate::tree_ref::MdElemRef;
use serde::ser::SerializeSeq;
use serde::{Serialize, Serializer};

pub struct MdElemsStream<'md, 's> {
    elems: &'s Vec<MdElemRef<'md>>,
}

impl<'md, 's> From<&'s Vec<MdElemRef<'md>>> for MdElemsStream {
    fn from(elems: &'s Vec<MdElemRef<'md>>) -> Self {
        Self { elems }
    }
}

impl<'md, 's> Serialize for MdElemsStream<'md, 's> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        // TODO: create Inlines holder, use it to serialize something of form:
        {
            items: <vec>,
            references: <vec>
        }
        // or maybe:
        {
            items: []items
        }
        where:
            item: {
                item: <contents>
                references: []refs
            }
    }
}

impl<'md, 's> MdElemsStream<'md, 's> {
    fn serialize_elem<S: Serializer>(&mut self, elem: MdElemRef<'md>, out: S) {
        todo!();
        match elem {
            MdElemRef::Doc(doc) => {}
            MdElemRef::BlockQuote(block) => {}
            MdElemRef::CodeBlock(block) => {}
            MdElemRef::Inline(inline) => {}
            MdElemRef::List(list) => {}
            MdElemRef::Paragraph(paragraph) => {}
            MdElemRef::Section(section) => {}
            MdElemRef::Table(table) => {}
            MdElemRef::ThematicBreak => {}
            MdElemRef::ListItem(list_item) => {}
            MdElemRef::Link(link) => {}
            MdElemRef::Image(image) => {}
        }
    }
}
