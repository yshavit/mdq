use crate::tree::{
    BlockQuote, CodeBlock, Container, Image, Inline, LeafBlock, Link, List, ListItem, MdElem, Paragraph, Section, Table,
};

/// An MdqNodeRef is a slice into an MdqNode tree, where each element can be outputted, and certain elements can be
/// selected.
#[derive(Debug, Clone, Copy)]
pub enum MdElemRef<'a> {
    // Multiple elements that form a single area
    Doc(&'a Vec<MdElem>),

    // main elements
    BlockQuote(&'a BlockQuote),
    CodeBlock(&'a CodeBlock),
    Inline(&'a Inline),
    List(&'a List),
    Paragraph(&'a Paragraph),
    Section(&'a Section),
    Table(&'a Table),
    ThematicBreak,

    // sub-elements
    ListItem(ListItemRef<'a>),
    Link(&'a Link),
    Image(&'a Image),
}

#[derive(Debug, Clone, Copy)]
pub struct ListItemRef<'a>(pub Option<u32>, pub &'a ListItem);

impl<'a> From<&'a MdElem> for MdElemRef<'a> {
    fn from(value: &'a MdElem) -> Self {
        match value {
            MdElem::LeafBlock(leaf) => match leaf {
                LeafBlock::ThematicBreak => Self::ThematicBreak,
                LeafBlock::Paragraph(p) => Self::Paragraph(p),
                LeafBlock::CodeBlock(c) => Self::CodeBlock(c),
                LeafBlock::Table(t) => Self::Table(t),
            },
            MdElem::Container(container) => match container {
                Container::List(list) => Self::List(list),
                Container::BlockQuote(block) => Self::BlockQuote(block),
                Container::Section(section) => Self::Section(section),
            },
            MdElem::Inline(child) => MdElemRef::Inline(child),
        }
    }
}

#[macro_export]
macro_rules! wrap_mdq_refs {
    ($variant:ident: $source:expr) => {{
        let source = $source;
        let mut result: Vec<MdElemRef> = Vec::with_capacity(source.len());
        for elem in source {
            result.push(MdElemRef::$variant(elem));
        }
        result
    }};
}
