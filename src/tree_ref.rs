use crate::tree::{BlockQuote, CodeBlock, Image, Inline, Link, List, ListItem, MdElem, Paragraph, Section, Table};

/// An MdqNodeRef is a slice into an MdqNode tree, where each element can be outputted, and certain elements can be
/// selected.
#[derive(Debug, Clone, Copy, PartialEq)]
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

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ListItemRef<'a>(pub Option<u32>, pub &'a ListItem);

impl<'a> From<&'a MdElem> for MdElemRef<'a> {
    fn from(value: &'a MdElem) -> Self {
        match value {
            MdElem::ThematicBreak => Self::ThematicBreak,
            MdElem::Paragraph(p) => Self::Paragraph(p),
            MdElem::CodeBlock(c) => Self::CodeBlock(c),
            MdElem::Table(t) => Self::Table(t),
            MdElem::List(list) => Self::List(list),
            MdElem::BlockQuote(block) => Self::BlockQuote(block),
            MdElem::Section(section) => Self::Section(section),
            MdElem::Inline(child) => MdElemRef::Inline(child),
        }
    }
}

impl<'a> From<&'a BlockQuote> for MdElemRef<'a> {
    fn from(value: &'a BlockQuote) -> Self {
        MdElemRef::BlockQuote(value)
    }
}

impl<'a> From<ListItemRef<'a>> for MdElemRef<'a> {
    fn from(value: ListItemRef<'a>) -> Self {
        MdElemRef::ListItem(value)
    }
}

impl<'a> From<&'a Image> for MdElemRef<'a> {
    fn from(value: &'a Image) -> Self {
        MdElemRef::Image(value)
    }
}

impl<'a> From<&'a Link> for MdElemRef<'a> {
    fn from(value: &'a Link) -> Self {
        MdElemRef::Link(value)
    }
}

impl<'a> From<&'a Section> for MdElemRef<'a> {
    fn from(value: &'a Section) -> Self {
        MdElemRef::Section(value)
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
