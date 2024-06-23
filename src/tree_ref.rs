use crate::tree::{
    Block, BlockQuote, CodeBlock, Container, Inline, LeafBlock, List, ListItem, MdElem, Paragraph, Section, Table,
};

/// An MdqNodeRef is a slice into an MdqNode tree, where each element can be outputted, and certain elements can be
/// selected.
#[derive(Debug, Clone)]
pub enum MdElemRef<'a> {
    BlockQuote(&'a BlockQuote),
    CodeBlock(&'a CodeBlock),
    Inline(&'a Inline),
    List(&'a List),
    ListItem(ListItemRef<'a>),
    Paragraph(&'a Paragraph),
    Section(&'a Section),
    Table(&'a Table),
    ThematicBreak,
}

#[derive(Debug, Clone, Copy)]
pub struct ListItemRef<'a>(pub Option<u32>, pub &'a ListItem);

impl<'a> From<&'a MdElem> for MdElemRef<'a> {
    fn from(value: &'a MdElem) -> Self {
        match value {
            MdElem::Block(block) => match block {
                Block::LeafBlock(leaf) => match leaf {
                    LeafBlock::ThematicBreak => Self::ThematicBreak,
                    LeafBlock::Paragraph(p) => Self::Paragraph(p),
                    LeafBlock::CodeBlock(c) => Self::CodeBlock(c),
                    LeafBlock::Table(t) => Self::Table(t),
                },
                Block::Container(container) => match container {
                    Container::List(list) => Self::List(list),
                    Container::BlockQuote(block) => Self::BlockQuote(block),
                    Container::Section(section) => Self::Section(section),
                },
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

impl<'a> MdElemRef<'a> {
    pub fn wrap_vec(source: &'a Vec<MdElem>) -> Vec<Self> {
        let mut result: Vec<Self> = Vec::with_capacity(source.len());
        for elem in source {
            result.push(elem.into());
        }
        result
    }
}
