use crate::tree::{
    Block, BlockQuote, CodeBlock, Container, Inline, LeafBlock, List, ListItem, MdqElem, Paragraph, Section, Table,
};

/// An MdqNodeRef is a slice into an MdqNode tree, where each element can be outputted, and certain elements can be
/// selected.
#[derive(Debug, Clone)]
pub enum MdElemRef<'a> {
    Section(&'a Section),
    ListItem(ListItemRef<'a>),
    Inline(&'a Inline),

    NonSelectable(NonSelectable<'a>),
}

#[derive(Debug, Clone)]
pub enum NonSelectable<'a> {
    ThematicBreak,
    CodeBlock(&'a CodeBlock),
    Paragraph(&'a Paragraph),
    BlockQuote(&'a BlockQuote),
    List(&'a List),
    Table(&'a Table),
}

#[derive(Debug, Clone, Copy)]
pub struct ListItemRef<'a>(pub Option<u32>, pub &'a ListItem);

impl<'a> From<&'a MdqElem> for MdElemRef<'a> {
    fn from(value: &'a MdqElem) -> Self {
        match value {
            MdqElem::Block(block) => match block {
                Block::LeafBlock(leaf) => match leaf {
                    LeafBlock::ThematicBreak => Self::NonSelectable(NonSelectable::ThematicBreak),
                    LeafBlock::Paragraph(p) => Self::NonSelectable(NonSelectable::Paragraph(p)),
                    LeafBlock::CodeBlock(c) => Self::NonSelectable(NonSelectable::CodeBlock(c)),
                    LeafBlock::Table(t) => Self::NonSelectable(NonSelectable::Table(t)),
                },
                Block::Container(container) => match container {
                    Container::List(list) => Self::NonSelectable(NonSelectable::List(list)),
                    Container::BlockQuote(block) => Self::NonSelectable(NonSelectable::BlockQuote(block)),
                    Container::Section(section) => Self::Section(section),
                },
            },
            MdqElem::Inline(v) => Self::Inline(v),
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
    pub fn wrap_vec(source: &'a Vec<MdqElem>) -> Vec<Self> {
        let mut result: Vec<Self> = Vec::with_capacity(source.len());
        for elem in source {
            result.push(elem.into());
        }
        result
    }
}
