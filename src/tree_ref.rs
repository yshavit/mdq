use crate::tree::{
    Block, BlockQuote, CodeBlock, Container, Inline, LeafBlock, List, ListItem, MdElem, Paragraph, Section, Table,
};

/// An MdqNodeRef is a slice into an MdqNode tree, where each element can be outputted, and certain elements can be
/// selected.
#[derive(Debug, Clone)]
pub enum MdElemRef<'a> {
    Section(&'a Section),
    ListItem(ListItemRef<'a>),

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
    Inline(&'a Inline),
}

#[derive(Debug, Clone, Copy)]
pub struct ListItemRef<'a>(pub Option<u32>, pub &'a ListItem);

/// We implement this one specifically because it's non-trivial: some Inlines are selectable, and some aren't.
impl<'a> From<&'a Inline> for MdElemRef<'a> {
    fn from(value: &'a Inline) -> Self {
        // TODO actually make this be non-trivial, as the doc says it will be. :-) Specifically, make Link and Image
        // selectable.
        MdElemRef::NonSelectable(NonSelectable::Inline(value))
    }
}

impl<'a> From<&'a MdElem> for MdElemRef<'a> {
    fn from(value: &'a MdElem) -> Self {
        match value {
            MdElem::Block(block) => match block {
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
            MdElem::Inline(v) => v.into(),
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
