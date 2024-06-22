use crate::tree::{
    Block, BlockQuote, CodeBlock, Inline, LeafBlock, List, ListItem, MdqNode, Paragraph, Section, Table,
};

#[derive(Debug, Clone)]
pub enum MdqNodeRef<'a> {
    // paragraphs with child nodes
    Section(&'a Section),
    Paragraph(&'a Paragraph),
    BlockQuote(&'a BlockQuote),
    List(&'a List),
    Table(&'a Table),

    // blocks that contain strings (&'a as opposed to nodes)
    CodeBlock(&'a CodeBlock),

    ListItem(ListItemRef<'a>),

    // inline spans
    Inline(&'a Inline),

    NonSelectable(NonSelectable),
}

#[derive(Debug, Clone)]
pub enum NonSelectable {
    ThematicBreak,
}

#[derive(Debug, Clone, Copy)]
pub struct ListItemRef<'a>(pub Option<u32>, pub &'a ListItem);

impl<'a> From<&'a MdqNode> for MdqNodeRef<'a> {
    fn from(value: &'a MdqNode) -> Self {
        match value {
            MdqNode::Block(block) => match block {
                Block::LeafBlock(leaf) => match leaf {
                    LeafBlock::ThematicBreak => Self::NonSelectable(NonSelectable::ThematicBreak),
                    LeafBlock::Paragraph(p) => Self::Paragraph(p),
                },
            },
            MdqNode::Section(v) => Self::Section(v),
            MdqNode::BlockQuote(v) => Self::BlockQuote(v),
            MdqNode::List(v) => Self::List(v),
            MdqNode::Table(v) => Self::Table(v),
            MdqNode::CodeBlock(v) => Self::CodeBlock(v),
            MdqNode::Inline(v) => Self::Inline(v),
        }
    }
}

#[macro_export]
macro_rules! wrap_mdq_refs {
    ($variant:ident: $source:expr) => {{
        let source = $source;
        let mut result: Vec<MdqNodeRef> = Vec::with_capacity(source.len());
        for elem in source {
            result.push(MdqNodeRef::$variant(elem));
        }
        result
    }};
}

impl<'a> MdqNodeRef<'a> {
    pub fn wrap_vec(source: &'a Vec<MdqNode>) -> Vec<Self> {
        let mut result: Vec<Self> = Vec::with_capacity(source.len());
        for elem in source {
            result.push(elem.into());
        }
        result
    }
}
