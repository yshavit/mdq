use crate::tree::{BlockQuote, CodeBlock, Inline, List, ListItem, MdqNode, Paragraph, Section, Table};

pub enum MdqNodeRef<'a> {
    // paragraphs with child nodes
    Section(&'a Section),
    Paragraph(&'a Paragraph),
    BlockQuote(&'a BlockQuote),
    List(&'a List),
    Table(&'a Table),

    ThematicBreak,

    // blocks that contain strings (&'a as opposed to nodes)
    CodeBlock(&'a CodeBlock),

    ListItem(Option<u32>, &'a ListItem),

    // inline spans
    Inline(&'a Inline),
}

impl<'a> From<&'a MdqNode> for MdqNodeRef<'a> {
    fn from(value: &'a MdqNode) -> Self {
        match value {
            MdqNode::Section(v) => Self::Section(v),
            MdqNode::Paragraph(v) => Self::Paragraph(v),
            MdqNode::BlockQuote(v) => Self::BlockQuote(v),
            MdqNode::List(v) => Self::List(v),
            MdqNode::Table(v) => Self::Table(v),
            MdqNode::ThematicBreak => Self::ThematicBreak,
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
