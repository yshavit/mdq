use crate::tree::{
    BlockQuote, CodeBlock, Image, Inline, Line, Link, List, ListItem, MdElem, Paragraph, Section, Table,
};
use crate::vec_utils::{IndexRemover, ItemRetainer};
use markdown::mdast;

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
    Html(HtmlRef<'a>),
    ThematicBreak,

    // sub-elements
    ListItem(ListItemRef<'a>),
    Link(&'a Link),
    Image(&'a Image),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ListItemRef<'a>(pub Option<u32>, pub &'a ListItem);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct HtmlRef<'a>(pub &'a String);

pub struct TableSlice<'a> {
    pub alignments: Vec<mdast::AlignKind>,
    pub rows: Vec<TableRowSlice<'a>>,
}

pub type TableRowSlice<'a> = Vec<&'a Line>;

impl<'a> TableSlice<'a> {
    pub fn from_table(table: &'a Table) -> Option<Self> {
        if table.alignments.is_empty() {
            return None;
        }
        if table.rows.len() < 2 {
            return None;
        }
        let alignments = table.alignments.clone();
        let mut rows = Vec::with_capacity(table.rows.len());
        for table_row in &table.rows {
            let cols: Vec<_> = table_row.iter().collect();
            // TODO unify jagged tables; these shouldn't happen, but let's guard against them anyway
            // TODO add unit tests to cover this. Or maybe we don't actually need to, and unit tests will show it
            // working without any additional checks? The remover should safely handle it all.
            rows.push(cols);
        }
        Some(Self { alignments, rows })
    }

    // TODO rename to "retain_columns"
    pub fn filter_columns<F>(mut self, f: F) -> Option<Self>
    where
        F: Fn(&Line) -> bool,
    {
        let first_row = self.rows.first()?;
        let removals = IndexRemover::for_items(first_row, |_, &i| f(i));

        if removals.count_removals() == 0 {
            return Some(self);
        }
        if removals.count_removals() == first_row.len() {
            // all columns filtered out!
            return None;
        }

        removals.apply(&mut self.alignments);
        for row in self.rows.iter_mut() {
            removals.apply(row);
        }
        Some(self)
    }

    // TODO rename to "retain_rows"
    pub fn filter_rows<F>(mut self, f: F) -> Option<Self>
    where
        F: Fn(&Line) -> bool,
    {
        self.rows
            .retain_if_with_index(|idx, row| if idx == 0 { true } else { row.iter().any(|&col| f(col)) });

        // We always keep the first row; but if we then removed all the other rows, this is empty.
        if self.rows.len() <= 1 {
            return None;
        }

        Some(self)
    }
}

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
            MdElem::Html(html) => MdElemRef::Html(HtmlRef(html)),
        }
    }
}

impl<'a> From<&'a BlockQuote> for MdElemRef<'a> {
    fn from(value: &'a BlockQuote) -> Self {
        MdElemRef::BlockQuote(value)
    }
}

impl<'a> From<&'a CodeBlock> for MdElemRef<'a> {
    fn from(value: &'a CodeBlock) -> Self {
        MdElemRef::CodeBlock(value)
    }
}

impl<'a> From<ListItemRef<'a>> for MdElemRef<'a> {
    fn from(value: ListItemRef<'a>) -> Self {
        MdElemRef::ListItem(value)
    }
}

impl<'a> From<HtmlRef<'a>> for MdElemRef<'a> {
    fn from(value: HtmlRef<'a>) -> Self {
        Self::Html(HtmlRef(value.0))
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

impl<'a> From<&'a Paragraph> for MdElemRef<'a> {
    fn from(value: &'a Paragraph) -> Self {
        MdElemRef::Paragraph(value)
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
