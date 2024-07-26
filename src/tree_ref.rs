use crate::tree::{
    BlockQuote, CodeBlock, Image, Inline, Line, Link, List, ListItem, MdElem, Paragraph, Section, Table,
};
use crate::vec_utils::{IndexKeeper, ItemRetainer};
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

            // Maybe for jagged rows, we should extend the header (and other columns) to an implicit cell, with empty
            // contents. That way, a * matcher will match them, but anything that requires at least one char won't.
            // So, if you want a normalized table: " :-: . :-:". Though that will also remove cols that are aligned
            // but whose header is empty.
            // Maybe I should just always be strict: truncate extra cells, fill missing ones with empty
            rows.push(cols);
        }
        Some(Self { alignments, rows })
    }

    pub fn retain_columns<F>(mut self, f: F) -> Option<Self>
    where
        F: Fn(&Line) -> bool,
    {
        let first_row = self.rows.first()?;
        let mut keeper_indices = IndexKeeper::new();
        for row in &self.rows {
            keeper_indices.retain_when(row, |_, &line| f(line));
        }

        match keeper_indices.count_keeps() {
            0 => return None,
            n if n == first_row.len() => return Some(self),
            _ => {}
        }

        self.alignments.retain_with_index(keeper_indices.retain_fn());
        for row in self.rows.iter_mut() {
            row.retain_with_index(keeper_indices.retain_fn());
        }
        Some(self)
    }

    pub fn retain_rows<F>(mut self, f: F) -> Option<Self>
    where
        F: Fn(&Line) -> bool,
    {
        self.rows
            .retain_with_index(|idx, row| idx == 0 || row.iter().any(|&col| f(col)));

        // We always keep the first row; but if we then removed all the other rows, this TableSlice is empty.
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

#[cfg(test)]
mod tests {
    mod tables {
        use crate::tree::{Inline, Line, Table, Text, TextVariant};
        use crate::tree_ref::TableSlice;
        use markdown::mdast;

        #[test]
        fn table_slice_from_table() {
            let table = new_table(vec![
                vec!["header a", "header b"],
                vec!["data 1 a", "data 1 b"],
                vec!["data 2 a", "data 2 b"],
            ]);
            let slice = TableSlice::from_table(&table).expect("expected Some(TableSlice)");
            assert_eq!(slice.alignments, vec![mdast::AlignKind::Left, mdast::AlignKind::Right]);
            assert_eq!(
                slice.rows,
                vec![
                    vec![&cell("header a"), &cell("header b")],
                    vec![&cell("data 1 a"), &cell("data 1 b")],
                    vec![&cell("data 2 a"), &cell("data 2 b")],
                ]
            );
        }

        #[test]
        fn table_slice_from_table_jagged() {
            let table = new_table(vec![
                vec!["header a", "header b"],
                vec!["data 1 a"],
                vec!["data 2 a", "data 2 b", "data 2 c"],
            ]);
            let slice = TableSlice::from_table(&table).expect("expected Some(TableSlice)");
            assert_eq!(slice.alignments, vec![mdast::AlignKind::Left, mdast::AlignKind::Right]);
            assert_eq!(
                slice.rows,
                vec![
                    vec![&cell("header a"), &cell("header b")],
                    vec![&cell("data 1 a"), &vec![]],
                    vec![&cell("data 2 a"), &cell("data 2 b")],
                ]
            );
        }

        #[test]
        fn retain_col() {
            let table = new_table(vec![
                vec!["KEEPER a", "header b", "header c"],
                vec!["data 1 a", "data 1 b", "data 1 c"],
                vec!["data 2 a", "data 2 b", "KEEPER c"],
            ]);
            let slice = TableSlice::from_table(&table).expect("expected Some(TableSlice)");
            let slice = slice
                .retain_columns(cell_matches("KEEPER"))
                .expect("expected Some(TableSlice)");

            assert_eq!(slice.alignments, vec![mdast::AlignKind::Left, mdast::AlignKind::Center]);
            // note: The "header c" column also contains "KEEPER", but not in the header; only the header counts.
            assert_eq!(
                slice.rows,
                vec![
                    vec![&cell("KEEPER a")],
                    vec![&cell("data 1 a")],
                    vec![&cell("data 2 a")],
                ]
            );
        }

        #[test]
        fn retain_row() {
            let table = new_table(vec![
                vec!["header a", "header b", "header c"],
                vec!["data 1 a", "data 1 b", "data 1 c"],
                vec!["data 2 a", "KEEPER b", "data 2 c"],
            ]);
            let slice = TableSlice::from_table(&table).expect("expected Some(TableSlice)");
            let slice = slice
                .retain_rows(cell_matches("KEEPER"))
                .expect("expected Some(TableSlice)");

            assert_eq!(
                slice.alignments,
                vec![
                    mdast::AlignKind::Left,
                    mdast::AlignKind::Right,
                    mdast::AlignKind::Center
                ]
            );
            // note: header row always gets kept
            assert_eq!(
                slice.rows,
                vec![
                    vec![&cell("header a"), &cell("header b"), &cell("header c")],
                    vec![&cell("data 2 a"), &cell("KEEPER b"), &cell("data 2 c")],
                ]
            );
        }

        fn cell_matches(substring: &str) -> impl Fn(&Line) -> bool + '_ {
            move |line| {
                let line_str = format!("{:?}", line);
                line_str.contains(substring)
            }
        }

        fn new_table(cells: Vec<Vec<&str>>) -> Table {
            let mut rows_iter = cells.iter().peekable();
            let Some(first_row) = rows_iter.peek() else {
                return Table {
                    alignments: vec![],
                    rows: vec![],
                };
            };

            // for alignments, just cycle [L, R, C].
            let alignments = [
                mdast::AlignKind::Left,
                mdast::AlignKind::Right,
                mdast::AlignKind::Center,
            ]
            .iter()
            .cycle()
            .take(first_row.len())
            .map(ToOwned::to_owned)
            .collect();
            let mut rows = Vec::with_capacity(cells.len());

            while let Some(row_strings) = rows_iter.next() {
                let mut row = Vec::with_capacity(row_strings.len());
                for cell_string in row_strings {
                    row.push(cell(cell_string));
                }
                rows.push(row);
            }

            Table { alignments, rows }
        }

        fn cell(value: &str) -> Line {
            vec![Inline::Text(Text {
                variant: TextVariant::Plain,
                value: value.to_string(),
            })]
        }
    }
}
