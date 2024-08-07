use crate::tree::{
    BlockQuote, CodeBlock, Image, Inline, Line, Link, List, ListItem, MdElem, Paragraph, Section, Table,
};
use crate::vec_utils::{IndexKeeper, ItemRetainer};
use markdown::mdast;

/// An MdqNodeRef is a slice into an MdqNode tree, where each element can be outputted, and certain elements can be
/// selected.
#[derive(Debug, Clone, PartialEq)]
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
    TableSlice(TableSlice<'a>),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ListItemRef<'a>(pub Option<u32>, pub &'a ListItem);

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct HtmlRef<'a>(pub &'a String);

#[derive(Debug, Clone, PartialEq)]
pub struct TableSlice<'a> {
    alignments: Vec<mdast::AlignKind>,
    rows: Vec<TableRowSlice<'a>>,
}

pub type TableRowSlice<'a> = Vec<Option<&'a Line>>;

impl<'a> From<&'a Table> for TableSlice<'a> {
    fn from(table: &'a Table) -> Self {
        let alignments = table.alignments.clone();
        let mut rows = Vec::with_capacity(table.rows.len());
        for table_row in &table.rows {
            let cols: Vec<_> = table_row.iter().map(Some).collect();
            rows.push(cols);
        }
        Self { alignments, rows }
    }
}

impl<'a> TableSlice<'a> {
    pub fn alignments(&self) -> &Vec<mdast::AlignKind> {
        &self.alignments
    }

    pub fn rows(&self) -> impl Iterator<Item = &TableRowSlice<'a>> {
        self.rows.iter()
    }

    /// Normalizes this slice, so that every row has the same number of columns.
    ///
    /// If the table is jagged, all jagged rows will be filled in with [None] cells. Any missing
    /// alignments will be filled in as `None`.
    /// This is a departure from the Markdown standard, which specifies that the first row defines
    /// the number of rows, and extras are discarded.
    pub fn normalize(&mut self) {
        let max_cols = self.rows.iter().map(Vec::len).max().unwrap_or(0);

        for row in &mut self.rows {
            let n_missing = max_cols - row.len();
            for _ in 0..n_missing {
                row.push(None);
            }
        }
        if self.alignments.len() > max_cols {
            self.alignments.truncate(max_cols);
        } else {
            let nones = [mdast::AlignKind::None]
                .iter()
                .cycle()
                .take(max_cols - self.alignments.len());
            self.alignments.extend(nones);
        }
    }

    pub fn retain_columns_by_header<F>(&mut self, mut f: F)
    where
        F: FnMut(&Line) -> bool,
    {
        let Some(first_row) = self.rows.first() else {
            return;
        };
        let mut keeper_indices = IndexKeeper::new();
        keeper_indices.retain_when(first_row, |_, opt_cell| {
            let empty_cell = Line::new();
            let resolved_cell = opt_cell.unwrap_or(&empty_cell);
            f(resolved_cell)
        });

        match keeper_indices.count_keeps() {
            0 => {
                // no columns match: clear everything out
                self.alignments.clear();
                self.rows.clear();
                return;
            }
            n if n == first_row.len() => {
                // all columns match: no need to go one by one, just return without modifications
                return;
            }
            _ => {
                // some columns match: retain those, and discard the rest
                self.alignments.retain_with_index(keeper_indices.retain_fn());
                for row in self.rows.iter_mut() {
                    row.retain_with_index(keeper_indices.retain_fn());
                }
            }
        }
    }

    pub fn retain_rows<F>(&mut self, mut f: F)
    where
        F: FnMut(&Line) -> bool,
    {
        self.rows.retain_with_index(|idx, row| {
            if idx == 0 {
                return true;
            }
            row.iter().any(|opt_cell| {
                let empty_cell = Line::new();
                let resolved_cell = opt_cell.unwrap_or(&empty_cell);
                f(resolved_cell)
            })
        });
    }

    pub fn is_empty(&self) -> bool {
        // We always keep the first row; but if we then removed all the other rows, this TableSlice is empty.
        if self.rows.len() <= 1 {
            return true;
        }
        self.rows.iter().all(Vec::is_empty)
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

// TODO do I need all these explicit 'a lifetimes? I think I can do '_
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

impl<'a> From<&'a Table> for MdElemRef<'a> {
    fn from(value: &'a Table) -> Self {
        MdElemRef::Table(value)
    }
}

impl<'a> From<TableSlice<'a>> for MdElemRef<'a> {
    fn from(value: TableSlice<'a>) -> Self {
        MdElemRef::TableSlice(value)
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
            let slice = TableSlice::from(&table);
            assert_eq!(slice.alignments, vec![mdast::AlignKind::Left, mdast::AlignKind::Right]);
            assert_eq!(
                slice.rows,
                vec![
                    vec![Some(&cell("header a")), Some(&cell("header b"))],
                    vec![Some(&cell("data 1 a")), Some(&cell("data 1 b"))],
                    vec![Some(&cell("data 2 a")), Some(&cell("data 2 b"))],
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
            {
                let plain_slice = TableSlice::from(&table);
                assert_eq!(
                    plain_slice.alignments,
                    vec![mdast::AlignKind::Left, mdast::AlignKind::Right]
                );
                assert_eq!(
                    plain_slice.rows,
                    vec![
                        vec![Some(&cell("header a")), Some(&cell("header b"))],
                        vec![Some(&cell("data 1 a"))],
                        vec![
                            Some(&cell("data 2 a")),
                            Some(&cell("data 2 b")),
                            Some(&cell("data 2 c"))
                        ],
                    ]
                );
            }
            {
                let mut normalized_slice = TableSlice::from(&table);
                normalized_slice.normalize();
                assert_eq!(
                    normalized_slice.alignments,
                    vec![mdast::AlignKind::Left, mdast::AlignKind::Right, mdast::AlignKind::None]
                );
                assert_eq!(
                    normalized_slice.rows,
                    vec![
                        vec![Some(&cell("header a")), Some(&cell("header b")), None],
                        vec![Some(&cell("data 1 a")), None, None],
                        vec![
                            Some(&cell("data 2 a")),
                            Some(&cell("data 2 b")),
                            Some(&cell("data 2 c"))
                        ],
                    ]
                );
            }
        }

        #[test]
        fn retain_col() {
            let table = new_table(vec![
                vec!["KEEPER a", "header b", "header c"],
                vec!["data 1 a", "data 1 b", "data 1 c"],
                vec!["data 2 a", "data 2 b", "KEEPER c"],
            ]);
            let mut slice = TableSlice::from(&table);
            slice.retain_columns_by_header(cell_matches("KEEPER"));

            // note: "KEEPER" is in the last column, but not in the header; only the header gets
            // matched.
            assert_eq!(slice.alignments, vec![mdast::AlignKind::Left]);
            assert_eq!(
                slice.rows,
                vec![
                    vec![Some(&cell("KEEPER a"))],
                    vec![Some(&cell("data 1 a"))],
                    vec![Some(&cell("data 2 a"))],
                ]
            );
        }

        #[test]
        fn retain_all_columns_on_jagged_normalized_table() {
            let table = new_table(vec![
                vec!["header a", "header b"],
                vec!["data 1 a", "data 1 b", "data 1 c"],
                vec!["data 2 a"],
            ]);
            let mut slice = TableSlice::from(&table);
            slice.normalize();

            let mut seen_lines = Vec::with_capacity(3);
            slice.retain_columns_by_header(|line| {
                seen_lines.push(simple_to_string(line));
                true
            });

            // normalization
            assert_eq!(
                slice.alignments,
                vec![mdast::AlignKind::Left, mdast::AlignKind::Right, mdast::AlignKind::None]
            );
            assert_eq!(
                slice.rows,
                vec![
                    vec![Some(&cell("header a")), Some(&cell("header b")), None],
                    vec![
                        Some(&cell("data 1 a")),
                        Some(&cell("data 1 b")),
                        Some(&cell("data 1 c"))
                    ],
                    vec![Some(&cell("data 2 a")), None, None],
                ]
            );
            assert_eq!(
                seen_lines,
                vec!["header a".to_string(), "header b".to_string(), "".to_string(),],
            );
        }

        #[test]
        fn retain_row() {
            let table = new_table(vec![
                vec!["header a", "header b", "header c"],
                vec!["data 1 a", "data 1 b", "data 1 c"],
                vec!["data 2 a", "KEEPER b", "data 2 c"],
            ]);
            let mut slice = TableSlice::from(&table);
            slice.retain_rows(cell_matches("KEEPER"));

            assert_eq!(
                slice.alignments,
                vec![
                    mdast::AlignKind::Left,
                    mdast::AlignKind::Right,
                    mdast::AlignKind::Center,
                ]
            );
            // note: header row always gets kept
            assert_eq!(
                slice.rows,
                vec![
                    vec![
                        Some(&cell("header a")),
                        Some(&cell("header b")),
                        Some(&cell("header c"))
                    ],
                    vec![
                        Some(&cell("data 2 a")),
                        Some(&cell("KEEPER b")),
                        Some(&cell("data 2 c"))
                    ],
                ]
            );
        }

        #[test]
        fn retain_rows_on_jagged_normalized_table() {
            let table = new_table(vec![
                vec!["header a", "header b"],
                vec!["data 1 a", "data 1 b", "data 1 c"],
                vec!["data 2 a"],
            ]);
            let mut slice = TableSlice::from(&table);
            slice.normalize();

            let mut seen_lines = Vec::with_capacity(3);
            // retain only the rows with empty cells. This lets us get around the short-circuiting
            // of retain_rows (it short-circuits within each row as soon as it finds a matching
            // cell), to validate that the normalization works as expected.
            slice.retain_rows(|line| {
                seen_lines.push(simple_to_string(line));
                line.is_empty()
            });

            // normalization
            assert_eq!(
                slice.alignments,
                vec![mdast::AlignKind::Left, mdast::AlignKind::Right, mdast::AlignKind::None]
            );
            assert_eq!(
                slice.rows,
                vec![
                    vec![Some(&cell("header a")), Some(&cell("header b")), None],
                    vec![Some(&cell("data 2 a")), None, None],
                ]
            );
            assert_eq!(
                seen_lines,
                vec![
                    // header row gets skipped, since it's always retained
                    // second row:
                    "data 1 a".to_string(),
                    "data 1 b".to_string(),
                    "data 1 c".to_string(),
                    // third row: note that the 2nd cell short-circuits the row, so there is no 3rd
                    "data 2 a".to_string(),
                    "".to_string(),
                ],
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

        fn simple_to_string(line: &Line) -> String {
            let mut result = String::with_capacity(32);
            for segment in line {
                match segment {
                    Inline::Text(Text { variant, value }) if variant == &TextVariant::Plain => {
                        result.push_str(value);
                    }
                    _ => {
                        panic!("test error: unimplemented inline segment in simple_to_string");
                    }
                }
            }
            result
        }
    }
}
