use crate::matcher::StringMatcher;
use crate::select::Selector;
use crate::tree_ref::{MdElemRef, TableSlice};

#[derive(Debug, PartialEq)]
pub struct TableSliceSelector {
    headers_matcher: StringMatcher,
    rows_matcher: StringMatcher,
}

impl<'md> Selector<'md, TableSlice<'md>> for TableSliceSelector {
    fn try_select(&self, slice: TableSlice<'md>) -> Option<MdElemRef<'md>> {
        let mut slice = slice.clone(); // GH #168 is there any way to avoid this? There may not be.
        slice.normalize();

        slice.retain_columns_by_header(|line| self.headers_matcher.matches_inlines(line));
        if slice.is_empty() {
            return None;
        }

        slice.retain_rows(|line| self.rows_matcher.matches_inlines(line));
        if slice.is_empty() {
            return None;
        }
        Some(slice.into())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::select::Selector;
    use crate::tree::{Inline, Line, Table, Text, TextVariant};
    use crate::unwrap;
    use markdown::mdast;

    #[test]
    fn select_all_on_normalized_table() {
        let table: Table = Table {
            alignments: vec![mdast::AlignKind::Left, mdast::AlignKind::Right],
            rows: vec![
                vec![cell("header a"), cell("header b")],
                vec![cell("data 1 a"), cell("data 1 b")],
            ],
        }
        .into();
        let maybe_selected = TableSliceSelector {
            headers_matcher: ".*".into(),
            rows_matcher: ".*".into(),
        }
        .try_select((&table).into());

        unwrap!(maybe_selected, Some(MdElemRef::TableSlice(table)));
        assert_eq!(
            table.alignments(),
            &vec![mdast::AlignKind::Left, mdast::AlignKind::Right]
        );
        assert_eq!(
            table.rows(),
            &vec![
                vec![Some(&cell("header a")), Some(&cell("header b"))],
                vec![Some(&cell("data 1 a")), Some(&cell("data 1 b"))],
            ]
        );
    }

    #[test]
    fn select_columns_on_normalized_table() {
        let table: Table = Table {
            alignments: vec![mdast::AlignKind::Left, mdast::AlignKind::Right],
            rows: vec![
                vec![cell("header a"), cell("KEEP b")],
                vec![cell("data 1 a"), cell("data 1 b")],
            ],
        };
        let maybe_selected = TableSliceSelector {
            headers_matcher: "KEEP".into(),
            rows_matcher: ".*".into(),
        }
        .try_select((&table).into());

        unwrap!(maybe_selected, Some(MdElemRef::TableSlice(table)));
        assert_eq!(table.alignments(), &vec![mdast::AlignKind::Right]);
        assert_eq!(
            table.rows(),
            &vec![vec![Some(&cell("KEEP b"))], vec![Some(&cell("data 1 b"))],]
        );
    }

    #[test]
    fn select_rows_on_normalized_table() {
        let table: Table = Table {
            alignments: vec![mdast::AlignKind::Left, mdast::AlignKind::Right],
            rows: vec![
                vec![cell("header a"), cell("header b")],
                vec![cell("data 1 a"), cell("data 1 b")],
                vec![cell("data 2 a"), cell("data 2 b")],
            ],
        };
        let maybe_selected = TableSliceSelector {
            headers_matcher: ".*".into(),
            rows_matcher: "data 2".into(),
        }
        .try_select((&table).into());

        unwrap!(maybe_selected, Some(MdElemRef::TableSlice(table)));
        assert_eq!(
            table.alignments(),
            &vec![mdast::AlignKind::Left, mdast::AlignKind::Right]
        );
        assert_eq!(
            table.rows(),
            &vec![
                // note: header always gets retained
                vec![Some(&cell("header a")), Some(&cell("header b"))],
                vec![Some(&cell("data 2 a")), Some(&cell("data 2 b"))],
            ]
        );
    }

    /// Tests (a) that the table gets normalized, and (b) a smoke test of the matchers.
    /// More extensive tests for the `retain_*` methods can be found in [TableSlice]'s tests.
    #[test]
    fn jagged_table() {
        let table: Table = Table {
            // only 1 align; rest will be filled with None
            alignments: vec![mdast::AlignKind::Left],
            rows: vec![
                vec![cell("header a")],
                vec![cell("data 1 a"), cell("data 1 b")],
                vec![cell("data 2 a"), cell("data 2 b"), cell("data 2 c")],
            ],
        };
        let maybe_selected = TableSliceSelector {
            headers_matcher: ".*".into(),
            rows_matcher: "data 1".into(),
        }
        .try_select((&table).into());

        unwrap!(maybe_selected, Some(MdElemRef::TableSlice(table)));
        assert_eq!(
            table.alignments(),
            &vec![mdast::AlignKind::Left, mdast::AlignKind::None, mdast::AlignKind::None]
        );
        assert_eq!(
            table.rows(),
            &vec![
                vec![Some(&cell("header a")), None, None],
                vec![Some(&cell("data 1 a")), Some(&cell("data 1 b")), None],
            ]
        );
    }

    fn cell(cell_str: &str) -> Line {
        vec![Inline::Text(Text {
            variant: TextVariant::Plain,
            value: cell_str.to_string(),
        })]
    }
}
