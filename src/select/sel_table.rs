use crate::md_elem::elem::Table;
use crate::md_elem::*;
use crate::select::string_matcher::StringMatcher;
use crate::select::{TableMatcher, TrySelector};

#[derive(Debug, PartialEq)]
pub struct TableSelector {
    headers_matcher: StringMatcher,
    rows_matcher: StringMatcher,
}

impl TrySelector<Table> for TableSelector {
    fn try_select(&self, _: &MdContext, orig: Table) -> Result<Vec<MdElem>, MdElem> {
        let mut table = orig.clone();

        table.normalize();

        table.retain_columns_by_header(|line| self.headers_matcher.matches_inlines(line));
        if table.is_empty() {
            return Err(orig.into());
        }

        table.retain_rows(|line| self.rows_matcher.matches_inlines(line));
        if table.is_empty() {
            return Err(orig.into());
        }
        Ok(vec![table.into()])
    }
}

impl From<TableMatcher> for TableSelector {
    fn from(value: TableMatcher) -> Self {
        Self {
            headers_matcher: value.headers.into(),
            rows_matcher: value.rows.into(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::md_elem::elem::*;
    use crate::select::TrySelector;
    use crate::util::utils_for_test::*;

    #[test]
    fn select_all_on_normalized_table() {
        let table: Table = Table {
            alignments: vec![Some(ColumnAlignment::Left), Some(ColumnAlignment::Right)],
            rows: vec![
                vec![cell("header a"), cell("header b")],
                vec![cell("data 1 a"), cell("data 1 b")],
            ],
        };
        let maybe_selected = TableSelector {
            headers_matcher: ".*".into(),
            rows_matcher: ".*".into(),
        }
        .try_select(&MdContext::empty(), table)
        .map(get_only);

        unwrap!(maybe_selected, Ok(MdElem::Table(table)));
        assert_eq!(
            table.alignments(),
            &vec![Some(ColumnAlignment::Left), Some(ColumnAlignment::Right)]
        );
        assert_eq!(
            table.rows(),
            &vec![
                vec![cell("header a"), cell("header b")],
                vec![cell("data 1 a"), cell("data 1 b")],
            ]
        );
    }

    #[test]
    fn select_columns_on_normalized_table() {
        let table: Table = Table {
            alignments: vec![Some(ColumnAlignment::Left), Some(ColumnAlignment::Right)],
            rows: vec![
                vec![cell("header a"), cell("KEEP b")],
                vec![cell("data 1 a"), cell("data 1 b")],
            ],
        };
        let maybe_selected = TableSelector {
            headers_matcher: "KEEP".into(),
            rows_matcher: ".*".into(),
        }
        .try_select(&MdContext::empty(), table)
        .map(get_only);

        unwrap!(maybe_selected, Ok(MdElem::Table(table)));
        assert_eq!(table.alignments(), &vec![Some(ColumnAlignment::Right)]);
        assert_eq!(table.rows(), &vec![vec![cell("KEEP b")], vec![cell("data 1 b")],]);
    }

    #[test]
    fn select_rows_on_normalized_table() {
        let table: Table = Table {
            alignments: vec![Some(ColumnAlignment::Left), Some(ColumnAlignment::Right)],
            rows: vec![
                vec![cell("header a"), cell("header b")],
                vec![cell("data 1 a"), cell("data 1 b")],
                vec![cell("data 2 a"), cell("data 2 b")],
            ],
        };
        let maybe_selected = TableSelector {
            headers_matcher: ".*".into(),
            rows_matcher: "data 2".into(),
        }
        .try_select(&MdContext::empty(), table)
        .map(get_only);

        unwrap!(maybe_selected, Ok(MdElem::Table(table)));
        assert_eq!(
            table.alignments(),
            &vec![Some(ColumnAlignment::Left), Some(ColumnAlignment::Right)]
        );
        assert_eq!(
            table.rows(),
            &vec![
                // note: header always gets retained
                vec![cell("header a"), cell("header b")],
                vec![cell("data 2 a"), cell("data 2 b")],
            ]
        );
    }

    /// Tests (a) that the table gets normalized, and (b) a smoke test of the matchers.
    /// More extensive tests for the `retain_*` methods can be found in [TableSlice]'s tests.
    #[test]
    fn jagged_table() {
        let table: Table = Table {
            // only 1 align; rest will be filled with None
            alignments: vec![Some(ColumnAlignment::Left)],
            rows: vec![
                vec![cell("header a")],
                vec![cell("data 1 a"), cell("data 1 b")],
                vec![cell("data 2 a"), cell("data 2 b"), cell("data 2 c")],
            ],
        };
        let maybe_selected = TableSelector {
            headers_matcher: ".*".into(),
            rows_matcher: "data 1".into(),
        }
        .try_select(&MdContext::empty(), table)
        .map(get_only);

        unwrap!(maybe_selected, Ok(MdElem::Table(table)));
        assert_eq!(table.alignments(), &vec![Some(ColumnAlignment::Left), None, None]);
        assert_eq!(
            table.rows(),
            &vec![
                vec![cell("header a"), Vec::new(), Vec::new()],
                vec![cell("data 1 a"), cell("data 1 b"), Vec::new()],
            ]
        );
    }

    fn cell(cell_str: &str) -> TableCell {
        vec![Inline::Text(Text {
            variant: TextVariant::Plain,
            value: cell_str.to_string(),
        })]
    }
}
