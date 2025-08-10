use crate::md_elem::elem::{Table, TableCell, TableRow};
use crate::md_elem::inline_regex_replace::Replaced;
use crate::md_elem::*;
use crate::select::string_matcher::StringMatcher;
use crate::select::{Result, Select, TableMatcher, TrySelector};

#[derive(Debug, PartialEq)]
pub(crate) struct TableSelector {
    headers_matcher: StringMatcher,
    rows_matcher: StringMatcher,
}

impl TrySelector<Table> for TableSelector {
    fn try_select(&self, _: &MdContext, orig: Table) -> Result<Select> {
        let mut table = orig.clone();

        table.normalize();

        let selected = match self.replace_table(table)? {
            None => Select::Miss(orig.into()),
            Some(replaced) => Select::Hit(vec![replaced.into()]),
        };
        Ok(selected)
    }
}

type ReplacementCell = Replaced<TableCell>;

struct ReplacementRow {
    row: Vec<ReplacementCell>,
}

impl ReplacementRow {
    fn any_cell_matched(&self) -> bool {
        self.row.iter().any(|cell| cell.matched_any)
    }

    fn to_row(self, allowed_cells: &[bool]) -> TableRow {
        self.row
            .into_iter()
            .enumerate()
            .filter_map(|(idx, cell)| {
                if allowed_cells.get(idx).copied().unwrap_or(false) {
                    Some(cell.item)
                } else {
                    None
                }
            })
            .collect()
    }
}

impl TableSelector {
    fn replace_table(&self, table: Table) -> Result<Option<Table>> {
        let row_count = table.rows.len();
        let mut rows_iter = table.rows.into_iter();

        let Some(header_row) = rows_iter.next() else {
            return Ok(Some(Table {
                alignments: table.alignments,
                rows: vec![],
            }));
        };
        let header_replacement = Self::replace_row(&self.headers_matcher, header_row)?;
        if !header_replacement.any_cell_matched() {
            return Ok(None);
        }

        let mut rows = Vec::with_capacity(row_count);

        let indexes_to_keep: Vec<_> = header_replacement.row.iter().map(|item| item.matched_any).collect();
        rows.push(header_replacement.to_row(&indexes_to_keep));

        let alignments = table
            .alignments
            .into_iter()
            .enumerate()
            .filter_map(|(idx, item)| {
                if indexes_to_keep.get(idx).copied().unwrap_or(false) {
                    Some(item)
                } else {
                    None
                }
            })
            .collect();

        if rows_iter.len() == 0 {
            // No content rows, so just return a hit of the header rows. We don't expect this to happen, though.
            return Ok(Some(Table { alignments, rows }));
        }
        while let Some(row) = rows_iter.next() {
            let replaced_row = Self::replace_row(&self.rows_matcher, row)?;
            if replaced_row.any_cell_matched() {
                let replaced_row = replaced_row.to_row(&indexes_to_keep);
                rows.push(replaced_row);
            }
        }

        if rows.len() == 1 {
            // header always matches, but do we have any content rows?
            return Ok(None);
        }

        Ok(Some(Table { alignments, rows }))
    }

    fn replace_row(matcher: &StringMatcher, row: TableRow) -> Result<ReplacementRow> {
        let replacement_cells: Vec<ReplacementCell> = row
            .into_iter()
            .map(|cell| matcher.match_replace_inlines(cell))
            .collect::<core::result::Result<Vec<_>, _>>()
            .map_err(|e| e.to_select_error("table"))?;
        Ok(ReplacementRow { row: replacement_cells })
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
    use crate::select::{MatchReplace, TrySelector};
    use crate::util::utils_for_test::*;

    #[test]
    fn regex_matches_all_on_normalized_table() {
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
        .map(|selection| match selection {
            Select::Hit(elems) => get_only(elems),
            Select::Miss(_) => panic!("Expected selection to succeed"),
        });

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
    fn regex_matches_columns_on_normalized_table() {
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
        .map(|selection| match selection {
            Select::Hit(elems) => get_only(elems),
            Select::Miss(_) => panic!("Expected selection to succeed"),
        });

        unwrap!(maybe_selected, Ok(MdElem::Table(table)));
        assert_eq!(table.alignments(), &vec![Some(ColumnAlignment::Right)]);
        assert_eq!(table.rows(), &vec![vec![cell("KEEP b")], vec![cell("data 1 b")],]);
    }

    #[test]
    fn regex_matches_rows_on_normalized_table() {
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
        .map(|selection| match selection {
            Select::Hit(elems) => get_only(elems),
            Select::Miss(_) => panic!("Expected selection to succeed"),
        });

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
        .map(|selection| match selection {
            Select::Hit(elems) => get_only(elems),
            Select::Miss(_) => panic!("Expected selection to succeed"),
        });

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

    #[test]
    fn matcher_matches_no_columns() {
        let table: Table = Table {
            alignments: vec![Some(ColumnAlignment::Left), Some(ColumnAlignment::Right)],
            rows: vec![
                vec![cell("header a"), cell("header b")],
                vec![cell("data 1 a"), cell("data 1 b")],
                vec![cell("data 2 a"), cell("data 2 b")],
            ],
        };
        let selection = TableSelector {
            headers_matcher: "NOMATCH".into(), // This won't match any headers
            rows_matcher: ".*".into(),
        }
        .try_select(&MdContext::empty(), table.clone())
        .unwrap();

        unwrap!(selection, Select::Miss(MdElem::Table(returned_table)));
        // Should return the original table unchanged
        assert_eq!(returned_table, table);
    }

    #[test]
    fn matcher_matches_no_rows() {
        let table: Table = Table {
            alignments: vec![Some(ColumnAlignment::Left), Some(ColumnAlignment::Right)],
            rows: vec![
                vec![cell("header a"), cell("header b")],
                vec![cell("data 1 a"), cell("data 1 b")],
                vec![cell("data 2 a"), cell("data 2 b")],
            ],
        };
        let selection = TableSelector {
            headers_matcher: ".*".into(),   // This matches all headers
            rows_matcher: "NOMATCH".into(), // This won't match any data rows
        }
        .try_select(&MdContext::empty(), table.clone())
        .unwrap();

        unwrap!(selection, Select::Miss(MdElem::Table(returned_table)));
        // Should return the original table unchanged
        assert_eq!(returned_table, table);
    }

    #[test]
    fn regex_replace_on_column_header() {
        let table: Table = Table {
            alignments: vec![Some(ColumnAlignment::Left), Some(ColumnAlignment::Right)],
            rows: vec![
                vec![cell("header a"), cell("header b")],
                vec![cell("data 1 a"), cell("data 1 b")],
                vec![cell("data 2 a"), cell("data 2 b")],
            ],
        };
        let selection = TableSelector::from(TableMatcher {
            headers: MatchReplace::build(|b| b.match_regex(" *header *").replacement("")),
            rows: MatchReplace::match_any(),
        })
        .try_select(&MdContext::empty(), table)
        .unwrap();

        unwrap!(selection, Select::Hit(elems));
        let table_elem = get_only(elems);
        unwrap!(table_elem, MdElem::Table(result_table));

        // Should replace "old header" with "new header"
        assert_eq!(
            result_table.rows(),
            &vec![
                vec![cell("a"), cell("b")],
                vec![cell("data 1 a"), cell("data 1 b")],
                vec![cell("data 2 a"), cell("data 2 b")],
            ]
        );
    }

    #[test]
    fn regex_replace_on_row() {
        let table: Table = Table {
            alignments: vec![Some(ColumnAlignment::Left), Some(ColumnAlignment::Right)],
            rows: vec![
                vec![cell("header a"), cell("header b")],
                vec![cell("old data"), cell("data 1 b")],
                vec![cell("data 2 a"), cell("old data")],
            ],
        };
        let selection = TableSelector::from(TableMatcher {
            headers: MatchReplace::match_any(),
            rows: MatchReplace::build(|b| b.match_regex("old").replacement("new")),
        })
        .try_select(&MdContext::empty(), table)
        .unwrap();

        unwrap!(selection, Select::Hit(elems));
        let table_elem = get_only(elems);
        unwrap!(table_elem, MdElem::Table(result_table));

        // Should replace "old data" with "new data" in rows
        assert_eq!(
            result_table.rows(),
            &vec![
                vec![cell("header a"), cell("header b")],
                vec![cell("new data"), cell("data 1 b")],
                vec![cell("data 2 a"), cell("new data")],
            ]
        );
    }

    #[test]
    fn regex_replace_on_both_header_and_row() {
        let table: Table = Table {
            alignments: vec![Some(ColumnAlignment::Left), Some(ColumnAlignment::Right)],
            rows: vec![
                vec![cell("header a"), cell("header b")],
                vec![cell("old data"), cell("data 1 b")],
                vec![cell("data 2 a"), cell("old data")],
            ],
        };
        let selection = TableSelector::from(TableMatcher {
            headers: MatchReplace::build(|b| b.match_regex("header (.*)").replacement(">$1<")),
            rows: MatchReplace::build(|b| b.match_regex("old").replacement("new")),
        })
        .try_select(&MdContext::empty(), table)
        .unwrap();

        unwrap!(selection, Select::Hit(elems));
        let table_elem = get_only(elems);
        unwrap!(table_elem, MdElem::Table(result_table));

        // Should replace "old" with "NEW" in headers and "new" in rows
        assert_eq!(
            result_table.rows(),
            &vec![
                vec![cell(">a<"), cell(">b<")],
                vec![cell("new data"), cell("data 1 b")],
                vec![cell("data 2 a"), cell("new data")],
            ]
        );
    }

    #[test]
    fn regex_replace_on_column_header_no_matches() {
        let table: Table = Table {
            alignments: vec![Some(ColumnAlignment::Left), Some(ColumnAlignment::Right)],
            rows: vec![
                vec![cell("header a"), cell("header b")],
                vec![cell("data 1 a"), cell("data 1 b")],
                vec![cell("data 2 a"), cell("data 2 b")],
            ],
        };
        let selection = TableSelector::from(TableMatcher {
            headers: MatchReplace::build(|b| b.match_regex("NOMATCH").replacement("replacement")),
            rows: MatchReplace::match_any(),
        })
        .try_select(&MdContext::empty(), table.clone())
        .unwrap();

        // Should return Miss since no headers match the pattern
        unwrap!(selection, Select::Miss(MdElem::Table(returned_table)));
        assert_eq!(returned_table, table);
    }

    #[test]
    fn regex_replace_on_row_no_matches() {
        let table: Table = Table {
            alignments: vec![Some(ColumnAlignment::Left), Some(ColumnAlignment::Right)],
            rows: vec![
                vec![cell("header a"), cell("header b")],
                vec![cell("data 1 a"), cell("data 1 b")],
                vec![cell("data 2 a"), cell("data 2 b")],
            ],
        };
        let selection = TableSelector::from(TableMatcher {
            headers: MatchReplace::match_any(),
            rows: MatchReplace::build(|b| b.match_regex("NOMATCH").replacement("replacement")),
        })
        .try_select(&MdContext::empty(), table.clone())
        .unwrap();

        // Should return Miss since no rows match the pattern
        unwrap!(selection, Select::Miss(MdElem::Table(returned_table)));
        assert_eq!(returned_table, table);
    }

    fn cell(cell_str: &str) -> TableCell {
        vec![Inline::Text(Text {
            variant: TextVariant::Plain,
            value: cell_str.to_string(),
        })]
    }
}
