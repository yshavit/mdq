use crate::md_elem::elem::*;
use crate::util::vec_utils::ItemRetainer;

mod elem_ref {
    use super::*;
    use crate::util::vec_utils::IndexKeeper;

    impl Table {
        pub fn alignments(&self) -> &[Option<ColumnAlignment>] {
            &self.alignments
        }

        pub fn rows(&self) -> &Vec<TableRow> {
            &self.rows
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
                    row.push(Vec::new());
                }
            }
            if self.alignments.len() > max_cols {
                self.alignments.truncate(max_cols);
            } else {
                let nones = [None].iter().cycle().take(max_cols - self.alignments.len());
                self.alignments.extend(nones);
            }
        }

        pub fn retain_columns_by_header<F>(&mut self, mut f: F)
        where
            F: FnMut(&TableCell) -> bool,
        {
            let Some(first_row) = self.rows.first() else {
                return;
            };
            let mut keeper_indices = IndexKeeper::new();
            keeper_indices.retain_when(first_row, |_, cell| f(cell));

            match keeper_indices.count_keeps() {
                0 => {
                    // no columns match: clear everything out
                    self.alignments.clear();
                    self.rows.clear();
                }
                n if n == first_row.len() => {
                    // all columns match: no need to go one by one, just return without modifications
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
            F: FnMut(&TableCell) -> bool,
        {
            self.rows.retain_with_index(|idx, row| {
                if idx == 0 {
                    return true;
                }
                row.iter().any(&mut f)
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
}

#[cfg(test)]
mod tests {
    use super::*;

    mod tables {
        use super::*;

        #[test]
        fn retain_col() {
            let mut table = new_table(vec![
                vec!["KEEPER a", "header b", "header c"],
                vec!["data 1 a", "data 1 b", "data 1 c"],
                vec!["data 2 a", "data 2 b", "KEEPER c"],
            ]);
            table.retain_columns_by_header(cell_matches("KEEPER"));

            // note: "KEEPER" is in the last column, but not in the header; only the header gets
            // matched.
            assert_eq!(table.alignments, vec![Some(ColumnAlignment::Left)]);
            assert_eq!(
                table.rows,
                vec![vec![cell("KEEPER a")], vec![cell("data 1 a")], vec![cell("data 2 a")],]
            );
        }

        #[test]
        fn retain_all_columns_on_jagged_normalized_table() {
            let mut table = new_table(vec![
                vec!["header a", "header b"],
                vec!["data 1 a", "data 1 b", "data 1 c"],
                vec!["data 2 a"],
            ]);
            table.normalize();

            let mut seen_lines = Vec::with_capacity(3);
            table.retain_columns_by_header(|line| {
                seen_lines.push(simple_to_string(line));
                true
            });

            // normalization
            assert_eq!(
                table.alignments,
                vec![Some(ColumnAlignment::Left), Some(ColumnAlignment::Right), None]
            );
            assert_eq!(
                table.rows,
                vec![
                    vec![cell("header a"), cell("header b"), Vec::new()],
                    vec![cell("data 1 a"), cell("data 1 b"), cell("data 1 c")],
                    vec![cell("data 2 a"), Vec::new(), Vec::new()],
                ]
            );
            assert_eq!(
                seen_lines,
                vec!["header a".to_string(), "header b".to_string(), "".to_string(),],
            );
        }

        #[test]
        fn retain_row() {
            let mut table = new_table(vec![
                vec!["header a", "header b", "header c"],
                vec!["data 1 a", "data 1 b", "data 1 c"],
                vec!["data 2 a", "KEEPER b", "data 2 c"],
            ]);
            table.retain_rows(cell_matches("KEEPER"));

            assert_eq!(
                table.alignments,
                vec![
                    Some(ColumnAlignment::Left),
                    Some(ColumnAlignment::Right),
                    Some(ColumnAlignment::Center),
                ]
            );
            // note: header row always gets kept
            assert_eq!(
                table.rows,
                vec![
                    vec![cell("header a"), cell("header b"), cell("header c")],
                    vec![cell("data 2 a"), cell("KEEPER b"), cell("data 2 c")],
                ]
            );
        }

        #[test]
        fn retain_rows_on_jagged_normalized_table() {
            let mut table = new_table(vec![
                vec!["header a", "header b"],
                vec!["data 1 a", "data 1 b", "data 1 c"],
                vec!["data 2 a"],
            ]);
            table.normalize();

            let mut seen_lines = Vec::with_capacity(3);
            // retain only the rows with empty cells. This lets us get around the short-circuiting
            // of retain_rows (it short-circuits within each row as soon as it finds a matching
            // cell), to validate that the normalization works as expected.
            table.retain_rows(|line| {
                seen_lines.push(simple_to_string(line));
                line.is_empty()
            });

            // normalization
            assert_eq!(
                table.alignments,
                vec![Some(ColumnAlignment::Left), Some(ColumnAlignment::Right), None]
            );
            assert_eq!(
                table.rows,
                vec![
                    vec![cell("header a"), cell("header b"), Vec::new()],
                    vec![cell("data 2 a"), Vec::new(), Vec::new()],
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

        fn cell_matches(substring: &str) -> impl Fn(&TableCell) -> bool + '_ {
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
                Some(ColumnAlignment::Left),
                Some(ColumnAlignment::Right),
                Some(ColumnAlignment::Center),
            ]
            .iter()
            .cycle()
            .take(first_row.len())
            .map(ToOwned::to_owned)
            .collect();
            let mut rows = Vec::with_capacity(cells.len());

            for row_strings in rows_iter {
                let mut row = Vec::with_capacity(row_strings.len());
                for cell_string in row_strings {
                    row.push(cell(cell_string));
                }
                rows.push(row);
            }

            Table { alignments, rows }
        }

        fn cell(value: &str) -> TableCell {
            vec![Inline::Text(Text {
                variant: TextVariant::Plain,
                value: value.to_string(),
            })]
        }

        fn simple_to_string(line: &TableCell) -> String {
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
