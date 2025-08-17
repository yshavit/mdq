use crate::md_elem::elem::*;

mod elem_ref {
    use super::*;

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
    }
}
