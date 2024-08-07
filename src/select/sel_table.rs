use crate::matcher::StringMatcher;
use crate::parsing_iter::ParsingIterator;
use crate::select::{ParseErrorReason, ParseResult, Selector, SELECTOR_SEPARATOR};
use crate::tree::Table;
use crate::tree_ref::{MdElemRef, TableSlice};

#[derive(Debug, PartialEq)]
pub struct TableSelector {
    headers_matcher: StringMatcher,
    rows_matcher: StringMatcher,
}

impl TableSelector {
    pub fn read(iter: &mut ParsingIterator) -> ParseResult<Self> {
        // headers matcher
        iter.require_str("-:")?;
        iter.require_whitespace(":-:")?;
        if iter.peek() == Some(':') {
            return Err(ParseErrorReason::InvalidSyntax("table headers matcher may not be empty. Use an explicit \"*\" to select all columns.".to_string()));
        }
        let headers_matcher = StringMatcher::read(iter, ':')?;

        // rows matcher
        iter.require_str(":-:")?;
        iter.require_whitespace_or(SELECTOR_SEPARATOR, ":-:")?;
        let rows_matcher = StringMatcher::read(iter, SELECTOR_SEPARATOR)?;

        Ok(Self { headers_matcher, rows_matcher })
    }
}

impl<'a> Selector<'a, &'a Table> for TableSelector {
    fn try_select(&self, item: &'a Table) -> Option<MdElemRef<'a>> {
        let mut slice = TableSlice::from(item);
        slice.normalize();

        slice.retain_columns_by_header(|line| {
            self.headers_matcher.matches_inlines(line)
        });
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

    mod parse {
        use crate::select::ParseErrorReason::InvalidSyntax;
        use super::*;

        #[test]
        fn only_row_matcher_provided() {
            // note: no space required before the second ":-:"
            expect_ok(":-: 'a':-:", "a", ".*");
        }

        #[test]
        fn both_matchers_provided() {
            expect_ok(":-: 'a' :-: 'b'", "a", "b");
        }

        #[test]
        fn row_matcher_empty() {
            expect_invalid(":-: :-:","table headers matcher may not be empty. Use an explicit \"*\" to select all columns.");
        }

        #[test]
        fn row_matcher_no_space() {
            expect_invalid(":-:X :-:",":-: must be followed by whitespace");
        }

        fn expect_ok(in_str: &str, headers_matcher_re: &str, rows_matcher_re: &str) {
            let actual_test = |actual_str: &str| {
                let mut in_chars = ParsingIterator::new(actual_str);
                in_chars.require_char(':').expect("test error: bad selector format");

                let actual = TableSelector::read(&mut in_chars).expect("test error: bad selector format");

                let expect_headers_matcher = StringMatcher::from(headers_matcher_re);
                let expect_rows_matcher = StringMatcher::from(rows_matcher_re);
                assert_eq!(actual.headers_matcher, expect_headers_matcher);
                assert_eq!(actual.rows_matcher, expect_rows_matcher);
            };

            // Run it three times: once as-is, and then with a trailing selector separator, and then
            // with a space and separator. They should all be equivalent.
            actual_test(in_str);
            actual_test(&format!("{in_str}{SELECTOR_SEPARATOR}"));
            actual_test(&format!("{in_str} {SELECTOR_SEPARATOR}"));
        }

        fn expect_invalid(in_str: &str, expect_err: &str) {
            let mut in_chars = ParsingIterator::new(in_str);
            in_chars.require_char(':').expect("test error: bad selector format");

            let actual = TableSelector::read(&mut in_chars);
            assert_eq!(actual, Err(InvalidSyntax(expect_err.to_string())))
        }
    }

    mod select {
        use super::*;
        use markdown::mdast;
        use crate::tree::{Inline, Line, Text, TextVariant};
        use crate::unwrap;

        #[test]
        fn select_all_on_normalized_table() {
            let table = Table{
                alignments: vec![mdast::AlignKind::Left, mdast::AlignKind::Right],
                rows: vec![
                    vec![cell("header a"), cell("header b")],
                    vec![cell("data 1 a"), cell("data 1 b")],
                ],
            };
            let maybe_selected = TableSelector {
                headers_matcher: ".*".into(),
                rows_matcher: ".*".into(),
            }.try_select(&table);

            unwrap!(maybe_selected, Some(MdElemRef::TableSlice(table)));
            assert_eq!(
                table.alignments(),
                &vec![mdast::AlignKind::Left, mdast::AlignKind::Right]
            );
            assert_eq!(
                table.rows().collect::<Vec<_>>(),
                vec![
                    &vec![Some(&cell("header a")), Some(&cell("header b"))],
                    &vec![Some(&cell("data 1 a")), Some(&cell("data 1 b"))],
                ]
            );
        }

        #[test]
        fn select_columns_on_normalized_table() {
            let table = Table{
                alignments: vec![mdast::AlignKind::Left, mdast::AlignKind::Right],
                rows: vec![
                    vec![cell("header a"), cell("KEEP b")],
                    vec![cell("data 1 a"), cell("data 1 b")],
                ],
            };
            let maybe_selected = TableSelector {
                headers_matcher: "KEEP".into(),
                rows_matcher: ".*".into(),
            }.try_select(&table);

            unwrap!(maybe_selected, Some(MdElemRef::TableSlice(table)));
            assert_eq!(
                table.alignments(),
                &vec![mdast::AlignKind::Right]
            );
            assert_eq!(
                table.rows().collect::<Vec<_>>(),
                vec![
                    &vec![Some(&cell("KEEP b"))],
                    &vec![Some(&cell("data 1 b"))],
                ]
            );
        }

        #[test]
        fn select_rows_on_normalized_table() {
            let table = Table{
                alignments: vec![mdast::AlignKind::Left, mdast::AlignKind::Right],
                rows: vec![
                    vec![cell("header a"), cell("header b")],
                    vec![cell("data 1 a"), cell("data 1 b")],
                    vec![cell("data 2 a"), cell("data 2 b")],
                ],
            };
            let maybe_selected = TableSelector {
                headers_matcher: ".*".into(),
                rows_matcher: "data 2".into(),
            }.try_select(&table);

            unwrap!(maybe_selected, Some(MdElemRef::TableSlice(table)));
            assert_eq!(
                table.alignments(),
                &vec![mdast::AlignKind::Left, mdast::AlignKind::Right]
            );
            assert_eq!(
                table.rows().collect::<Vec<_>>(),
                vec![
                    // note: header always gets retained
                    &vec![Some(&cell("header a")), Some(&cell("header b"))],
                    &vec![Some(&cell("data 2 a")), Some(&cell("data 2 b"))],
                ]
            );
        }

        /// Tests (a) that the table gets normalized, and (b) a smoke test of the matchers.
        /// More extensive tests for the `retain_*` methods can be found in [TableSlice]'s tests.
        #[test]
        fn jagged_table() {
            let table = Table{
                // only 1 align; rest will be filled with None
                alignments: vec![mdast::AlignKind::Left],
                rows: vec![
                    vec![cell("header a")],
                    vec![cell("data 1 a"), cell("data 1 b")],
                    vec![cell("data 2 a"), cell("data 2 b"), cell("data 2 c")],
                ],
            };
            let maybe_selected = TableSelector {
                headers_matcher: ".*".into(),
                rows_matcher: "data 1".into(),
            }.try_select(&table);

            unwrap!(maybe_selected, Some(MdElemRef::TableSlice(table)));
            assert_eq!(
                table.alignments(),
                &vec![mdast::AlignKind::Left, mdast::AlignKind::None, mdast::AlignKind::None]
            );
            assert_eq!(
                table.rows().collect::<Vec<_>>(),
                vec![
                    &vec![Some(&cell("header a")), None, None],
                    &vec![Some(&cell("data 1 a")), Some(&cell("data 1 b")), None],
                ]
            );
        }

        fn cell(cell_str: &str) -> Line {
            vec![Inline::Text(Text{variant: TextVariant::Plain, value: cell_str.to_string()})]
        }
    }
}