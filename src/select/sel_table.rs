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
            return Err(ParseErrorReason::InvalidSyntax("table headers matcher may not be empty. Use an explicit \"*\" to select all columns.".to_string()))
        }
        let headers_matcher = StringMatcher::read(iter, ':')?;

        // rows matcher
        iter.drop_whitespace();
        iter.require_str(":-:")?;
        iter.require_whitespace(":-:")?;
        let rows_matcher = StringMatcher::read(iter, SELECTOR_SEPARATOR)?;

        Ok(Self{headers_matcher, rows_matcher})
    }
}

impl<'a> Selector<'a, &'a Table> for TableSelector {
    fn try_select(&self, item: &'a Table) -> Option<MdElemRef<'a>> {
        let slice = TableSlice::from(item);
        let mut slice = slice.normalize(); // TODO make this a "&mut self" that returns ()
        let mut slice = slice.retain_columns(|line| {
            let res = self.headers_matcher.matches_inlines(line);
            eprint!("looking for {:?} in {line:?}: {res}\n", self.headers_matcher);
            res
        })?; // TODO should retain_columns return an Option<()> so I can just ?; it here?
        let slice = slice.retain_rows(|line| self.rows_matcher.matches_inlines(line))?;
        Some(slice.into())
    }
}