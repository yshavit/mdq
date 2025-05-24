use crate::md_elem::*;
use crate::select::api::{Result, Selection};
use crate::select::TrySelector;

/// MatchSelector is a helper trait for implementing [TrySelector]. Simply provide the boolean predicate for whether a
/// given item matches, and MatchSelector will do the rest.
pub trait MatchSelector<I> {
    fn matches(&self, item: &I) -> bool;
}

impl<I, M> TrySelector<I> for M
where
    I: Into<MdElem>,
    M: MatchSelector<I>,
{
    fn try_select(&self, _: &MdContext, item: I) -> Result<Selection> {
        if self.matches(&item) {
            Ok(Selection::Selected(vec![item.into()]))
        } else {
            Ok(Selection::NotSelected(item.into()))
        }
    }
}
