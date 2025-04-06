use crate::md_elem::*;
use crate::select::TrySelector;

/// MatchSelector is a helper trait for implementing [TrySelector]. Simply provide the boolean predicate for whether a
/// given item matches, and MatchSelector will do the rest.
pub trait MatchSelector<I> {
    fn matches(&self, item: I) -> bool;
}

impl<'md, I, M> TrySelector<'md, I> for M
where
    I: Copy + Into<MdElemRef<'md>>,
    M: MatchSelector<I>,
{
    fn try_select(&self, item: I) -> Option<MdElemRef<'md>> {
        if self.matches(item) {
            Some(item.into())
        } else {
            None
        }
    }
}
