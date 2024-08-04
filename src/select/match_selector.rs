use crate::select::Selector;
use crate::tree_ref::MdElemRef;

/// MatchSelector is a helper trait for implementing [Selector]. Simply provide the boolean predicate for whether a
/// given item matches, and MatchSelector will do the rest.
pub trait MatchSelector<I> {
    fn matches(&self, item: I) -> bool;
}

impl<'a, I, M> Selector<'a, I> for M
where
    I: Copy + Into<MdElemRef<'a>>,
    M: MatchSelector<I>,
{
    fn try_select(&self, item: I) -> Option<MdElemRef<'a>> {
        if self.matches(item) {
            Some(item.into())
        } else {
            None
        }
    }
}
