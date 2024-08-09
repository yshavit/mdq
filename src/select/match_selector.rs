use crate::select::Selector;
use crate::tree_ref::MdElemRef;

/// MatchSelector is a helper trait for implementing [Selector]. Simply provide the boolean predicate for whether a
/// given item matches, and MatchSelector will do the rest.
pub trait MatchSelector<I> {
    fn matches(&self, item: I) -> bool;
}

impl<'md, I, M> Selector<'md, I> for M
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
