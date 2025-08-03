use crate::md_elem::*;
use crate::select::api::{Result, Select};
use crate::select::string_matcher::StringMatchError;
use crate::select::TrySelector;

/// MatchSelector is a helper trait for implementing [TrySelector]. Simply provide the boolean predicate for whether a
/// given item matches, and MatchSelector will do the rest.
pub(crate) trait MatchSelector<I> {
    const NAME: &'static str;

    fn matches(&self, item: &I) -> std::result::Result<bool, StringMatchError>;
}

impl<I, M> TrySelector<I> for M
where
    I: Into<MdElem>,
    M: MatchSelector<I>,
{
    fn try_select(&self, _: &MdContext, item: I) -> Result<Select> {
        if self.matches(&item).map_err(|e| e.to_select_error(M::NAME))? {
            Ok(Select::Hit(vec![item.into()]))
        } else {
            Ok(Select::Miss(item.into()))
        }
    }
}

pub(crate) fn make_select_result<T: Into<MdElem>>(item: T, matched_any: bool) -> Select {
    if matched_any {
        Select::Hit(vec![item.into()])
    } else {
        Select::Miss(item.into())
    }
}
