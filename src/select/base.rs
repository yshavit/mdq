use crate::tree_ref::MdElemRef;

pub trait Selector<'a, I: Copy + Into<MdElemRef<'a>>> {
    fn matches(&self, item: I) -> bool;

    fn try_select(&self, item: I) -> Option<MdElemRef<'a>> {
        if self.matches(item) {
            Some(item.into())
        } else {
            None
        }
    }
}
