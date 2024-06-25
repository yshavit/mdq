use crate::tree_ref::MdElemRef;

pub trait Selector<'a, I: Copy> {
    fn matches(&self, item: I) -> bool;
    fn pick(&self, item: I) -> MdElemRef<'a>;

    fn try_select(&self, item: I) -> Option<MdElemRef<'a>> {
        if self.matches(item) {
            Some(self.pick(item))
        } else {
            None
        }
    }
}
