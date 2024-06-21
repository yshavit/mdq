use crate::select::SelectResult;

pub trait Selector<'a, I: Copy> {
    fn matches(&self, item: I) -> bool;
    fn pick(&self, item: I) -> SelectResult<'a>;

    fn try_select(&self, item: I) -> Option<SelectResult<'a>> {
        if self.matches(item) {
            Some(self.pick(item))
        } else {
            None
        }
    }
}
