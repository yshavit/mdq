use crate::md_elem::elem::List;
use crate::md_elem::{MdContext, MdElem};
use crate::select::string_matcher::StringMatcher;
use crate::select::{ListItemMatcher, ListItemTask, TrySelector};

#[derive(Debug, PartialEq)]
pub struct ListItemSelector {
    li_type: ListItemType,
    checkbox: ListItemTask,
    string_matcher: StringMatcher,
}

impl From<ListItemMatcher> for ListItemSelector {
    fn from(value: ListItemMatcher) -> Self {
        Self {
            li_type: if value.ordered {
                ListItemType::Ordered
            } else {
                ListItemType::Unordered
            },
            checkbox: value.task,
            string_matcher: value.matcher.into(),
        }
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum ListItemType {
    Ordered,
    Unordered,
}

fn task_matches(matcher: ListItemTask, md_is_checked: Option<bool>) -> bool {
    match matcher {
        ListItemTask::Unselected => md_is_checked == Some(false),
        ListItemTask::Selected => md_is_checked == Some(true),
        ListItemTask::Either => md_is_checked.is_some(),
        ListItemTask::None => md_is_checked.is_none(),
    }
}

impl TrySelector<List> for ListItemSelector {
    fn try_select(&self, _: &MdContext, item: List) -> Result<Vec<MdElem>, MdElem> {
        // This one works a bit differently than most:
        // - If the item has a single list, check it; this is essentially a recursive base case.
        // - Otherwise, never match, but return an MdElem::Doc of the list items, each as its own list.
        //   That way, the find_children code in api.rs will recurse back into this method for each of those items, but
        //   as a single-item list for the base case.
        let List { starting_index, items } = item;
        match items.as_slice() {
            [li] => {
                let matched = self.li_type.matches(&starting_index)
                    && task_matches(self.checkbox, li.checked)
                    && self.string_matcher.matches_any(&li.item);
                let list = MdElem::List(List { starting_index, items });
                if matched {
                    Ok(vec![list])
                } else {
                    Err(list)
                }
            }
            _ => {
                let mut idx = starting_index;
                let mut items_doc = Vec::with_capacity(items.len());
                for item in items {
                    items_doc.push(MdElem::List(List {
                        starting_index: idx,
                        items: vec![item],
                    }));
                    if let Some(idx) = idx.as_mut() {
                        *idx += 1;
                    }
                }
                Err(MdElem::Doc(items_doc))
            }
        }
    }
}

impl ListItemType {
    fn matches(&self, idx: &Option<u32>) -> bool {
        match self {
            ListItemType::Ordered => idx.is_some(),
            ListItemType::Unordered => idx.is_none(),
        }
    }
}
