use crate::md_elem::elem_ref::*;
use crate::select::match_selector::MatchSelector;
use crate::select::string_matcher::StringMatcher;
use crate::select::{ListItemMatcher, ListItemTask};

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

impl MatchSelector<ListItemRef<'_>> for ListItemSelector {
    fn matches(&self, item: ListItemRef) -> bool {
        let ListItemRef(idx, li) = item;
        self.li_type.matches(&idx)
            && task_matches(self.checkbox, li.checked)
            && self.string_matcher.matches_any(&li.item)
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
