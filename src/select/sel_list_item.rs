use crate::matcher::StringMatcher;
use crate::query::selectors::ListItemMatcher;
use crate::select::match_selector::MatchSelector;
use crate::tree_ref::ListItemRef;

#[derive(Debug, PartialEq)]
pub struct ListItemSelector {
    li_type: ListItemType,
    checkbox: CheckboxSpecifier,
    string_matcher: StringMatcher,
}

impl From<ListItemMatcher> for ListItemSelector {
    fn from(value: ListItemMatcher) -> Self {
        todo!()
    }
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum ListItemType {
    Ordered,
    Unordered,
}

#[derive(Debug, PartialEq)]
enum CheckboxSpecifier {
    CheckboxUnchecked,
    CheckboxChecked,
    CheckboxEither,
    NoCheckbox,
}

impl CheckboxSpecifier {
    fn matches(&self, checked: &Option<bool>) -> bool {
        match self {
            CheckboxSpecifier::CheckboxUnchecked => checked == &Some(false),
            CheckboxSpecifier::CheckboxChecked => checked == &Some(true),
            CheckboxSpecifier::CheckboxEither => checked.is_some(),
            CheckboxSpecifier::NoCheckbox => checked.is_none(),
        }
    }
}

impl MatchSelector<ListItemRef<'_>> for ListItemSelector {
    fn matches(&self, item: ListItemRef) -> bool {
        let ListItemRef(idx, li) = item;
        self.li_type.matches(&idx) && self.checkbox.matches(&li.checked) && self.string_matcher.matches_any(&li.item)
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
