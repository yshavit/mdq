use crate::md_elem::elem::List;
use crate::md_elem::{MdContext, MdElem};
use crate::select::match_selector::make_select_result;
use crate::select::string_matcher::StringMatcher;
use crate::select::{ListItemMatcher, ListItemTask, Result, Select, TrySelector};

#[derive(Debug, PartialEq)]
pub(crate) struct ListItemSelector {
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
pub(crate) enum ListItemType {
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
    fn try_select(&self, _: &MdContext, item: List) -> Result<Select> {
        // This one works a bit differently than most:
        // - If the item has a single list, check it; this is essentially a recursive base case.
        // - Otherwise, never match, but return an MdElem::Doc of the list items, each as its own list.
        //   That way, the find_children code in api.rs will recurse back into this method for each of those items, but
        //   as a single-item list for the base case.
        let List {
            starting_index,
            mut items,
        } = item;
        match items.as_mut_slice() {
            [li] => {
                let (matched, items) =
                    if !(self.li_type.matches(&starting_index) && task_matches(self.checkbox, li.checked)) {
                        (false, items)
                    } else {
                        let mut replacement = self
                            .string_matcher
                            .match_replace_any(std::mem::take(&mut li.item))
                            .map_err(|e| e.to_select_error("list item"))?;
                        std::mem::swap(&mut replacement.item, &mut li.item);
                        (replacement.matched_any, items)
                    };
                let list = MdElem::List(List { starting_index, items });
                Ok(make_select_result(list, matched))
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
                Ok(Select::Miss(MdElem::Doc(items_doc)))
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::md_elem::elem::{ListItem, Paragraph};
    use crate::md_elem::{inlines, md_elem};
    use crate::select::{MatchReplace, Matcher, Regex};

    #[test]
    fn regex_match_matches() {
        let original = list("hello, world");
        let result = li_selector("hello", None)
            .try_select(&MdContext::empty(), original)
            .unwrap();
        assert_eq!(result, Select::Hit(vec![MdElem::List(list("hello, world"))]));
    }

    #[test]
    fn regex_match_no_match() {
        let original = list("hello, world");
        let result = li_selector("goodbye", None)
            .try_select(&MdContext::empty(), original)
            .unwrap();
        assert_eq!(result, Select::Miss(MdElem::List(list("hello, world"))));
    }

    #[test]
    fn regex_replace_matches() {
        let original = list("hello, world");
        let result = li_selector("hello", Some("Hi there"))
            .try_select(&MdContext::empty(), original)
            .unwrap();
        assert_eq!(result, Select::Hit(vec![MdElem::List(list("Hi there, world"))]));
    }

    #[test]
    fn regex_replace_no_match() {
        let original = list("hello, world");
        let result = li_selector("goodbye", Some("Hi there"))
            .try_select(&MdContext::empty(), original)
            .unwrap();
        assert_eq!(result, Select::Miss(MdElem::List(list("hello, world"))));
    }

    fn list(contents: &str) -> List {
        List {
            starting_index: None,
            items: vec![ListItem {
                checked: None,
                item: vec![md_elem!(Paragraph {
                    body: inlines!(text[contents])
                })],
            }],
        }
    }

    fn li_selector(pattern: &str, replacement: Option<&str>) -> ListItemSelector {
        let matcher = ListItemMatcher {
            ordered: false,
            task: ListItemTask::None,
            matcher: MatchReplace {
                matcher: Matcher::Regex(Regex {
                    re: fancy_regex::Regex::new(pattern).unwrap(),
                }),
                replacement: replacement.map(String::from),
            },
        };
        ListItemSelector::from(matcher)
    }
}
