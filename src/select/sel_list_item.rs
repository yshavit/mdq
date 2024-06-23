use crate::matcher::StringMatcher;
use crate::parsing_iter::ParsingIterator;
use crate::select::base::Selector;
use crate::select::{ParseErrorReason, ParseResult, SelectResult};
use crate::tree_ref::{ListItemRef, MdElemRef};

#[derive(Debug, PartialEq)]
pub struct ListItemSelector {
    li_type: ListItemType,
    checkbox: CheckboxSpecifier,
    string_matcher: StringMatcher,
}

#[derive(Debug, PartialEq)]
pub enum ListItemType {
    Ordered,
    Unordered,
}

impl ListItemType {
    pub fn read(self, chars: &mut ParsingIterator) -> ParseResult<ListItemSelector> {
        // list-type-specific parsing (ie, the dot after "1" for ordered lists)
        self.read_type(chars)?;

        // whitespace
        chars.require_whitespace("List item specifier")?;

        // checkbox
        let checkbox = match chars.consume_if('[') {
            false => CheckboxSpecifier::NoCheckbox,
            true => {
                let checkbox = match chars.next() {
                    Some(' ') => CheckboxSpecifier::CheckboxUnchecked,
                    Some('x') => CheckboxSpecifier::CheckboxChecked,
                    Some('?') => CheckboxSpecifier::CheckboxEither,
                    Some(other) => return Err(ParseErrorReason::UnexpectedCharacter(other)),
                    None => return Err(ParseErrorReason::UnexpectedEndOfInput),
                };
                chars.require_char(']', || ParseErrorReason::UnexpectedEndOfInput)?;
                checkbox
            }
        };

        // item string matcher
        let string_matcher = StringMatcher::read(chars)?;

        Ok(ListItemSelector {
            li_type: self,
            checkbox,
            string_matcher,
        })
    }
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

impl<'a> Selector<'a, ListItemRef<'a>> for ListItemSelector {
    fn matches(&self, item: ListItemRef<'a>) -> bool {
        let ListItemRef(idx, li) = item;
        self.li_type.matches(&idx) && self.checkbox.matches(&li.checked) && self.string_matcher.matches_any(&li.item)
    }

    fn pick(&self, item: ListItemRef<'a>) -> SelectResult<'a> {
        SelectResult::One(MdElemRef::ListItem(item))
    }
}

impl ListItemType {
    fn matches(&self, idx: &Option<u32>) -> bool {
        match self {
            ListItemType::Ordered => idx.is_some(),
            ListItemType::Unordered => idx.is_none(),
        }
    }

    fn read_type(&self, chars: &mut ParsingIterator) -> ParseResult<()> {
        if matches!(self, ListItemType::Ordered) {
            chars.require_char('.', || {
                ParseErrorReason::InvalidSyntax("Ordered list item specifier must start with \"1.\"".to_string())
            })
        } else {
            Ok(())
        }
    }
}
