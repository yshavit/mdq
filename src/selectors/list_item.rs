use crate::matcher::Matcher;
use crate::parsing_iter::ParsingIterator;
use crate::selectors::base::{ParseErrorReason, ParseResult, SelectResult, Selector};
use crate::selectors::util::require_whitespace;
use crate::tree_ref::{ListItemRef, MdqNodeRef};

#[derive(Debug, PartialEq)]
pub struct ListItemSelector {
    li_type: ListItemType,
    checkbox: CheckboxSpecifier,
    string_matcher: Matcher,
}

#[derive(Debug, PartialEq)]
pub enum ListItemType {
    Ordered,
    Unordered,
}

impl ListItemType {
    pub fn read<C: Iterator<Item = char>>(self, chars: &mut ParsingIterator<C>) -> ParseResult<ListItemSelector> {
        // list-type-specific parsing (ie, the dot after "1" for ordered lists)
        self.read_type(chars)?;

        // whitespace
        require_whitespace(chars, "List item specifier")?;

        // checkbox
        let checkbox = if chars.peek() == Some('[') {
            chars.next(); // consume the '['
            let spec = match chars.next() {
                None => return Err(ParseErrorReason::UnexpectedEndOfInput),
                Some(' ') => CheckboxSpecifier::CheckboxUnchecked,
                Some('x') => CheckboxSpecifier::CheckboxChecked,
                Some('?') => CheckboxSpecifier::CheckboxEither,
                Some(other) => return Err(ParseErrorReason::UnexpectedCharacter(other)),
            };
            if chars.next() != Some(']') {
                return Err(ParseErrorReason::InvalidSyntax("expected \"]\"".to_string()));
            }
            require_whitespace(chars, "list item type")?;
            spec
        } else {
            CheckboxSpecifier::NoCheckbox
        };

        // item string matcher
        let string_matcher = Matcher::parse_matcher(chars)?;

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
        SelectResult::One(MdqNodeRef::ListItem(item))
    }
}

impl ListItemType {
    fn matches(&self, idx: &Option<u32>) -> bool {
        match self {
            ListItemType::Ordered => idx.is_some(),
            ListItemType::Unordered => idx.is_none(),
        }
    }

    fn read_type<C: Iterator<Item = char>>(&self, chars: &mut ParsingIterator<C>) -> ParseResult<()> {
        if matches!(self, ListItemType::Ordered) {
            chars.next().map(|_| ()).ok_or_else(|| {
                ParseErrorReason::InvalidSyntax("Ordered list item specifier must start with \"1.\"".to_string())
            })
        } else {
            Ok(())
        }
    }
}
