use crate::matcher::StringMatcher;
use crate::parsing_iter::ParsingIterator;
use crate::select::match_selector::MatchSelector;
use crate::select::{ParseErrorReason, ParseResult, SELECTOR_SEPARATOR};
use crate::tree_ref::ListItemRef;

#[derive(Debug, PartialEq)]
pub struct ListItemSelector {
    li_type: ListItemType,
    checkbox: CheckboxSpecifier,
    string_matcher: StringMatcher,
}

#[derive(Debug, PartialEq, Copy, Clone)]
pub enum ListItemType {
    Ordered,
    Unordered,
}

impl ListItemType {
    pub fn read(self, chars: &mut ParsingIterator) -> ParseResult<ListItemSelector> {
        // list-type-specific parsing (ie, the dot after "1" for ordered lists)
        self.read_type(chars)?;

        // whitespace
        chars.require_whitespace_or(SELECTOR_SEPARATOR, "List item specifier")?;

        // checkbox
        let checkbox = match chars.consume_if('[') {
            false => CheckboxSpecifier::NoCheckbox,
            true => {
                let checkbox = match chars.next() {
                    Some(' ') => CheckboxSpecifier::CheckboxUnchecked,
                    Some('x') => CheckboxSpecifier::CheckboxChecked,
                    Some('?') => CheckboxSpecifier::CheckboxEither,
                    Some(_) => {
                        return Err(ParseErrorReason::InvalidSyntax(
                            "checkbox specifier must be one of: [ ], [x], or [?]".to_string(),
                        ))
                    }
                    None => return Err(ParseErrorReason::UnexpectedEndOfInput),
                };
                chars.require_char(']')?;
                chars.require_whitespace_or(SELECTOR_SEPARATOR, "task specifier")?;
                checkbox
            }
        };

        // item string matcher
        let string_matcher = StringMatcher::read(chars, SELECTOR_SEPARATOR)?;

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

impl ListItemSelector {
    pub fn read(variant: ListItemType, chars: &mut ParsingIterator) -> ParseResult<Self> {
        variant.read(chars)
    }
}

impl<'a> MatchSelector<'a, ListItemRef<'a>> for ListItemSelector {
    fn matches(&self, item: ListItemRef<'a>) -> bool {
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

    fn read_type(&self, chars: &mut ParsingIterator) -> ParseResult<()> {
        if matches!(self, ListItemType::Ordered) {
            chars.require_char_or_else('.', || {
                ParseErrorReason::InvalidSyntax("Ordered list item specifier must start with \"1.\"".to_string())
            })
        } else {
            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parsing_iter::ParsingIterator;

    #[test]
    fn unordered_empty_matcher() {
        check_ok(ListItemType::Unordered, "", CheckboxSpecifier::NoCheckbox, "");
    }

    #[test]
    fn unordered_empty_matcher_then_pipe() {
        check_ok(ListItemType::Unordered, "|", CheckboxSpecifier::NoCheckbox, "");
    }

    #[test]
    fn ordered_empty_matcher() {
        check_ok(ListItemType::Ordered, ".", CheckboxSpecifier::NoCheckbox, "");
    }

    #[test]
    fn ordered_empty_matcher_then_pipe() {
        check_ok(ListItemType::Ordered, ".|", CheckboxSpecifier::NoCheckbox, "");
    }

    #[test]
    fn ordered_missing_dot() {
        check_bad(
            ListItemType::Ordered,
            "",
            ParseErrorReason::InvalidSyntax("Ordered list item specifier must start with \"1.\"".to_string()),
        );
    }

    #[test]
    fn ordered_with_matcher() {
        check_ok(ListItemType::Ordered, ". foo ", CheckboxSpecifier::NoCheckbox, "foo");
    }

    #[test]
    fn task_complete() {
        check_ok(ListItemType::Unordered, " [x]", CheckboxSpecifier::CheckboxChecked, "");
    }

    #[test]
    fn task_incomplete() {
        check_ok(
            ListItemType::Unordered,
            " [ ]",
            CheckboxSpecifier::CheckboxUnchecked,
            "",
        );
    }

    #[test]
    fn task_any_completion() {
        check_ok(ListItemType::Unordered, " [?]", CheckboxSpecifier::CheckboxEither, "");
    }

    #[test]
    fn task_bad_completion_char() {
        check_bad(
            ListItemType::Unordered,
            " [!]",
            ParseErrorReason::InvalidSyntax("checkbox specifier must be one of: [ ], [x], or [?]".to_string()),
        );
    }

    #[test]
    fn task_no_space_before() {
        check_bad(
            ListItemType::Unordered,
            "[ ]",
            ParseErrorReason::InvalidSyntax("List item specifier must be followed by whitespace".to_string()),
        );
    }

    #[test]
    fn task_with_matcher() {
        check_ok(
            ListItemType::Unordered,
            " [x] foo",
            CheckboxSpecifier::CheckboxChecked,
            "foo",
        );
    }

    #[test]
    fn task_premature_eof() {
        check_bad(ListItemType::Unordered, " [", ParseErrorReason::UnexpectedEndOfInput);
        check_bad(ListItemType::Unordered, " [x", ParseErrorReason::Expected(']'));
    }

    #[test]
    fn task_then_pipe() {
        check_ok(
            ListItemType::Unordered,
            " [ ]|",
            CheckboxSpecifier::CheckboxUnchecked,
            "",
        );
    }

    #[test]
    fn task_no_space_after_matcher() {
        check_bad(
            ListItemType::Unordered,
            " [ ]foo",
            ParseErrorReason::InvalidSyntax("task specifier must be followed by whitespace".to_string()),
        );
    }

    fn check_ok(li_type: ListItemType, input_str: &str, expect_checkbox: CheckboxSpecifier, expect_matcher_str: &str) {
        assert_eq!(
            li_type.read(&mut ParsingIterator::new(input_str)).unwrap(),
            ListItemSelector {
                li_type,
                checkbox: expect_checkbox,
                string_matcher: StringMatcher::read(&mut ParsingIterator::new(expect_matcher_str), '|').unwrap(),
            }
        )
    }

    fn check_bad(li_type: ListItemType, input_str: &str, reason: ParseErrorReason) {
        let parsed = li_type.read(&mut ParsingIterator::new(input_str));
        assert_eq!(parsed, Err(reason))
    }
}
