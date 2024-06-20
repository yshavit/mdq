use crate::fmt_str::inlines_to_plain_string;
use crate::matcher::Matcher;
use crate::parse_common::{ParseError, ParseErrorReason, ParseResult};
use crate::parsing_iter::ParsingIterator;
use crate::tree::{Inline, ListItem};
use crate::tree_ref::MdqNodeRef;
use crate::wrap_mdq_refs;

#[derive(Debug, PartialEq)]
pub enum Selector {
    Any,
    // List(Matcher, ListType, Selected),  // should this be list or list item?
    // Image { text: Matcher, href: Matcher },
    // Link { text: Matcher, href: Matcher },
    // CodeBlock(Matcher),
    /// Selects the content of a section identified by its heading.
    ///
    /// Format: `# <string_matcher>`
    ///
    /// In bareword form, the string matcher terminates with the
    /// [selector delimiter character](parse_common::SELECTOR_SEPARATOR).
    Section(SectionSelector),

    /// Selects a list item.
    ///
    /// Format: `<type> [checkbox] <string_matcher>` where:
    /// - _type_ is either `-` for unordered lists or `1.` for ordered lists. Note that ordered lists are _only_
    ///   identified by `1.`. Other digits are invalid.
    /// - _checkbox_, if provided, must be one of:
    ///   - `[ ]` for an unchecked box
    ///   - `[x]` for a checked box
    ///   - `[?]` for a box that may be checked or unchecked
    ///
    /// If the checkbox specifier is provided, the selector will only select list items with a checkbox. If the
    /// checkbox specifier is omitted, the selector will only select list items without a checkbox.
    ///
    /// In bareword form, the string matcher terminates with the [selector delimiter character](SELECTOR_SEPARATOR).
    ListItem(ListItemSelector),
}

impl Selector {
    pub fn find_nodes<'a>(&self, nodes: Vec<MdqNodeRef<'a>>) -> Vec<MdqNodeRef<'a>> {
        let mut result = Vec::with_capacity(8); // arbitrary guess
        for node in nodes {
            self.build_output(&mut result, node);
        }
        result
    }

    pub fn build_output<'a>(&self, out: &mut Vec<MdqNodeRef<'a>>, node: MdqNodeRef<'a>) {
        let found = match (self, node.clone()) {
            (Selector::Section(selector), MdqNodeRef::Section(header)) => {
                let header_text = inlines_to_plain_string(&header.title);
                if selector.matcher.matches(&header_text) {
                    header.body.iter().for_each(|child| out.push(child.into()));
                    true
                } else {
                    false
                }
            }
            (Selector::ListItem(selector), MdqNodeRef::ListItem(idx, item)) => selector.matches(&idx, item),
            _ => false,
        };
        if !found {
            for child in Self::find_children(node) {
                self.build_output(out, child);
            }
        }
    }

    /// Recurse from this node to its children.
    ///
    /// This makes sense to put here (as opposed to in the [tree] module) because the definition of a "child" is
    /// selector-specific. For example, an [MdqNode::Section] has child nodes both in its title and in its body, but
    /// only the body nodes are relevant for select recursion. `MdqNode` shouldn't need to know about that oddity; it
    /// belongs here.
    fn find_children<'a>(node: MdqNodeRef) -> Vec<MdqNodeRef> {
        match node {
            MdqNodeRef::Section(s) => MdqNodeRef::wrap_vec(&s.body),
            MdqNodeRef::Paragraph(p) => wrap_mdq_refs!(Inline: &p.body),
            MdqNodeRef::BlockQuote(b) => MdqNodeRef::wrap_vec(&b.body),
            MdqNodeRef::List(list) => {
                let mut idx = list.starting_index;
                let mut result = Vec::with_capacity(list.items.len());
                for item in &list.items {
                    result.push(MdqNodeRef::ListItem(idx.clone(), item));
                    if let Some(idx) = idx.as_mut() {
                        *idx += 1;
                    }
                }
                result
            }
            MdqNodeRef::Table(table) => {
                let count_estimate = table.rows.len() * table.rows.first().map(|tr| tr.len()).unwrap_or(0);
                let mut result = Vec::with_capacity(count_estimate);
                for row in &table.rows {
                    for col in row {
                        for cell in col {
                            result.push(MdqNodeRef::Inline(cell));
                        }
                    }
                }
                result
            }
            MdqNodeRef::ThematicBreak => Vec::new(),
            MdqNodeRef::CodeBlock(_) => Vec::new(),
            MdqNodeRef::ListItem(_, item) => MdqNodeRef::wrap_vec(&item.item),
            MdqNodeRef::Inline(inline) => match inline {
                Inline::Span { children, .. } => children.iter().map(|child| MdqNodeRef::Inline(child)).collect(),
                Inline::Footnote(footnote) => MdqNodeRef::wrap_vec(&footnote.text),
                Inline::Link { .. } => {
                    // TODO need to return an MdqNodeRef::Link
                    Vec::new()
                }
                Inline::Text { .. } | Inline::Image { .. } => Vec::new(),
            },
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct SectionSelector {
    matcher: Matcher,
}

#[derive(Debug, PartialEq)]
pub struct SubstringMatcher {
    // TODO move to matcher.rs. Or maybe just rm? I have it here in case I want to add anchors later, but YAGNI, and
    // I can add a struct later if I need it.
    pub look_for: String,
}

impl Selector {
    pub fn parse(text: &str) -> Result<Vec<Selector>, ParseError> {
        let mut iter = ParsingIterator::new(text.chars());
        let mut selectors = Vec::with_capacity(5); // just a guess

        loop {
            iter.drop_while(|ch| ch.is_whitespace());
            if iter.peek().is_none() {
                break;
            }
            let selector = Self::parse_selector(&mut iter).map_err(|reason| ParseError {
                reason,
                position: iter.input_position(),
            })?;
            selectors.push(selector);
        }

        Ok(selectors)
    }

    fn parse_selector<C: Iterator<Item = char>>(chars: &mut ParsingIterator<C>) -> ParseResult<Selector> {
        chars.drop_while(|ch| ch.is_whitespace()); // should already be the case, but this is cheap and future-proof
        match chars.next() {
            None => Ok(Selector::Any), // unexpected, but future-proof
            Some('#') => Self::parse_header(chars),
            Some('-') => ListItemSelector::read(ListItemType::Unordered, chars),
            Some('1') => ListItemSelector::read(ListItemType::Ordered, chars),

            Some(other) => Err(ParseErrorReason::UnexpectedCharacter(other)), // TODO should be Any w/ bareword if first char is a letter
        }
    }

    fn parse_header<C: Iterator<Item = char>>(chars: &mut ParsingIterator<C>) -> ParseResult<Selector> {
        require_whitespace(chars, "Section specifier")?;
        let matcher = Matcher::parse_matcher(chars)?;
        Ok(Selector::Section(SectionSelector { matcher }))
    }
}

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
    fn matches(&self, idx: &Option<u32>) -> bool {
        match self {
            ListItemType::Ordered => idx.is_some(),
            ListItemType::Unordered => idx.is_none(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum CheckboxSpecifier {
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

fn require_whitespace<C: Iterator<Item = char>>(chars: &mut ParsingIterator<C>, description: &str) -> ParseResult<()> {
    if chars.drop_while(|ch| ch.is_whitespace()).is_empty() {
        return Err(ParseErrorReason::InvalidSyntax(format!(
            "{} must be followed by whitespace",
            description
        )));
    } else {
        Ok(())
    }
}

impl ListItemSelector {
    fn read<C: Iterator<Item = char>>(li_type: ListItemType, chars: &mut ParsingIterator<C>) -> ParseResult<Selector> {
        if matches!(li_type, ListItemType::Ordered) {
            if chars.next() != Some('.') {
                return Err(ParseErrorReason::InvalidSyntax(
                    "Ordered list item specifier must start with \"1.\"".to_string(),
                ));
            }
        }
        require_whitespace(chars, "List item specifier")?;
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
        let string_matcher = Matcher::parse_matcher(chars)?;

        Ok(Selector::ListItem(ListItemSelector {
            li_type,
            checkbox,
            string_matcher,
        }))
    }

    fn matches(&self, actual_idx: &Option<u32>, actual_item: &ListItem) -> bool {
        if !self.li_type.matches(actual_idx) {
            return false;
        }
        if !self.checkbox.matches(&actual_item.checked) {
            return false;
        }
        self.string_matcher.matches_any(&actual_item.item)
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parse_common::parse_and_check_mapped;

    #[test]
    fn section() {
        parse_and_check(
            "# foo",
            Selector::Section(SectionSelector {
                matcher: Matcher::Substring(SubstringMatcher {
                    look_for: "foo".to_string(),
                }),
            }),
            "",
        );

        parse_and_check("# ", Selector::Section(SectionSelector { matcher: Matcher::Any }), "");

        parse_and_check(
            "# | next",
            Selector::Section(SectionSelector { matcher: Matcher::Any }),
            " next",
        );
    }

    mod list_item {
        use super::*;
        use crate::select::test::parse_and_check;

        // TODO need negative tests

        #[test]
        fn ordered_no_checkbox() {
            parse_and_check(
                "1. foo",
                Selector::ListItem(ListItemSelector {
                    li_type: ListItemType::Ordered,
                    checkbox: CheckboxSpecifier::NoCheckbox,
                    string_matcher: Matcher::Substring(SubstringMatcher {
                        look_for: "foo".to_string(),
                    }),
                }),
                "",
            );
        }

        #[test]
        fn unordered_no_checkbox() {
            parse_and_check(
                "- foo",
                Selector::ListItem(ListItemSelector {
                    li_type: ListItemType::Unordered,
                    checkbox: CheckboxSpecifier::NoCheckbox,
                    string_matcher: Matcher::Substring(SubstringMatcher {
                        look_for: "foo".to_string(),
                    }),
                }),
                "",
            );
        }

        #[test]
        fn unordered_unchecked() {
            parse_and_check(
                "- [ ] foo",
                Selector::ListItem(ListItemSelector {
                    li_type: ListItemType::Unordered,
                    checkbox: CheckboxSpecifier::CheckboxUnchecked,
                    string_matcher: Matcher::Substring(SubstringMatcher {
                        look_for: "foo".to_string(),
                    }),
                }),
                "",
            );
        }

        #[test]
        fn unordered_checked() {
            parse_and_check(
                "- [x] foo",
                Selector::ListItem(ListItemSelector {
                    li_type: ListItemType::Unordered,
                    checkbox: CheckboxSpecifier::CheckboxChecked,
                    string_matcher: Matcher::Substring(SubstringMatcher {
                        look_for: "foo".to_string(),
                    }),
                }),
                "",
            );
        }

        #[test]
        fn unordered_maybe_checked() {
            parse_and_check(
                "- [?] foo",
                Selector::ListItem(ListItemSelector {
                    li_type: ListItemType::Unordered,
                    checkbox: CheckboxSpecifier::CheckboxEither,
                    string_matcher: Matcher::Substring(SubstringMatcher {
                        look_for: "foo".to_string(),
                    }),
                }),
                "",
            );
        }
    }

    fn parse_and_check(text: &str, expect: Selector, expect_remaining: &str) {
        parse_and_check_mapped(text, expect, expect_remaining, |iter| Selector::parse_selector(iter))
    }
}
