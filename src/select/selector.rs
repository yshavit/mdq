use crate::parsing_iter::ParsingIterator;
use crate::select::base::Selector;
use crate::select::interface::{ParseError, ParseErrorReason, ParseResult, SelectResult};
use crate::select::list_item::{ListItemSelector, ListItemType};
use crate::select::section::SectionSelector;
use crate::tree::Inline;
use crate::tree_ref::{ListItemRef, MdqNodeRef};
use crate::wrap_mdq_refs;

#[derive(Debug, PartialEq)]
pub enum MdqRefSelector {
    Any,

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

impl MdqRefSelector {
    pub fn parse(text: &str) -> Result<Vec<Self>, ParseError> {
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

    pub fn find_nodes<'a>(&self, nodes: Vec<MdqNodeRef<'a>>) -> Vec<MdqNodeRef<'a>> {
        let mut result = Vec::with_capacity(8); // arbitrary guess
        for node in nodes {
            self.build_output(&mut result, node);
        }
        result
    }

    fn build_output<'a>(&self, out: &mut Vec<MdqNodeRef<'a>>, node: MdqNodeRef<'a>) {
        let result = match (self, node.clone()) {
            (MdqRefSelector::Section(selector), MdqNodeRef::Section(header)) => selector.try_select(&header),
            (MdqRefSelector::ListItem(selector), MdqNodeRef::ListItem(item)) => selector.try_select(item),
            _ => None,
        };
        match result {
            Some(SelectResult::One(found)) => out.push(found),
            Some(SelectResult::Multi(found)) => found.iter().for_each(|item| out.push(item.into())),
            None => {
                for child in Self::find_children(node) {
                    self.build_output(out, child);
                }
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
                    result.push(MdqNodeRef::ListItem(ListItemRef(idx.clone(), item)));
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
            MdqNodeRef::ListItem(ListItemRef(_, item)) => MdqNodeRef::wrap_vec(&item.item),
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

    fn parse_selector<C: Iterator<Item = char>>(chars: &mut ParsingIterator<C>) -> ParseResult<Self> {
        chars.drop_while(|ch| ch.is_whitespace()); // should already be the case, but this is cheap and future-proof
        match chars.next() {
            None => Ok(MdqRefSelector::Any), // unexpected, but future-proof
            Some('#') => Ok(Self::Section(SectionSelector::read(chars)?)),
            Some('1') => Ok(Self::ListItem(ListItemType::Ordered.read(chars)?)),
            Some('-') => Ok(Self::ListItem(ListItemType::Unordered.read(chars)?)),

            Some(other) => Err(ParseErrorReason::UnexpectedCharacter(other)), // TODO should be Any w/ bareword if first char is a letter
        }
    }
}
