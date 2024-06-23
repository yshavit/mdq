use crate::parse_common::Position;
use crate::parsing_iter::ParsingIterator;
use crate::select::base::Selector;
use crate::select::sel_link::LinkSelector;
use crate::select::sel_list_item::{ListItemSelector, ListItemType};
use crate::select::sel_section::SectionSelector;
use crate::tree::{Formatting, Image, Inline, Link, MdElem, Text};
use crate::tree_ref::{ListItemRef, MdElemRef};
use std::fmt::{Display, Formatter};

pub enum SelectResult<'a> {
    One(MdElemRef<'a>),
    Multi(&'a Vec<MdElem>),
}

pub type ParseResult<T> = Result<T, ParseErrorReason>;

pub const SELECTOR_SEPARATOR: char = '|';

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub position: Position,
    pub reason: ParseErrorReason,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseErrorReason {
    Expected(char),
    UnexpectedCharacter(char),
    UnexpectedEndOfInput,
    InvalidSyntax(String),
}

impl Display for ParseErrorReason {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseErrorReason::Expected(ch) => write!(f, "expected \"{}\"", ch),
            ParseErrorReason::UnexpectedCharacter(ch) => write!(f, "unexpected character \"{}\"", ch),
            ParseErrorReason::UnexpectedEndOfInput => write!(f, "unexpected end of input"),
            ParseErrorReason::InvalidSyntax(s) => write!(f, "{}", s),
        }
    }
}

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

    // TODO docs
    Link(LinkSelector),
}

impl MdqRefSelector {
    pub fn parse(text: &str) -> Result<Vec<Self>, ParseError> {
        let mut iter = ParsingIterator::new(text);
        let mut selectors = Vec::with_capacity(5); // just a guess

        loop {
            iter.drop_whitespace();
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

    pub fn find_nodes<'a>(&self, nodes: Vec<MdElemRef<'a>>) -> Vec<MdElemRef<'a>> {
        let mut result = Vec::with_capacity(8); // arbitrary guess
        for node in nodes {
            self.build_output(&mut result, node);
        }
        result
    }

    fn build_output<'a>(&self, out: &mut Vec<MdElemRef<'a>>, node: MdElemRef<'a>) {
        let result = match (self, node.clone()) {
            (MdqRefSelector::Section(selector), MdElemRef::Section(header)) => selector.try_select(header),
            (MdqRefSelector::ListItem(selector), MdElemRef::ListItem(item)) => selector.try_select(item),
            (MdqRefSelector::Link(selector), MdElemRef::Link(item)) => selector.try_select(item),
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
    fn find_children<'a>(node: MdElemRef) -> Vec<MdElemRef> {
        match node {
            MdElemRef::Section(s) => MdElemRef::wrap_vec(&s.body),
            MdElemRef::ListItem(ListItemRef(_, item)) => MdElemRef::wrap_vec(&item.item),
            MdElemRef::Paragraph(p) => p.body.iter().map(|child| MdElemRef::Inline(child)).collect(),
            MdElemRef::BlockQuote(b) => MdElemRef::wrap_vec(&b.body),
            MdElemRef::List(list) => {
                let mut idx = list.starting_index;
                let mut result = Vec::with_capacity(list.items.len());
                for item in &list.items {
                    result.push(MdElemRef::ListItem(ListItemRef(idx.clone(), item)));
                    if let Some(idx) = idx.as_mut() {
                        *idx += 1;
                    }
                }
                result
            }
            MdElemRef::Table(table) => {
                let count_estimate = table.rows.len() * table.rows.first().map(|tr| tr.len()).unwrap_or(0);
                let mut result = Vec::with_capacity(count_estimate);
                for row in &table.rows {
                    for col in row {
                        for cell in col {
                            result.push(MdElemRef::Inline(cell));
                        }
                    }
                }
                result
            }
            MdElemRef::ThematicBreak | MdElemRef::CodeBlock(_) => Vec::new(),
            MdElemRef::Inline(inline) => match inline {
                Inline::Formatting(Formatting { children, .. }) => {
                    children.iter().map(|child| MdElemRef::Inline(child)).collect()
                }
                Inline::Footnote(footnote) => MdElemRef::wrap_vec(&footnote.text),
                Inline::Link(link) => vec![MdElemRef::Link(link)], // TODO find a test case that hits this to make sure it doesn't infinite-loop!
                Inline::Text(Text { .. }) | Inline::Image(Image { .. }) => Vec::new(),
            },

            MdElemRef::Link(Link { text, .. }) => text.iter().map(|child| MdElemRef::Inline(child)).collect(),
        }
    }

    fn parse_selector(chars: &mut ParsingIterator) -> ParseResult<Self> {
        chars.drop_whitespace(); // should already be the case, but this is cheap and future-proof
        match chars.next() {
            None => Ok(MdqRefSelector::Any), // unexpected, but future-proof
            Some('#') => Ok(Self::Section(SectionSelector::read(chars)?)),
            Some('1') => Ok(Self::ListItem(ListItemType::Ordered.read(chars)?)),
            Some('-') => Ok(Self::ListItem(ListItemType::Unordered.read(chars)?)),
            Some('[') => Ok(Self::Link(LinkSelector::read(chars)?)),

            Some(other) => Err(ParseErrorReason::UnexpectedCharacter(other)), // TODO should be Any w/ bareword if first char is a letter
        }
    }
}
