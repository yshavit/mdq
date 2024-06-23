use crate::parse_common::Position;
use crate::parsing_iter::ParsingIterator;
use crate::select::base::Selector;
use crate::select::sel_image::ImageSelector;
use crate::select::sel_link::LinkSelector;
use crate::select::sel_list_item::ListItemSelector;
use crate::select::sel_list_item::ListItemType;
use crate::select::sel_section::SectionSelector;
use crate::tree::{Formatting, Inline, Link, MdElem, Text};
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

macro_rules! selectors {
    // TODO can I replace $bang:literal with literally just a "!"?
    [$($(#[$meta:meta])* $(!)? {$($char:literal $(::$($read_variant:ident)::+)? ),+} $name:ident),* $(,)?] => {
        #[derive(Debug, PartialEq)]
        pub enum MdqRefSelector {
            $(
                $(#[$meta])*
                 $name ( paste::paste!{[<$name Selector >]}),
            )*
        }

        impl MdqRefSelector {
            fn try_select_node<'a>(&self, node: MdElemRef<'a>) -> Option<SelectResult<'a>> {
                match (self, node) {
                    $(
                    (Self::$name(selector), MdElemRef::$name(elem)) => selector.try_select(elem),
                    )*
                    _ => None
                }
            }

            fn my_parse(chars: &mut ParsingIterator) -> ParseResult<Self> {
                chars.drop_whitespace(); // should already be the case, but this is cheap and future-proof
                match chars.next() {
                    None => Err(ParseErrorReason::UnexpectedEndOfInput),
                    $(
                        $(
                            Some($char) => paste::paste!{ Ok(Self::$name([<$name Selector>]::read($( $($read_variant)::+ ,)?chars)?))},
                        )+
                    )*
                    Some(other) => Err(ParseErrorReason::UnexpectedCharacter(other)), // TODO should be Any w/ bareword if first char is a letter
                }
            }
        }
    };
}

selectors![
    // TODO rust-doc-ify these; I'm just commenting them out for now to remove noise while debugging a macro
    // Selects the content of a section identified by its heading.
    //
    // Format: `# <string_matcher>`
    //
    // In bareword form, the string matcher terminates with the
    // [selector delimiter character](parse_common::SELECTOR_SEPARATOR).
    {'#'} Section,

    // Selects a list item.
    //
    // Format: `<type> [checkbox] <string_matcher>` where:
    // - _type_ is either `-` for unordered lists or `1.` for ordered lists. Note that ordered lists are _only_
    //   identified by `1.`. Other digits are invalid.
    // - _checkbox_, if provided, must be one of:
    //   - `[ ]` for an unchecked box
    //   - `[x]` for a checked box
    //   - `[?]` for a box that may be checked or unchecked
    //
    // If the checkbox specifier is provided, the selector will only select list items with a checkbox. If the
    // checkbox specifier is omitted, the selector will only select list items without a checkbox.
    //
    // In bareword form, the string matcher terminates with the [selector delimiter character](SELECTOR_SEPARATOR).
    {'1'::ListItemType::Ordered,'-'::ListItemType::Unordered} ListItem,

    {'['} Link,
    ! {'!'} Image,
];

impl MdqRefSelector {
    pub fn parse(text: &str) -> Result<Vec<Self>, ParseError> {
        let mut iter = ParsingIterator::new(text);
        let mut selectors = Vec::with_capacity(5); // just a guess

        loop {
            iter.drop_whitespace();
            match iter.peek() {
                None => break,
                Some(SELECTOR_SEPARATOR) => {
                    // This is usually just the separator between selectors: "# foo | # bar" comes through in this loop
                    // as "#f foo", then "|", then "#bar" -- so this condition is that middle bar.
                    // The reason we want this here, as opposed to after the Self::parse_selector, is that if we did it
                    // there, we'd also have to guard against end-of-input -- and we'd need to appropriately drop
                    // whitespace. With this approach, we just march on.
                    //
                    // This block can also happen if you have multiple bars in a row: "#foo | | | #bar". Those are all
                    // valid Any selectors, which we can just elide away (not for performance reasons, but because it's
                    // more convenient to).
                    let _ = iter.next(); // consume the separator
                    continue;
                }
                _ => {}
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
        // try_select_node is defined in macro_helpers::selectors!
        match self.try_select_node(node) {
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
                Inline::Image(image) => vec![MdElemRef::Image(image)],
                Inline::Text(Text { .. }) => Vec::new(),
            },

            MdElemRef::Link(Link { text, .. }) => text.iter().map(|child| MdElemRef::Inline(child)).collect(),
            MdElemRef::Image(_) => Vec::new(),
        }
    }

    fn parse_selector(chars: &mut ParsingIterator) -> ParseResult<Self> {
        Self::my_parse(chars)
        // chars.drop_whitespace(); // should already be the case, but this is cheap and future-proof
        // match chars.next() {
        //     None => todo!("not yet implemented: should be Any w/ a matcher"),
        //     Some('#') => Ok(Self::Section(SectionSelector::read(chars)?)),
        //     // Some('1') => Ok(Self::ListItem(ListItemType::Ordered.read(chars)?)),
        //     // Some('-') => Ok(Self::ListItem(ListItemType::Unordered.read(chars)?)),
        //     Some('[') => {
        //         let selector = LinkSelector::read(chars)?;
        //         let selector_variant = Self::Link(selector);
        //         Ok(selector_variant)
        //     }
        //     Some('!') => {
        //         match chars.peek() {
        //             Some('[') => {
        //                 let _ = chars.next();
        //                 Ok(Self::Image(ImageSelector::read(chars)?))
        //             }
        //             Some(other) => Err(ParseErrorReason::UnexpectedCharacter(other)), // reserved for functions
        //             None => Err(ParseErrorReason::UnexpectedEndOfInput),
        //         }
        //     }
        //
        //     Some(other) => Err(ParseErrorReason::UnexpectedCharacter(other)), // TODO should be Any w/ bareword if first char is a letter
        // }
    }
}

/*

*/
