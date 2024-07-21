use crate::parse_common::Position;
use crate::parsing_iter::ParsingIterator;
use crate::select::base::Selector;
use crate::select::sel_block_quote::BlockQuoteSelector;
use crate::select::sel_code_block::CodeBlockSelector;
use crate::select::sel_image::ImageSelector;
use crate::select::sel_link::LinkSelector;
use crate::select::sel_list_item::ListItemSelector;
use crate::select::sel_list_item::ListItemType;
use crate::select::sel_section::SectionSelector;
use crate::tree::{Formatting, Inline, Link, Text};
use crate::tree_ref::{ListItemRef, MdElemRef};
use std::fmt::{Display, Formatter};

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
    InvalidEscape,
}

impl Display for ParseErrorReason {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseErrorReason::Expected(ch) => write!(f, "expected \"{}\"", ch),
            ParseErrorReason::UnexpectedCharacter(ch) => write!(f, "unexpected character \"{}\"", ch),
            ParseErrorReason::UnexpectedEndOfInput => write!(f, "unexpected end of input"),
            ParseErrorReason::InvalidSyntax(s) => write!(f, "{}", s),
            ParseErrorReason::InvalidEscape => write!(f, "invalid escape sequence"),
        }
    }
}

macro_rules! selectors {
    [
        $($(#[$meta:meta])*
        $({$($char:literal $(=>$($read_variant:ident)::+)? ),+})?
        $(! {$($bang_char:literal $(=>$($bang_read_variant:ident)::+)? ),+})?
        $name:ident),* $(,)?
    ] => {
        #[derive(Debug, PartialEq)]
        pub enum MdqRefSelector {
            $(
                $(#[$meta])*
                $name ( paste::paste!{[<$name Selector >]}),
            )*
        }

        impl MdqRefSelector {
            fn try_select_node<'a>(&self, node: MdElemRef<'a>) -> Option<MdElemRef<'a>> {
                match (self, node) {
                    $(
                    (Self::$name(selector), MdElemRef::$name(elem)) => selector.try_select(elem),
                    )*
                    _ => None
                }
            }

            fn parse_selector(chars: &mut ParsingIterator) -> ParseResult<Self> {
                chars.drop_whitespace(); // should already be the case, but this is cheap and future-proof
                match chars.next() {
                    None => Err(ParseErrorReason::UnexpectedEndOfInput),
                    $(
                        $(
                            $(
                                Some($char) => paste::paste!{ Ok(Self::$name([<$name Selector>]::read($( $($read_variant)::+ ,)?chars)?))},
                            )+
                        )?
                    )*
                    Some('!') => {
                        match chars.peek() {
                            $(
                                $(
                                    $(
                                        Some($bang_char) => {
                                            let _ = chars.next(); // drop the peeked char
                                            paste::paste!{ Ok(Self::$name([<$name Selector>]::read($( $($bang_read_variant)::+ ,)?chars)?))}
                                        }
                                    )+
                                )?
                            )*
                            Some(other) => Err(ParseErrorReason::UnexpectedCharacter(other)), // reserved for functions
                            None => Err(ParseErrorReason::UnexpectedEndOfInput),
                        }
                    }
                    Some(other) => Err(ParseErrorReason::UnexpectedCharacter(other)),
                }
            }
        }
    };
}

selectors![
    /// Selects the content of a section identified by its heading.
    ///
    /// Format: `# <string_matcher>`
    ///
    /// In bareword form, the string matcher terminates with the
    /// [selector delimiter character](parse_common::SELECTOR_SEPARATOR).
    {'#'} Section,

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
    {
        '1' => ListItemType::Ordered,
        '-' => ListItemType::Unordered
    } ListItem,

    {'['} Link,
    ! {'['} Image,

    {'>'} BlockQuote,

    {'`'} CodeBlock,
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
            // parse_selector  is defined in macro_helpers::selectors!
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
            Some(found) => out.push(found),
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
            MdElemRef::Doc(body) => {
                let mut wrapped = Vec::with_capacity(body.len());
                for elem in body {
                    wrapped.push(elem.into());
                }
                wrapped
            }
            MdElemRef::Section(s) => vec![MdElemRef::Doc(&s.body)],
            MdElemRef::ListItem(ListItemRef(_, item)) => vec![MdElemRef::Doc(&item.item)],
            MdElemRef::Paragraph(p) => p.body.iter().map(|child| MdElemRef::Inline(child)).collect(),
            MdElemRef::BlockQuote(b) => vec![MdElemRef::Doc(&b.body)],
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
                Inline::Footnote(footnote) => vec![MdElemRef::Doc(&footnote.text)],
                Inline::Link(link) => vec![MdElemRef::Link(link)],
                Inline::Image(image) => vec![MdElemRef::Image(image)],
                Inline::Text(Text { .. }) => Vec::new(),
            },

            MdElemRef::Link(Link { text, .. }) => text.iter().map(|child| MdElemRef::Inline(child)).collect(),
            MdElemRef::Image(_) => Vec::new(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    mod single_selector_parse {
        use super::*;
        use crate::variants_checker;

        #[test]
        fn section() {
            let input = "#";
            let mdq_ref_sel_parsed = MdqRefSelector::parse_selector(&mut ParsingIterator::new(input));
            let section_sel_parsed = SectionSelector::read(&mut ParsingIterator::new(&input[1..])).unwrap();
            expect_ok(mdq_ref_sel_parsed, MdqRefSelector::Section(section_sel_parsed));
        }

        #[test]
        fn ordered_list() {
            let input = "1.";
            let mdq_ref_sel_parsed = MdqRefSelector::parse_selector(&mut ParsingIterator::new(input));
            let item_parsed =
                ListItemSelector::read(ListItemType::Ordered, &mut ParsingIterator::new(&input[1..])).unwrap();
            expect_ok(mdq_ref_sel_parsed, MdqRefSelector::ListItem(item_parsed));
        }

        #[test]
        fn unordered_list() {
            let input = "-";
            let mdq_ref_sel_parsed = MdqRefSelector::parse_selector(&mut ParsingIterator::new(input));
            let item_parsed =
                ListItemSelector::read(ListItemType::Unordered, &mut ParsingIterator::new(&input[1..])).unwrap();
            expect_ok(mdq_ref_sel_parsed, MdqRefSelector::ListItem(item_parsed));
        }

        #[test]
        fn link() {
            let input = "[]()";
            let mdq_ref_sel_parsed = MdqRefSelector::parse_selector(&mut ParsingIterator::new(input));
            let item_parsed = LinkSelector::read(&mut ParsingIterator::new(&input[1..])).unwrap();
            expect_ok(mdq_ref_sel_parsed, MdqRefSelector::Link(item_parsed));
        }

        #[test]
        fn image() {
            let input = "![]()";
            let mdq_ref_sel_parsed = MdqRefSelector::parse_selector(&mut ParsingIterator::new(input));
            // note: input[2..] because parse_selector reads both the '!' and the '['
            let item_parsed = ImageSelector::read(&mut ParsingIterator::new(&input[2..])).unwrap();
            expect_ok(mdq_ref_sel_parsed, MdqRefSelector::Image(item_parsed));
        }

        #[test]
        fn block_quote() {
            let input = ">";
            let mdq_ref_sel_parsed = MdqRefSelector::parse_selector(&mut ParsingIterator::new(input));
            let item_parsed = BlockQuoteSelector::read(&mut ParsingIterator::new(&input[1..])).unwrap();
            expect_ok(mdq_ref_sel_parsed, MdqRefSelector::BlockQuote(item_parsed));
        }

        #[test]
        fn code_block() {
            let input = "```";
            let mdq_ref_sel_parsed = MdqRefSelector::parse_selector(&mut ParsingIterator::new(input));
            let item_parsed = CodeBlockSelector::read(&mut ParsingIterator::new(&input[1..])).unwrap();
            expect_ok(mdq_ref_sel_parsed, MdqRefSelector::CodeBlock(item_parsed));
        }

        #[test]
        fn code_block_only_two_backticks() {
            let input = "``";
            let mdq_ref_sel_parsed = MdqRefSelector::parse_selector(&mut ParsingIterator::new(input));
            assert_eq!(mdq_ref_sel_parsed, Err(ParseErrorReason::Expected('`')));
        }

        #[test]
        fn unknown() {
            let input = "\u{2603}";
            let mdq_ref_sel_parsed = MdqRefSelector::parse_selector(&mut ParsingIterator::new(input));
            assert_eq!(
                mdq_ref_sel_parsed,
                Err(ParseErrorReason::UnexpectedCharacter('\u{2603}'))
            );
        }

        fn expect_ok(actual: ParseResult<MdqRefSelector>, expected: MdqRefSelector) {
            actual.iter().for_each(|s| CHECKER.see(s));
            assert_eq!(actual, Ok(expected))
        }

        variants_checker!(CHECKER = MdqRefSelector{
            Section(_),
            ListItem(_),
            Link(_),
            Image(_),
            BlockQuote(_),
            CodeBlock(_),
        });
    }

    #[test]
    fn pipes_smoke_test() {
        // - leading pipes are discarded
        // - block quote selector
        // - empty selectors are disregarded
        // - section selector
        // - trailing selectors are disregarded
        let selected = MdqRefSelector::parse("| > || # |||");
        let expect = vec![
            MdqRefSelector::BlockQuote(BlockQuoteSelector::read(&mut ParsingIterator::new("")).unwrap()),
            MdqRefSelector::Section(SectionSelector::read(&mut ParsingIterator::new("")).unwrap()),
        ];
        assert_eq!(selected, Ok(expect));
    }

    /// Only a smoke test, because the code is pretty straightforward, and I don't feel like writing more. :-)
    mod find_children_smoke {
        use crate::mdq_inline;
        use crate::select::MdqRefSelector;
        use crate::tree::{Inline, Link, LinkDefinition, LinkReference, Text, TextVariant};
        use crate::tree_ref::MdElemRef;

        #[test]
        fn link_direct() {
            let link = Link {
                text: vec![mdq_inline!("link text")],
                link_definition: LinkDefinition {
                    url: "https://example.com".to_string(),
                    title: None,
                    reference: LinkReference::Inline,
                },
            };
            let node_ref = MdElemRef::Link(&link);
            let children = MdqRefSelector::find_children(node_ref);
            assert_eq!(
                children,
                vec![MdElemRef::Inline(&Inline::Text(Text {
                    variant: TextVariant::Plain,
                    value: "link text".to_string(),
                }))]
            );
        }

        #[test]
        fn link_via_inlines() {
            fn mk_link() -> Link {
                Link {
                    text: vec![mdq_inline!("link text")],
                    link_definition: LinkDefinition {
                        url: "https://example.com".to_string(),
                        title: None,
                        reference: LinkReference::Inline,
                    },
                }
            }
            let inline = Inline::Link(mk_link());
            let node_ref = MdElemRef::Inline(&inline);
            let children = MdqRefSelector::find_children(node_ref);
            assert_eq!(children, vec![MdElemRef::Link(&mk_link()),]);
        }
    }
}
