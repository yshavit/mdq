use crate::fmt_str::inlines_to_plain_string;
use crate::matcher::Matcher;
use crate::parse_common::{ParseError, ParseErrorReason, ParseResult};
use crate::parsing_iter::ParsingIterator;
use crate::tree::Inline;
use crate::tree_ref::MdqNodeRef;
use crate::wrap_mdq_refs;

#[derive(Debug, PartialEq)]
pub enum Selector {
    Any,
    // List(Matcher, ListType, Selected),  // should this be list or list item?
    // Image { text: Matcher, href: Matcher },
    // Link { text: Matcher, href: Matcher },
    // CodeBlock(Matcher),
    Heading(HeadingSelector),
    // TODO I need an "any", or maybe just a "paragraph", or maybe both
    // TODO does it make sense to select on a block quote?
}

impl Selector {
    pub fn find_nodes<'a>(&self, nodes: Vec<MdqNodeRef<'a>>) -> Vec<MdqNodeRef<'a>> {
        let mut result = Vec::with_capacity(8); // arbitrary guess
        for node in nodes {
            self.find_nodes_one(&mut result, node);
        }
        result
    }

    // TODO need better name -- here but also in all the other methods
    pub fn find_nodes_one<'a>(&self, out: &mut Vec<MdqNodeRef<'a>>, node: MdqNodeRef<'a>) {
        match (self, node) {
            (Selector::Heading(selector), MdqNodeRef::Section(header)) => {
                let header_text = inlines_to_plain_string(&header.title);
                if selector.matcher.matches(&header_text) {
                    header.body.iter().for_each(|child| out.push(child.into()));
                }
            }
            (_, node) => {
                for child in Self::find_children(node) {
                    self.find_nodes_one(out, child);
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
                link @ Inline::Link { .. } => {
                    vec![MdqNodeRef::Inline(link)]
                }
                Inline::Text { .. } | Inline::Image { .. } => Vec::new(),
            },
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct HeadingSelector {
    matcher: Matcher,
}

#[derive(Debug, PartialEq)]
pub struct SubstringMatcher {
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

            Some(other) => Err(ParseErrorReason::UnexpectedCharacter(other)),
        }
    }

    fn parse_header<C: Iterator<Item = char>>(chars: &mut ParsingIterator<C>) -> ParseResult<Selector> {
        let matcher = Matcher::parse_matcher(chars)?;
        Ok(Selector::Heading(HeadingSelector { matcher }))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parse_common::parse_and_check_mapped;

    #[test]
    fn header() {
        parse_and_check(
            "# foo",
            Selector::Heading(HeadingSelector {
                matcher: Matcher::Substring(SubstringMatcher {
                    look_for: "foo".to_string(),
                }),
            }),
            "",
        );

        parse_and_check("# ", Selector::Heading(HeadingSelector { matcher: Matcher::Any }), "");

        parse_and_check(
            "# | next",
            Selector::Heading(HeadingSelector { matcher: Matcher::Any }),
            " next",
        );
    }

    fn parse_and_check(text: &str, expect: Selector, expect_remaining: &str) {
        parse_and_check_mapped(text, expect, expect_remaining, |iter| Selector::parse_selector(iter))
    }
}
