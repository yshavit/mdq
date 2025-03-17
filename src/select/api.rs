use crate::query::query::{Query, Rule};
use crate::query::selectors::Selector as ParsedSelector;
use crate::select::sel_link_like::{ImageSelector, LinkSelector};
use crate::select::sel_list_item::ListItemSelector;
use crate::select::sel_section::SectionSelector;
use crate::tree::{FootnoteId, Formatting, Inline, Link, MdContext, Text, TextVariant};
use crate::tree_ref::{HtmlRef, ListItemRef, MdElemRef};
use pest::error::Error;
use std::collections::HashSet;

pub trait Selector<'md, I: Into<MdElemRef<'md>>> {
    fn try_select(&self, item: I) -> Option<MdElemRef<'md>>;
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub pest_err: Error<Rule>,
}

// pub struct MdqRefSelector {
//     // TODO rename to "SelectorAdapter"
//     selector: ParsedSelector,
// }
//
// macro_rules! adapters {
//     ($($name:ident $(| $alias:ident)*),+ ,) => {
//         impl MdqRefSelector {
//             fn try_select_node<'md>(&self, node: MdElemRef<'md>) -> Option<MdElemRef<'md>> {
//                 match (&self.selector, node) {
//                     $(
//                     (crate::query::selectors::Selector::$name(selector), MdElemRef::$name(elem)) => Self::build_adapter(selector).try_select(elem),
//                     $( (crate::query::selectors::Selector::$name(selector), MdElemRef::$alias(elem)) => Self::build_adapter(selector).try_select(elem.into()), )?
//                     )*
//                     _ => None
//                 }
//             }
//
//             fn build_adapter<'md, I, S>(parsed_selector: &ParsedSelector) -> S
//             where
//                 I: Into<MdElemRef<'md>>,
//                 S: Into<Selector<'md, I>>
//             {
//                 match parsed_selector {
//                     _ => todo!(),
//                 }
//             }
//         }
//     };
// }
//
// adapters! {
//     Table,
// }

pub enum SelectorAdapter {
    Section(SectionSelector),
    ListItem(ListItemSelector),
    Link(LinkSelector),
    Image(ImageSelector),
    // todo the rest
}

impl From<ParsedSelector> for SelectorAdapter {
    fn from(value: ParsedSelector) -> Self {
        match value {
            ParsedSelector::Section(matcher) => Self::Section(matcher.into()),
            ParsedSelector::ListItem(li) => Self::ListItem(li.into()),
            ParsedSelector::Link(link_like) => Self::Link(link_like.into()),
            ParsedSelector::Image(link_like) => Self::Image(link_like.into()),
            _ => todo!(),
        }
    }
}

impl SelectorAdapter {
    fn try_select_node<'md>(&self, node: MdElemRef<'md>) -> Option<MdElemRef<'md>> {
        match (&self, node) {
            (Self::Section(selector), MdElemRef::Section(elem)) => selector.try_select(elem),
            (Self::ListItem(selector), MdElemRef::ListItem(elem)) => selector.try_select(elem),
            (Self::Link(selector), MdElemRef::Link(elem)) => selector.try_select(elem),
            (Self::Image(selector), MdElemRef::Image(elem)) => selector.try_select(elem),
            // ...
            _ => None,
        }
    }
}

impl SelectorAdapter {
    pub fn parse(text: &str) -> Result<Vec<Self>, ParseError> {
        let parsed = Query::parse(text).map_err(|pest_err| ParseError { pest_err })?;
        let parsed_selectors = ParsedSelector::from_top_pairs(parsed).unwrap(); // TODO do I need something better than unwrap? if not, I should just unwrap within from_top_pairs.
        Ok(parsed_selectors.into_iter().map(|s| s.into()).collect())
    }

    pub fn find_nodes<'md>(&self, ctx: &'md MdContext, nodes: Vec<MdElemRef<'md>>) -> Vec<MdElemRef<'md>> {
        let mut result = Vec::with_capacity(8); // arbitrary guess
        let mut search_context = SearchContext::new(ctx);
        for node in nodes {
            self.build_output(&mut result, &mut search_context, node);
        }
        result
    }

    fn build_output<'md>(&self, out: &mut Vec<MdElemRef<'md>>, ctx: &mut SearchContext<'md>, node: MdElemRef<'md>) {
        // GH #168 can we remove the clone()? Maybe by having try_select_node take a reference.
        match self.try_select_node(node.clone()) {
            Some(found) => out.push(found),
            None => {
                for child in Self::find_children(ctx, node) {
                    self.build_output(out, ctx, child);
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
    fn find_children<'md>(ctx: &mut SearchContext<'md>, node: MdElemRef<'md>) -> Vec<MdElemRef<'md>> {
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
            MdElemRef::Table(table) => Self::find_children(ctx, MdElemRef::TableSlice(table.into())),
            MdElemRef::TableSlice(table) => {
                let rows = table.rows();
                let first_row_cols = rows.first().map(Vec::len).unwrap_or(0);
                let count_estimate = rows.len() * first_row_cols;
                let mut result = Vec::with_capacity(count_estimate);
                for row in table.rows() {
                    for maybe_col in row {
                        if let Some(col) = maybe_col {
                            for cell in *col {
                                result.push(MdElemRef::Inline(cell));
                            }
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
                Inline::Footnote(footnote) => {
                    // guard against cycles
                    if ctx.seen_footnotes.insert(footnote) {
                        vec![MdElemRef::Doc(ctx.md_context.get_footnote(&footnote))]
                    } else {
                        Vec::new()
                    }
                }
                Inline::Link(link) => vec![MdElemRef::Link(link)],
                Inline::Image(image) => vec![MdElemRef::Image(image)],
                Inline::Text(Text { variant, value }) if variant == &TextVariant::Html => {
                    vec![MdElemRef::Html(HtmlRef(value))]
                }
                Inline::Text(Text { .. }) => Vec::new(),
            },

            MdElemRef::Link(Link { text, .. }) => text.iter().map(|child| MdElemRef::Inline(child)).collect(),
            MdElemRef::Image(_) => Vec::new(),
            MdElemRef::Html(_) => Vec::new(),
        }
    }
}

struct SearchContext<'md> {
    md_context: &'md MdContext,
    seen_footnotes: HashSet<&'md FootnoteId>,
}

impl<'md> SearchContext<'md> {
    fn new(ctx: &'md MdContext) -> Self {
        Self {
            md_context: ctx,
            seen_footnotes: HashSet::with_capacity(4), // guess
        }
    }
}

#[cfg(test)]
mod test {
    /// Only a smoke test, because the code is pretty straightforward, and I don't feel like writing more. :-)
    mod find_children_smoke {
        use crate::mdq_inline;
        use crate::select::api::{SearchContext, SelectorAdapter};
        use crate::tree::{Inline, Link, LinkDefinition, LinkReference, MdContext, Text, TextVariant};
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
            let md_context = MdContext::empty();
            let mut ctx = SearchContext {
                md_context: &md_context,
                seen_footnotes: Default::default(),
            };
            let children = SelectorAdapter::find_children(&mut ctx, node_ref);
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
            let md_context = MdContext::empty();
            let mut ctx = SearchContext {
                md_context: &md_context,
                seen_footnotes: Default::default(),
            };
            let children = SelectorAdapter::find_children(&mut ctx, node_ref);
            assert_eq!(children, vec![MdElemRef::Link(&mk_link())]);
        }
    }
}
