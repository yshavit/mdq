use crate::md_elem::elem::*;
use crate::md_elem::*;
use crate::select::sel_chain::ChainSelector;
use crate::select::sel_code_block::CodeBlockSelector;
use crate::select::sel_link_like::ImageSelector;
use crate::select::sel_link_like::LinkSelector;
use crate::select::sel_list_item::ListItemSelector;
use crate::select::sel_section::SectionSelector;
use crate::select::sel_single_matcher::BlockQuoteSelector;
use crate::select::sel_single_matcher::HtmlSelector;
use crate::select::sel_single_matcher::ParagraphSelector;
use crate::select::sel_table::TableSelector;
use crate::select::Selector;
use paste::paste;
use std::collections::HashSet;

pub(crate) trait TrySelector<I: Into<MdElem>> {
    fn try_select(&self, ctx: &MdContext, item: I) -> Result<Vec<MdElem>, MdElem>;
}

macro_rules! adapters {
    { $($name:ident => $md_elem:ident),+ , <inlines> { $($inline:ident),+ , } } => {

        #[derive(Debug)]
        pub(crate) enum SelectorAdapter {
            $(
            $name( paste!{[<$name Selector>]} ),
            )+
            $(
            $inline( paste!{[<$inline Selector>]} ),
            )+
        }

        impl From<Selector> for SelectorAdapter {
            fn from(parsed: Selector) -> Self {
                match parsed {
                    $(
                    Selector::$name(matcher) => Self::$name(matcher.into()),
                    )+
                    $(
                    Selector::$inline(matcher) => Self::$inline(matcher.into()),
                    )+
                }
            }
        }

        impl SelectorAdapter {
            fn try_select_node(&self, ctx: &MdContext, node: MdElem) -> Result<Vec<MdElem>, MdElem> {
                match (&self, node) {
                    $(
                    (Self::$name(adapter), MdElem::$md_elem(elem)) => adapter.try_select(ctx, elem),
                    )+
                    $(
                    (Self::$inline(adapter), MdElem::Inline(Inline::$inline(elem))) => adapter.try_select(ctx, elem),
                    )+
                    (_, unmatched) => Err(unmatched),
                }
            }
        }
    };
}

adapters! {
    Chain => Doc,
    Section => Section,
    ListItem => List,
    BlockQuote => BlockQuote,
    CodeBlock => CodeBlock,
    Html => BlockHtml,
    Paragraph => Paragraph,
    Table => Table,
    <inlines> {
        Link,
        Image,
    }
}

impl SelectorAdapter {
    pub(crate) fn find_nodes(&self, ctx: &MdContext, nodes: Vec<MdElem>) -> Vec<MdElem> {
        let mut result = Vec::with_capacity(8); // arbitrary guess
        let mut search_context = SearchContext::new(ctx);
        for node in nodes {
            self.build_output(&mut result, &mut search_context, node);
        }
        result
    }

    fn build_output(&self, out: &mut Vec<MdElem>, ctx: &mut SearchContext, node: MdElem) {
        match self.try_select_node(ctx.md_context, node) {
            Ok(mut found) => out.append(&mut found),
            Err(not_found) => {
                for child in Self::find_children(ctx, not_found) {
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
    fn find_children(ctx: &mut SearchContext, node: MdElem) -> Vec<MdElem> {
        match node {
            MdElem::Doc(body) => {
                let mut wrapped = Vec::with_capacity(body.len());
                for elem in body {
                    wrapped.push(elem);
                }
                wrapped
            }
            MdElem::Section(s) => vec![MdElem::Doc(s.body)],
            MdElem::Paragraph(p) => p.body.into_iter().map(MdElem::Inline).collect(),
            MdElem::BlockQuote(b) => vec![MdElem::Doc(b.body)],
            MdElem::List(list) => {
                let mut result = Vec::with_capacity(list.items.len());
                for mut item in list.items {
                    result.append(&mut item.item);
                }
                result
            }
            MdElem::Table(table) => {
                let rows = table.rows();
                let first_row_cols = rows.first().map(Vec::len).unwrap_or(0);
                let count_estimate = rows.len() * first_row_cols;
                let mut result = Vec::with_capacity(count_estimate);
                for row in table.rows {
                    for col in row {
                        for cell in col {
                            result.push(MdElem::Inline(cell));
                        }
                    }
                }
                result
            }
            MdElem::ThematicBreak | MdElem::CodeBlock(_) => Vec::new(),
            MdElem::Inline(inline) => match inline {
                Inline::Span(Span { children, .. }) => children.into_iter().map(MdElem::Inline).collect(),
                Inline::Footnote(footnote) => {
                    // guard against cycles
                    if ctx.seen_footnotes.insert(footnote.clone()) {
                        vec![MdElem::Doc(Vec::clone(ctx.md_context.get_footnote(&footnote)))]
                    } else {
                        Vec::new()
                    }
                }
                Inline::Text(Text {
                    variant: TextVariant::InlineHtml,
                    value,
                }) => {
                    vec![MdElem::BlockHtml(value.into())]
                }
                Inline::Link(Link { display: text, .. }) => text.into_iter().map(MdElem::Inline).collect(),
                Inline::Text(_) | Inline::Image(_) => Vec::new(),
            },
            MdElem::BlockHtml(_) => Vec::new(),
        }
    }
}

struct SearchContext<'md> {
    md_context: &'md MdContext,
    seen_footnotes: HashSet<FootnoteId>,
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
    use super::*;

    /// Only a smoke test, because the code is pretty straightforward, and I don't feel like writing more. :-)
    mod find_children_smoke {
        use super::*;
        use crate::select::api::{SearchContext, SelectorAdapter};

        #[test]
        fn link_direct() {
            let link = Link {
                display: vec![mdq_inline!("link text")],
                link: LinkDefinition {
                    url: "https://example.com".to_string(),
                    title: None,
                    reference: LinkReference::Inline,
                },
            };
            let node_ref = MdElem::Inline(Inline::Link(link));
            let md_context = MdContext::empty();
            let mut ctx = SearchContext {
                md_context: &md_context,
                seen_footnotes: Default::default(),
            };
            let children = SelectorAdapter::find_children(&mut ctx, node_ref);
            assert_eq!(
                children,
                vec![MdElem::Inline(Inline::Text(Text {
                    variant: TextVariant::Plain,
                    value: "link text".to_string(),
                }))]
            );
        }
    }
}
