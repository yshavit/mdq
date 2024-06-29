use crate::tree::{Block, Container, Inline, LeafBlock, MdElem, Text, TextVariant};

pub trait MutTreeVisitor {
    /// Visits a single node. Implementations must implement this method, and the implementation should _not_ traverse
    /// down the tree.
    fn visit(&mut self, elem: &mut MdElem);
}

/// Traverses the tree, calling [MutTreeVisitor::visit] along the way.
pub fn traverse<V: MutTreeVisitor>(start: &mut MdElem, visitor: &mut V) {
    match start {
        MdElem::Block(block) => match block {
            Block::Container(block) => match block {
                Container::BlockQuote(block) => traverse_all(&mut block.body, visitor),
                Container::List(list) => {
                    for item in &mut list.items {
                        traverse_all(&mut item.item, visitor)
                    }
                }
                Container::Section(section) => {
                    traverse_inlines(&mut section.title, visitor);
                    traverse_all(&mut section.body, visitor)
                }
            },
            Block::LeafBlock(block) => match block {
                LeafBlock::CodeBlock(_) => {}
                LeafBlock::Paragraph(paragraph) => traverse_inlines(&mut paragraph.body, visitor),
                LeafBlock::Table(table) => {
                    for row in table.rows.iter_mut() {
                        for cell in row.iter_mut() {
                            traverse_inlines(cell, visitor);
                        }
                    }
                }
                LeafBlock::ThematicBreak => {}
            },
        },
        MdElem::Inline(inline) => match inline {
            Inline::Footnote(footnote) => traverse_all(&mut footnote.text, visitor),
            Inline::Formatting(formatting) => traverse_inlines(&mut formatting.children, visitor),
            Inline::Image(_) => {}
            Inline::Link(link) => traverse_inlines(&mut link.text, visitor),
            Inline::Text(_) => {}
        },
    }
}

pub fn traverse_all<V: MutTreeVisitor>(elems: &mut Vec<MdElem>, visitor: &mut V) {
    elems.iter_mut().for_each(|elem| traverse(elem, visitor));
}

fn traverse_inlines<V: MutTreeVisitor>(elems: &mut Vec<Inline>, visitor: &mut V) {
    for elem in elems.iter_mut() {
        let placeholder = Inline::Text(Text {
            variant: TextVariant::Plain,
            value: String::new(),
        });
        let inline = std::mem::replace(elem, placeholder);
        let mut md_elem = MdElem::Inline(inline);
        visitor.visit(&mut md_elem);
        let MdElem::Inline(after_visit) = md_elem else {
            panic!("expected MdElem::Inline: {:?}", md_elem);
        };
        let _ = std::mem::replace(elem, after_visit);
    }
}
