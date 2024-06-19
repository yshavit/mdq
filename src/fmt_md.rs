use std::borrow::Borrow;
use std::cmp::max;
use std::collections::{HashMap, HashSet};
use std::fmt::Alignment;

use crate::output::{Block, Output, SimpleWrite};
use crate::str_utils::{pad_to, standard_align, CountingWriter};
use crate::tree::*;
use crate::tree_ref::MdqNodeRef;

#[derive(Default)]
pub struct MdOptions {
    link_reference_options: ReferencePlacement,
    footnote_reference_options: ReferencePlacement,
}

#[allow(dead_code)]
pub enum ReferencePlacement {
    /// Show links as references defined in the first section that uses the link.
    ///
    /// ```
    /// # Top Header
    ///
    /// ## Second section
    ///
    /// Lorem [ipsum][1] in the first sub-section.
    ///              ^^^
    ///
    /// [1]: https://example.com
    /// ^^^^^^^^^^^^^^^^^^^^^^^^
    ///
    /// ## Third section
    ///
    /// Lorem ipsum in the second sub-section.
    /// ```
    BottomOfSection,

    /// Show links as references defined at the bottom of the document.
    ///
    /// ```
    /// # Top Header
    ///
    /// ## Second section
    ///
    /// Lorem [ipsum][1] in the first sub-section.
    ///              ^^^
    ///
    /// ## Third section
    ///
    /// Lorem ipsum in the second sub-section.
    ///
    /// [1]: https://example.com
    /// ^^^^^^^^^^^^^^^^^^^^^^^^
    /// ```
    BottomOfDoc,
}

impl Default for ReferencePlacement {
    fn default() -> Self {
        Self::BottomOfSection
    }
}

pub fn write_md<'a, I, W>(options: &'a MdOptions, out: &mut Output<W>, nodes: I)
where
    I: Iterator<Item = MdqNodeRef<'a>>,
    W: SimpleWrite,
{
    let pending_refs_capacity = 8; // arbitrary guess

    let mut writer_state = MdWriterState {
        opts: options,
        seen_links: HashSet::with_capacity(pending_refs_capacity),
        seen_footnotes: HashSet::with_capacity(pending_refs_capacity),
        pending_references: PendingReferences {
            links: HashMap::with_capacity(pending_refs_capacity),
            footnotes: HashMap::with_capacity(pending_refs_capacity),
        },
    };
    writer_state.write_md(out, nodes);

    // Always write the pending definitions at the end of the doc. If there were no sections, then BottomOfSection
    // won't have been triggered, but we still want to write them
    // TODO test this specific case
    writer_state.write_definitions(out, DefinitionsToWrite::Both);
}

struct MdWriterState<'a> {
    #[allow(dead_code)]
    opts: &'a MdOptions,
    seen_links: HashSet<ReifiedLabel<'a>>,
    #[allow(dead_code)]
    seen_footnotes: HashSet<&'a String>,
    pending_references: PendingReferences<'a>,
}

struct PendingReferences<'a> {
    links: HashMap<ReifiedLabel<'a>, ReifiedLink<'a>>,
    #[allow(dead_code)]
    footnotes: HashMap<&'a String, &'a Vec<MdqNode>>,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
struct ReifiedLink<'a> {
    url: &'a String,
    title: &'a Option<String>,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
enum ReifiedLabel<'a> {
    Identifier(&'a String),
    Inline(&'a Vec<Inline>),
}

impl<'a> MdWriterState<'a> {
    fn write_md<I, W>(&mut self, out: &mut Output<W>, nodes: I)
    where
        I: Iterator<Item = MdqNodeRef<'a>>,
        W: SimpleWrite,
    {
        for node in nodes {
            self.write_one_md(out, node);
        }
    }

    pub fn write_one_md<W>(&mut self, out: &mut Output<W>, node: MdqNodeRef<'a>)
    where
        W: SimpleWrite,
    {
        match node {
            MdqNodeRef::Section(Section { depth, title, body }) => {
                out.with_block(Block::Plain, |out| {
                    for _ in 0..*depth {
                        out.write_str("#");
                    }
                    if !title.is_empty() {
                        out.write_str(" ");
                        self.write_line(out, title);
                    }
                });
                self.write_md(out, MdqNodeRef::wrap_vec(body).into_iter());
                let which_defs_to_write =
                    match (&self.opts.link_reference_options, &self.opts.footnote_reference_options) {
                        (ReferencePlacement::BottomOfSection, ReferencePlacement::BottomOfSection) => {
                            DefinitionsToWrite::Both
                        }
                        (_, ReferencePlacement::BottomOfSection) => DefinitionsToWrite::Footnotes,
                        (ReferencePlacement::BottomOfSection, _) => DefinitionsToWrite::Links,
                        (_, _) => DefinitionsToWrite::Neither,
                    };
                self.write_definitions(out, which_defs_to_write);
            }
            MdqNodeRef::Paragraph(Paragraph { body }) => {
                out.with_block(Block::Plain, |out| {
                    self.write_line(out, body);
                });
            }
            MdqNodeRef::BlockQuote(BlockQuote { body }) => {
                out.with_block(Block::Quote, |out| {
                    self.write_md(out, MdqNodeRef::wrap_vec(body).into_iter());
                });
            }
            MdqNodeRef::List(List { starting_index, items }) => {
                out.with_block(Block::Plain, |out| {
                    let mut index = starting_index.clone();
                    // let mut prefix = String::with_capacity(8); // enough for "12. [ ] "
                    for item in items {
                        self.write_list_item(out, &index, item);
                        if let Some(idx) = index.as_mut() {
                            *idx += 1;
                        }
                    }
                });
            }
            MdqNodeRef::ListItem(idx, item) => {
                self.write_list_item(out, &idx, item);
            }
            MdqNodeRef::Table(Table { alignments, rows }) => {
                let mut row_strs = Vec::with_capacity(rows.len());

                let mut column_widths = [0].repeat(alignments.len());
                if !alignments.is_empty() {
                    for (idx, alignment) in alignments.iter().enumerate() {
                        let width = match standard_align(alignment) {
                            Some(Alignment::Left | Alignment::Right) => 2,
                            Some(Alignment::Center) => 3,
                            None => 1,
                        };
                        column_widths[idx] = width;
                    }
                }

                // Pre-calculate all the cells, and also how wide each column needs to be
                for row in rows {
                    let mut col_strs = Vec::with_capacity(row.len());
                    for (idx, col) in row.iter().enumerate() {
                        let col_str = self.line_to_string(col);
                        // Extend the row_sizes if needed. This happens if we had fewer alignments than columns in any
                        // row. I'm not sure if that's possible, but it's easy to guard against.
                        while column_widths.len() <= idx {
                            column_widths.push(0);
                        }
                        // +1 for the padding on either side
                        let cell_width = if col_str.is_empty() { 1 } else { col_str.len() + 2 };
                        column_widths[idx] = max(column_widths[idx], cell_width);
                        col_strs.push(col_str);
                    }
                    row_strs.push(col_strs);
                }

                // Create column formatters for each column
                let write_row = |out: &mut Output<W>, row: Vec<String>, add_newline: bool| {
                    if row.is_empty() {
                        out.write_str("||\n");
                        return;
                    }
                    out.write_char('|');
                    for (idx, col) in row.iter().enumerate() {
                        let left_padding_count = if col.is_empty() {
                            0
                        } else {
                            out.write_char(' ');
                            1
                        };
                        pad_to(
                            out,
                            &col,
                            *column_widths.get(idx).unwrap_or(&0) - left_padding_count - 1, // -1 for right padding
                            alignments.get(idx),
                        );
                        out.write_str(" |");
                    }
                    if add_newline {
                        out.write_char('\n');
                    }
                };

                let mut rows_iter = row_strs.into_iter();

                // First row
                let Some(first_row) = rows_iter.next() else {
                    return; // unexpected!
                };
                write_row(out, first_row, true);

                // Headers
                if !alignments.is_empty() {
                    out.write_char('|');
                    for (idx, align) in alignments.iter().enumerate() {
                        let width = column_widths
                            .get(idx)
                            .unwrap_or_else(|| match standard_align(align) {
                                Some(Alignment::Left | Alignment::Right) => &2,
                                Some(Alignment::Center) => &3,
                                None => &1,
                            })
                            .to_owned();
                        match standard_align(align) {
                            Some(Alignment::Left) => {
                                out.write_char(':');
                                out.write_str(&"-".repeat(width - 1));
                            }
                            Some(Alignment::Right) => {
                                out.write_str(&"-".repeat(width - 1));
                                out.write_char(':');
                            }
                            Some(Alignment::Center) => {
                                out.write_char(':');
                                out.write_str(&"-".repeat(width - 2));
                                out.write_char(':');
                            }
                            None => {
                                out.write_str(&"-".repeat(width));
                            }
                        };
                        out.write_char('|');
                    }
                    out.write_char('\n');
                }

                // And finally, the rows
                let mut rows_iter = rows_iter.peekable();
                while let Some(row) = rows_iter.next() {
                    write_row(out, row, rows_iter.peek().is_some());
                }
            }
            MdqNodeRef::ThematicBreak => {
                out.with_block(Block::Plain, |out| out.write_str("***"));
            }
            MdqNodeRef::CodeBlock(CodeBlock { variant, value }) => {
                let (surround, meta) = match variant {
                    CodeVariant::Code(opts) => {
                        let meta = if let Some(opts) = opts {
                            let mut extras = opts.language.to_string();
                            if let Some(meta) = &opts.metadata {
                                extras.push(' ');
                                extras.push_str(meta);
                            }
                            Some(extras)
                        } else {
                            None
                        };
                        ("```", meta)
                    }
                    CodeVariant::Math { metadata } => {
                        let meta = if let Some(meta) = metadata {
                            let mut meta_with_space = String::with_capacity(meta.len() + 1);
                            meta_with_space.push(' ');
                            meta_with_space.push_str(meta);
                            Some(meta_with_space)
                        } else {
                            None
                        };
                        ("$$", meta)
                    }
                    CodeVariant::Toml => ("+++", None),
                    CodeVariant::Yaml => ("---", None),
                };

                out.with_pre_block(|out| {
                    out.write_str(surround);
                    if let Some(meta) = meta {
                        out.write_str(&meta);
                    }
                    out.write_char('\n');
                    out.write_str(value);
                    out.write_char('\n');
                    out.write_str(surround);
                });
            }
            MdqNodeRef::Inline(inline) => {
                self.write_inline_element(out, inline);
            }
        }
    }

    fn write_list_item<W: SimpleWrite>(&mut self, out: &mut Output<W>, index: &Option<u32>, item: &'a ListItem) {
        let mut counting_writer = CountingWriter::wrap(out);
        match index {
            None => std::fmt::Write::write_str(&mut counting_writer, "- ").unwrap(),
            Some(i) => {
                std::fmt::Write::write_fmt(&mut counting_writer, format_args!("{}. ", &i)).unwrap();
            }
        };
        if let Some(checked) = &item.checked {
            std::fmt::Write::write_char(&mut counting_writer, '[').unwrap();
            std::fmt::Write::write_char(&mut counting_writer, if *checked { 'x' } else { ' ' }).unwrap();
            std::fmt::Write::write_str(&mut counting_writer, "] ").unwrap();
        }
        let count = counting_writer.count();
        out.with_block(Block::Inlined(count), |out| {
            self.write_md(out, MdqNodeRef::wrap_vec(&item.item).into_iter());
        });
    }

    pub fn write_line<E, W>(&mut self, out: &mut Output<W>, elems: &'a [E])
    where
        E: Borrow<Inline>,
        W: SimpleWrite,
    {
        for elem in elems {
            self.write_inline_element(out, elem.borrow());
        }
    }

    pub fn write_inline_element<W>(&mut self, out: &mut Output<W>, elem: &'a Inline)
    where
        W: SimpleWrite,
    {
        match elem {
            Inline::Span { variant, children } => {
                let surround = match variant {
                    SpanVariant::Delete => "~~",
                    SpanVariant::Emphasis => "_",
                    SpanVariant::Strong => "**",
                };
                out.write_str(surround);
                self.write_line(out, children);
                out.write_str(surround);
            }
            Inline::Text { variant, value } => {
                let surround = match variant {
                    TextVariant::Plain => "",
                    TextVariant::Code => "`",
                    TextVariant::Math => "$",
                    TextVariant::Html => "",
                };
                out.write_str(surround);
                out.write_str(value);
                out.write_str(surround);
            }
            Inline::Link { text, link } => {
                self.write_link_inline(out, ReifiedLabel::Inline(text), link, |me, out| {
                    me.write_line(out, text)
                });
            }
            Inline::Image { alt, link } => {
                out.write_char('!');
                self.write_link_inline(out, ReifiedLabel::Identifier(alt), link, |_, out| out.write_str(alt));
            }
            Inline::Footnote(Footnote { label, text }) => {
                out.write_str("[^");
                out.write_str(label);
                out.write_char(']');
                if self.seen_footnotes.insert(label) {
                    self.pending_references.footnotes.insert(label, text);
                }
            }
        }
    }

    /// Writes the inline portion of the link, which may be the full link if it was originally inlined.
    ///
    /// Examples:
    ///
    /// ```
    /// [an inline link](https://example.com)
    /// [a referenced link][1]
    /// ```
    ///
    /// The `contents` function is what writes e.g. `an inline link` above. It's a function because it may be a recursive
    /// call into [write_line] (for links) or just simple text (for image alts).
    fn write_link_inline<W, F>(&mut self, out: &mut Output<W>, label: ReifiedLabel<'a>, link: &'a Link, contents: F)
    where
        W: SimpleWrite,
        F: FnOnce(&mut Self, &mut Output<W>),
    {
        out.write_char('[');
        contents(self, out);
        out.write_char(']');
        let reference_to_add = match &link.reference {
            LinkReference::Inline => {
                out.write_char('(');
                out.write_str(&link.url);
                self.write_url_title(out, &link.title);
                out.write_char(')');
                None
            }
            LinkReference::Full(identifier) => {
                out.write_char('[');
                out.write_str(identifier);
                out.write_char(']');
                Some(ReifiedLabel::Identifier(identifier))
            }
            LinkReference::Collapsed => {
                out.write_str("[]");
                Some(label)
            }
            LinkReference::Shortcut => {
                // no write
                Some(label)
            }
        };

        if let Some(reference_label) = reference_to_add {
            if self.seen_links.insert(reference_label.clone()) {
                self.pending_references.links.insert(
                    reference_label,
                    ReifiedLink {
                        url: &link.url,
                        title: &link.title,
                    },
                );
                // else warn?
            }
        }
    }

    fn write_definitions<W>(&mut self, out: &mut Output<W>, which: DefinitionsToWrite)
    where
        W: SimpleWrite,
    {
        let is_empty = match which {
            DefinitionsToWrite::Links => self.pending_references.links.is_empty(),
            DefinitionsToWrite::Footnotes => self.pending_references.footnotes.is_empty(),
            DefinitionsToWrite::Both => {
                self.pending_references.links.is_empty() && self.pending_references.footnotes.is_empty()
            }
            DefinitionsToWrite::Neither => true,
        };
        if is_empty {
            return;
        }
        out.with_block(Block::Plain, move |out| {
            let mut remaining_defs = 0;
            if matches!(which, DefinitionsToWrite::Links | DefinitionsToWrite::Both) {
                remaining_defs += self.pending_references.links.len()
            }
            if matches!(which, DefinitionsToWrite::Footnotes | DefinitionsToWrite::Both) {
                remaining_defs += self.pending_references.footnotes.len()
            }
            let mut newline = |out: &mut Output<W>| {
                if remaining_defs > 1 {
                    out.write_char('\n');
                }
                remaining_defs -= 1;
            };

            if matches!(which, DefinitionsToWrite::Links | DefinitionsToWrite::Both) {
                // TODO sort them
                let defs_to_write: Vec<_> = self.pending_references.links.drain().collect();
                for (link_ref, link_def) in defs_to_write {
                    out.write_char('[');
                    match link_ref {
                        ReifiedLabel::Identifier(identifier) => out.write_str(identifier),
                        ReifiedLabel::Inline(text) => self.write_line(out, text),
                    }
                    out.write_str("]: ");
                    out.write_str(&link_def.url);
                    self.write_url_title(out, &link_def.title);
                    newline(out);
                }
            }
            if matches!(which, DefinitionsToWrite::Footnotes | DefinitionsToWrite::Both) {
                let mut defs_to_write: Vec<_> = self.pending_references.footnotes.drain().collect();
                defs_to_write.sort_by_key(|&kv| kv.0);

                for (link_ref, text) in defs_to_write {
                    out.write_str("[^");
                    out.write_str(link_ref);
                    out.write_str("]: ");
                    out.with_block(Block::Inlined(2), |out| {
                        self.write_md(out, MdqNodeRef::wrap_vec(text).into_iter());
                        newline(out);
                    });
                }
            }
        });
    }

    fn write_url_title<W>(&mut self, out: &mut Output<W>, title: &Option<String>)
    where
        W: SimpleWrite,
    {
        let Some(title) = title else { return };
        out.write_char(' ');
        // TODO pick which quoting char to use (single, double, or paren) depending on title
        out.write_char('"');
        for ch in title.chars() {
            if ch == '"' {
                out.write_char('\\');
            }
            out.write_char(ch);
        }
        out.write_char('"');
    }

    fn line_to_string<E>(&mut self, line: &'a [E]) -> String
    where
        E: Borrow<Inline>,
    {
        let mut out = Output::new(String::with_capacity(line.len() * 10)); // rough guess
        self.write_line(&mut out, line);
        out.take_underlying().unwrap()
    }
}

enum DefinitionsToWrite {
    // simple enum-set-like definition
    Links,
    Footnotes,
    Both,
    Neither,
}

#[cfg(test)]
pub mod tests {
    use indoc::indoc;

    use crate::fmt_md::MdOptions;
    use crate::mdq_inline;
    use crate::mdq_nodes;
    use crate::output::Output;
    use crate::tree::*;
    use crate::tree_ref::MdqNodeRef;

    use super::write_md;

    crate::variants_checker!(VARIANTS_CHECKER = MdqNodeRef {
        Section(_),
        Paragraph(_),
        BlockQuote(_),
        List(_),
        ListItem(..),
        Table(_),
        ThematicBreak,
        CodeBlock(crate::tree::CodeBlock{variant: CodeVariant::Code(None), ..}),
        CodeBlock(crate::tree::CodeBlock{variant: CodeVariant::Code(Some(CodeOpts{metadata: None, ..})), ..}),
        CodeBlock(crate::tree::CodeBlock{variant: CodeVariant::Code(Some(CodeOpts{metadata: Some(_), ..})), ..}),
        CodeBlock(crate::tree::CodeBlock{variant: CodeVariant::Math{metadata: None}, ..}),
        CodeBlock(crate::tree::CodeBlock{variant: CodeVariant::Math{metadata: Some(_)}, ..}),
        CodeBlock(crate::tree::CodeBlock{variant: CodeVariant::Toml, ..}),
        CodeBlock(crate::tree::CodeBlock{variant: CodeVariant::Yaml, ..}),

        Inline(crate::tree::Inline::Span{variant: SpanVariant::Delete, ..}),
        Inline(crate::tree::Inline::Span{variant: SpanVariant::Emphasis, ..}),
        Inline(crate::tree::Inline::Span{variant: SpanVariant::Strong, ..}),

        Inline(crate::tree::Inline::Text{variant: TextVariant::Plain, ..}),
        Inline(crate::tree::Inline::Text{variant: TextVariant::Code, ..}),
        Inline(crate::tree::Inline::Text{variant: TextVariant::Math, ..}),
        Inline(crate::tree::Inline::Text{variant: TextVariant::Html, ..}),

        Inline(crate::tree::Inline::Link{link: Link{title: None, reference: LinkReference::Inline, ..}, ..}),
        Inline(crate::tree::Inline::Link{link: Link{title: None, reference: LinkReference::Full(_), ..}, ..}),
        Inline(crate::tree::Inline::Link{link: Link{title: None, reference: LinkReference::Collapsed, ..}, ..}),
        Inline(crate::tree::Inline::Link{link: Link{title: None, reference: LinkReference::Shortcut, ..}, ..}),
        Inline(crate::tree::Inline::Link{link: Link{title: Some(_), reference: LinkReference::Inline, ..}, ..}),
        Inline(crate::tree::Inline::Link{link: Link{title: Some(_), reference: LinkReference::Full(_), ..}, ..}),
        Inline(crate::tree::Inline::Link{link: Link{title: Some(_), reference: LinkReference::Collapsed, ..}, ..}),
        Inline(crate::tree::Inline::Link{link: Link{title: Some(_), reference: LinkReference::Shortcut, ..}, ..}),

        Inline(crate::tree::Inline::Image{link: Link{title: None, reference: LinkReference::Inline, ..}, ..}),
        Inline(crate::tree::Inline::Image{link: Link{title: None, reference: LinkReference::Full(_), ..}, ..}),
        Inline(crate::tree::Inline::Image{link: Link{title: None, reference: LinkReference::Collapsed, ..}, ..}),
        Inline(crate::tree::Inline::Image{link: Link{title: None, reference: LinkReference::Shortcut, ..}, ..}),
        Inline(crate::tree::Inline::Image{link: Link{title: Some(_), reference: LinkReference::Inline, ..}, ..}),
        Inline(crate::tree::Inline::Image{link: Link{title: Some(_), reference: LinkReference::Full(_), ..}, ..}),
        Inline(crate::tree::Inline::Image{link: Link{title: Some(_), reference: LinkReference::Collapsed, ..}, ..}),
        Inline(crate::tree::Inline::Image{link: Link{title: Some(_), reference: LinkReference::Shortcut, ..}, ..}),

        Inline(crate::tree::Inline::Footnote{..}),
    });

    #[test]
    fn empty() {
        check_render(vec![], indoc! {r#""#});
    }

    mod root {
        use super::*;

        #[test]
        fn one_paragraph() {
            check_render(
                mdq_nodes![Paragraph {
                    body: vec![Inline::Text {
                        variant: TextVariant::Plain,
                        value: "Hello, world".to_string()
                    }]
                }],
                indoc! {r#"
                Hello, world"#},
            );
        }

        #[test]
        fn two_paragraphs() {
            check_render(
                mdq_nodes![
                    Paragraph {
                        body: vec![Inline::Text {
                            variant: TextVariant::Plain,
                            value: "First".to_string()
                        }]
                    },
                    Paragraph {
                        body: vec![Inline::Text {
                            variant: TextVariant::Plain,
                            value: "Second".to_string()
                        }]
                    },
                ],
                indoc! {r#"
                First

                Second"#},
            )
        }
    }

    mod header {
        use super::*;
        use crate::mdq_inline;

        #[test]
        fn totally_empty() {
            check_render(
                mdq_nodes![Section {
                    depth: 3,
                    title: vec![],
                    body: vec![],
                }],
                indoc! {r#"
                ###"#},
            )
        }

        #[test]
        fn only_title() {
            check_render(
                mdq_nodes![Section {
                    depth: 3,
                    title: vec![mdq_inline!("My header")],
                    body: vec![],
                }],
                indoc! {r#"
                ### My header"#},
            )
        }

        #[test]
        fn only_body() {
            check_render(
                mdq_nodes![Section {
                    depth: 3,
                    title: vec![],
                    body: mdq_nodes![Paragraph {
                        body: vec![mdq_inline!("Hello, world.")]
                    },],
                }],
                indoc! {r#"
                    ###

                    Hello, world."#},
            )
        }

        #[test]
        fn title_and_body() {
            check_render(
                mdq_nodes![Section {
                    depth: 1,
                    title: vec![mdq_inline!("My title")],
                    body: mdq_nodes![BlockQuote {
                        body: mdq_nodes![Paragraph {
                            body: vec![mdq_inline!("Hello, world.")]
                        },],
                    },],
                }],
                indoc! {r#"
                    # My title

                    > Hello, world."#},
            )
        }
    }

    mod paragraph {
        use super::*;

        #[test]
        fn simple() {
            check_render(
                mdq_nodes![Paragraph {
                    body: vec![Inline::Text {
                        variant: TextVariant::Plain,
                        value: "Hello, world".to_string()
                    }]
                }],
                indoc! {r#"
                Hello, world"#},
            );
        }
    }

    mod block_quote {
        use super::*;
        use crate::mdq_inline;

        #[test]
        fn single_level() {
            check_render(
                mdq_nodes![BlockQuote {
                    body: mdq_nodes![Paragraph {
                        body: vec![Inline::Text {
                            variant: TextVariant::Plain,
                            value: "Hello, world".to_string()
                        }]
                    }]
                }],
                indoc! {
                    r#"> Hello, world"#
                },
            );
        }

        #[test]
        fn two_levels() {
            check_render(
                mdq_nodes![BlockQuote {
                    body: mdq_nodes![
                        Paragraph {
                            body: vec![mdq_inline!("Outer")],
                        },
                        BlockQuote {
                            body: mdq_nodes![Paragraph {
                                body: vec![mdq_inline!("Inner")],
                            },]
                        },
                    ]
                }],
                indoc! {r#"
                    > Outer
                    >
                    > > Inner"#
                },
            );
        }
    }

    mod list {
        use super::*;

        #[test]
        fn ordered() {
            check_render(
                mdq_nodes![List {
                    starting_index: Some(3),
                    items: vec![
                        ListItem {
                            checked: None,
                            item: mdq_nodes!("normal")
                        },
                        ListItem {
                            checked: Some(true),
                            item: mdq_nodes!("checked")
                        },
                        ListItem {
                            checked: Some(false),
                            item: mdq_nodes!("unchecked")
                        },
                    ],
                }],
                indoc! {r#"
                3. normal
                4. [x] checked
                5. [ ] unchecked"#},
            );
        }

        #[test]
        fn unordered() {
            check_render(
                mdq_nodes![List {
                    starting_index: None,
                    items: vec![
                        ListItem {
                            checked: None,
                            item: mdq_nodes!("normal")
                        },
                        ListItem {
                            checked: Some(true),
                            item: mdq_nodes!("checked")
                        },
                        ListItem {
                            checked: Some(false),
                            item: mdq_nodes!("unchecked")
                        },
                    ],
                }],
                indoc! {r#"
                - normal
                - [x] checked
                - [ ] unchecked"#},
            );
        }

        #[test]
        fn block_alignments() {
            check_render(
                mdq_nodes![List {
                    starting_index: None,
                    items: vec![
                        ListItem {
                            checked: None,
                            item: mdq_nodes!["first paragraph", "second paragraph"],
                        },
                        ListItem {
                            checked: None,
                            item: mdq_nodes!(BlockQuote {
                                body: mdq_nodes!["quoted block one", "quoted block two"]
                            })
                        },
                        ListItem {
                            checked: None,
                            item: mdq_nodes!(CodeBlock {
                                variant: CodeVariant::Code(None),
                                value: "line 1\nline 2".to_string(),
                            })
                        },
                        ListItem {
                            checked: Some(false),
                            item: mdq_nodes![
                                Paragraph {
                                    body: vec![mdq_inline!("closing argument")]
                                },
                                BlockQuote {
                                    body: mdq_nodes!["supporting evidence"]
                                },
                                CodeBlock {
                                    variant: CodeVariant::Code(None),
                                    value: "line a\nline b".to_string(),
                                },
                            ]
                        },
                    ],
                }],
                indoc! {r#"
                - first paragraph

                  second paragraph
                - > quoted block one
                  >
                  > quoted block two
                - ```
                  line 1
                  line 2
                  ```
                - [ ] closing argument

                      > supporting evidence

                      ```
                      line a
                      line b
                      ```"#},
            )
        }
    }

    mod list_item {
        use super::*;
        use crate::tree_ref::MdqNodeRef;

        #[test]
        fn unordered_no_checkbox() {
            create_li_singleton(None, None, mdq_nodes!("plain text"), "- plain text");
        }

        #[test]
        fn unordered_unchecked() {
            create_li_singleton(None, Some(false), mdq_nodes!("plain text"), "- [ ] plain text");
        }

        #[test]
        fn unordered_checked() {
            create_li_singleton(None, Some(true), mdq_nodes!("plain text"), "- [x] plain text");
        }

        #[test]
        fn ordered_no_checkbox() {
            create_li_singleton(Some(3), None, mdq_nodes!("plain text"), "3. plain text");
        }

        #[test]
        fn ordered_unchecked() {
            create_li_singleton(Some(3), Some(false), mdq_nodes!("plain text"), "3. [ ] plain text");
        }

        #[test]
        fn ordered_checked() {
            create_li_singleton(Some(3), Some(true), mdq_nodes!("plain text"), "3. [x] plain text");
        }

        fn create_li_singleton<'a>(idx: Option<u32>, checked: Option<bool>, item: Vec<MdqNode>, expected: &str) {
            let li = ListItem { checked, item };
            check_render_with_refs(&MdOptions::default(), vec![MdqNodeRef::ListItem(idx, &li)], expected)
        }
    }

    mod table {
        use super::*;
        use markdown::mdast;

        #[test]
        fn simple() {
            check_render(
                mdq_nodes![Table {
                    alignments: vec![
                        mdast::AlignKind::Left,
                        mdast::AlignKind::Right,
                        mdast::AlignKind::Center,
                        mdast::AlignKind::None,
                    ],
                    rows: vec![
                        // Header row
                        vec![
                            // columns
                            vec![mdq_inline!("Left")],
                            vec![mdq_inline!("Right")],
                            vec![mdq_inline!("Center")],
                            vec![mdq_inline!("Default")],
                        ],
                        // Data row
                        vec![
                            // columns
                            vec![mdq_inline!("a")],
                            vec![mdq_inline!("b")],
                            vec![mdq_inline!("c")],
                            vec![mdq_inline!("d")],
                        ],
                    ],
                }],
                indoc! {r#"
                | Left | Right | Center | Default |
                |:-----|------:|:------:|---------|
                | a    |     b |   c    | d       |"#},
            );
        }

        #[test]
        fn single_char_cells() {
            // This checks the minimum padding aspects
            check_render(
                mdq_nodes![Table {
                    alignments: vec![
                        mdast::AlignKind::Left,
                        mdast::AlignKind::Right,
                        mdast::AlignKind::Center,
                        mdast::AlignKind::None,
                    ],
                    rows: vec![
                        // Header row
                        vec![
                            // columns
                            vec![mdq_inline!("a")],
                            vec![mdq_inline!("b")],
                            vec![mdq_inline!("c")],
                            vec![mdq_inline!("d")],
                        ],
                        // Data row
                        vec![
                            // columns
                            vec![mdq_inline!("1")],
                            vec![mdq_inline!("2")],
                            vec![mdq_inline!("3")],
                            vec![mdq_inline!("4")],
                        ],
                    ],
                }],
                indoc! {r#"
                | a | b | c | d |
                |:--|--:|:-:|---|
                | 1 | 2 | 3 | 4 |"#},
            );
        }

        #[test]
        fn empty_cells() {
            // This checks the minimum padding aspects
            check_render(
                mdq_nodes![Table {
                    alignments: vec![
                        mdast::AlignKind::Left,
                        mdast::AlignKind::Right,
                        mdast::AlignKind::Center,
                        mdast::AlignKind::None,
                    ],
                    rows: vec![
                        // Header row
                        vec![
                            // columns
                            vec![mdq_inline!("")],
                            vec![mdq_inline!("")],
                            vec![mdq_inline!("")],
                            vec![mdq_inline!("")],
                        ],
                        // Data row
                        vec![
                            // columns
                            vec![mdq_inline!("")],
                            vec![mdq_inline!("")],
                            vec![mdq_inline!("")],
                            vec![mdq_inline!("")],
                        ],
                    ],
                }],
                indoc! {r#"
                |  |  |   | |
                |:-|-:|:-:|-|
                |  |  |   | |"#},
            );
        }

        #[test]
        fn row_counts_inconsistent() {
            // This is an invalid table, but we should still support it
            check_render(
                mdq_nodes![Table {
                    alignments: vec![mdast::AlignKind::None, mdast::AlignKind::None,],
                    rows: vec![
                        // Header row: two values
                        vec![
                            // columns
                            vec![mdq_inline!("A")],
                            vec![mdq_inline!("B")],
                        ],
                        // First row: only one value
                        vec![
                            // columns
                            vec![mdq_inline!("1")],
                        ],
                        // Second row: three values
                        vec![
                            // columns
                            vec![mdq_inline!("i")],
                            vec![mdq_inline!("ii")],
                            vec![mdq_inline!("iii")],
                        ],
                    ],
                }],
                indoc! {r#"
                | A | B  |
                |---|----|
                | 1 |
                | i | ii | iii |"#},
            );
        }
    }

    mod thematic_break {
        use super::*;
        use crate::mdq_node;

        #[test]
        fn by_itself() {
            check_render(
                vec![MdqNode::ThematicBreak],
                indoc! {r#"
                ***"#},
            );
        }

        #[test]
        fn with_paragraphs() {
            check_render(
                vec![mdq_node!("before"), MdqNode::ThematicBreak, mdq_node!("after")],
                indoc! {r#"
                before

                ***

                after"#},
            );
        }
    }

    mod code_block {
        use super::*;

        #[test]
        fn code_no_lang() {
            check_render(
                mdq_nodes![CodeBlock {
                    variant: CodeVariant::Code(None),
                    value: "one\ntwo".to_string(),
                }],
                indoc! {r#"
                ```
                one
                two
                ```"#},
            );
        }

        #[test]
        fn code_with_lang() {
            check_render(
                mdq_nodes![CodeBlock {
                    variant: CodeVariant::Code(Some(CodeOpts {
                        language: "rust".to_string(),
                        metadata: None
                    })),
                    value: "one\ntwo".to_string(),
                }],
                indoc! {r#"
                ```rust
                one
                two
                ```"#},
            );
        }

        #[test]
        fn code_with_lang_and_title() {
            check_render(
                mdq_nodes![CodeBlock {
                    variant: CodeVariant::Code(Some(CodeOpts {
                        language: "rust".to_string(),
                        metadata: Some(r#"title="my code""#.to_string())
                    })),
                    value: "one\ntwo".to_string(),
                }],
                indoc! {r#"
                ```rust title="my code"
                one
                two
                ```"#},
            );
        }

        #[test]
        fn math_no_metadata() {
            check_render(
                mdq_nodes![CodeBlock {
                    variant: CodeVariant::Math { metadata: None },
                    value: "one\ntwo".to_string(),
                }],
                indoc! {r#"
                $$
                one
                two
                $$"#},
            );
        }

        #[test]
        fn math_with_metadata() {
            check_render(
                mdq_nodes![CodeBlock {
                    variant: CodeVariant::Math {
                        metadata: Some("metadata".to_string())
                    },
                    value: "one\ntwo".to_string(),
                }],
                indoc! {r#"
                $$ metadata
                one
                two
                $$"#},
            );
        }

        #[test]
        fn toml() {
            check_render(
                mdq_nodes![CodeBlock {
                    variant: CodeVariant::Toml,
                    value: "one\ntwo".to_string(),
                }],
                indoc! {r#"
                +++
                one
                two
                +++"#},
            );
        }

        #[test]
        fn yaml() {
            check_render(
                mdq_nodes![CodeBlock {
                    variant: CodeVariant::Yaml,
                    value: "one\ntwo".to_string(),
                }],
                indoc! {r#"
                ---
                one
                two
                ---"#},
            );
        }
    }

    mod inline {
        use super::*;

        mod span {
            use super::*;

            #[test]
            fn delete() {
                check_render(
                    vec![MdqNode::Inline(mdq_inline!(span Delete [mdq_inline!("hello world")]))],
                    indoc! {"~~hello world~~"},
                );
            }

            #[test]
            fn emphasis() {
                check_render(
                    vec![MdqNode::Inline(mdq_inline!(span Emphasis [mdq_inline!("hello world")]))],
                    indoc! {"_hello world_"},
                );
            }

            #[test]
            fn strong() {
                check_render(
                    vec![MdqNode::Inline(mdq_inline!(span Strong [mdq_inline!("hello world")]))],
                    indoc! {"**hello world**"},
                );
            }

            #[test]
            fn mixed() {
                check_render(
                    vec![MdqNode::Inline(mdq_inline!(span Emphasis [
                        mdq_inline!("one "),
                        mdq_inline!(span Strong [
                            mdq_inline!("two "),
                            mdq_inline!(span Delete [
                                mdq_inline!("three")
                            ]),
                        ]),
                    ]))],
                    indoc! {"_one **two ~~three~~**_"},
                );
            }
        }

        mod text {
            use super::*;

            #[test]
            fn text() {
                check_render(
                    vec![MdqNode::Inline(Inline::Text {
                        variant: TextVariant::Plain,
                        value: "hello world".to_string(),
                    })],
                    indoc! {"hello world"},
                );
            }

            #[test]
            fn code() {
                check_render(
                    vec![MdqNode::Inline(Inline::Text {
                        variant: TextVariant::Code,
                        value: "hello world".to_string(),
                    })],
                    indoc! {"`hello world`"},
                );
            }

            #[test]
            fn math() {
                check_render(
                    vec![MdqNode::Inline(Inline::Text {
                        variant: TextVariant::Math,
                        value: "hello world".to_string(),
                    })],
                    indoc! {"$hello world$"},
                );
            }

            #[test]
            fn html() {
                check_render(
                    vec![MdqNode::Inline(Inline::Text {
                        variant: TextVariant::Html,
                        value: "<a hello />".to_string(),
                    })],
                    indoc! {"<a hello />"},
                );
            }
        }

        mod link {
            use super::*;
            use crate::tree::{Inline, Link, MdqNode};

            #[test]
            fn inline_no_title() {
                check_link(
                    Link {
                        url: "https://example.com".to_string(),
                        title: None,
                        reference: LinkReference::Inline,
                    },
                    indoc! {r#"
                        [hello _world_!](https://example.com)

                        ***"#},
                );
            }

            #[test]
            fn full_no_title() {
                check_link(
                    Link {
                        url: "https://example.com".to_string(),
                        title: None,
                        reference: LinkReference::Full("1".to_string()),
                    },
                    indoc! {r#"
                        [hello _world_!][1]

                        ***

                        [1]: https://example.com"#},
                );
            }

            #[test]
            fn collapsed_no_title() {
                check_link(
                    Link {
                        url: "https://example.com".to_string(),
                        title: None,
                        reference: LinkReference::Collapsed,
                    },
                    indoc! {r#"
                        [hello _world_!][]

                        ***

                        [hello _world_!]: https://example.com"#},
                );
            }

            #[test]
            fn shortcut_no_title() {
                check_link(
                    Link {
                        url: "https://example.com".to_string(),
                        title: None,
                        reference: LinkReference::Shortcut,
                    },
                    indoc! {r#"
                        [hello _world_!]

                        ***

                        [hello _world_!]: https://example.com"#},
                );
            }

            #[test]
            fn inline_with_title() {
                check_link(
                    Link {
                        url: "https://example.com".to_string(),
                        title: Some("my title".to_string()),
                        reference: LinkReference::Inline,
                    },
                    indoc! {r#"
                        [hello _world_!](https://example.com "my title")

                        ***"#},
                );
            }

            #[test]
            fn full_with_title() {
                check_link(
                    Link {
                        url: "https://example.com".to_string(),
                        title: Some("my title".to_string()),
                        reference: LinkReference::Full("1".to_string()),
                    },
                    indoc! {r#"
                        [hello _world_!][1]

                        ***

                        [1]: https://example.com "my title""#},
                );
            }

            #[test]
            fn collapsed_with_title() {
                check_link(
                    Link {
                        url: "https://example.com".to_string(),
                        title: Some("my title".to_string()),
                        reference: LinkReference::Collapsed,
                    },
                    indoc! {r#"
                        [hello _world_!][]

                        ***

                        [hello _world_!]: https://example.com "my title""#},
                );
            }

            #[test]
            fn shortcut_with_title() {
                check_link(
                    Link {
                        url: "https://example.com".to_string(),
                        title: Some("my title".to_string()),
                        reference: LinkReference::Shortcut,
                    },
                    indoc! {r#"
                        [hello _world_!]

                        ***

                        [hello _world_!]: https://example.com "my title""#},
                );
            }

            fn check_link(link: Link, expect: &str) {
                let nodes = vec![
                    MdqNode::Inline(Inline::Link {
                        text: vec![
                            mdq_inline!("hello "),
                            mdq_inline!(span Emphasis [mdq_inline!("world")]),
                            mdq_inline!("!"),
                        ],
                        link,
                    }),
                    MdqNode::ThematicBreak,
                ];
                check_render(nodes, expect);
            }
        }

        mod image {
            use super::*;
            use crate::tree::{Inline, Link, MdqNode};

            #[test]
            fn inline_no_title() {
                check_image(
                    Link {
                        url: "https://example.com".to_string(),
                        title: None,
                        reference: LinkReference::Inline,
                    },
                    indoc! {r#"
                        ![hello _world_!](https://example.com)

                        ***"#},
                );
            }

            #[test]
            fn full_no_title() {
                check_image(
                    Link {
                        url: "https://example.com".to_string(),
                        title: None,
                        reference: LinkReference::Full("1".to_string()),
                    },
                    indoc! {r#"
                        ![hello _world_!][1]

                        ***

                        [1]: https://example.com"#},
                );
            }

            #[test]
            fn collapsed_no_title() {
                check_image(
                    Link {
                        url: "https://example.com".to_string(),
                        title: None,
                        reference: LinkReference::Collapsed,
                    },
                    indoc! {r#"
                        ![hello _world_!][]

                        ***

                        [hello _world_!]: https://example.com"#},
                );
            }

            #[test]
            fn shortcut_no_title() {
                check_image(
                    Link {
                        url: "https://example.com".to_string(),
                        title: None,
                        reference: LinkReference::Shortcut,
                    },
                    indoc! {r#"
                        ![hello _world_!]

                        ***

                        [hello _world_!]: https://example.com"#},
                );
            }

            #[test]
            fn inline_with_title() {
                check_image(
                    Link {
                        url: "https://example.com".to_string(),
                        title: Some("my title".to_string()),
                        reference: LinkReference::Inline,
                    },
                    indoc! {r#"
                        ![hello _world_!](https://example.com "my title")

                        ***"#},
                );
            }

            #[test]
            fn full_with_title() {
                check_image(
                    Link {
                        url: "https://example.com".to_string(),
                        title: Some("my title".to_string()),
                        reference: LinkReference::Full("1".to_string()),
                    },
                    indoc! {r#"
                        ![hello _world_!][1]

                        ***

                        [1]: https://example.com "my title""#},
                );
            }

            #[test]
            fn collapsed_with_title() {
                check_image(
                    Link {
                        url: "https://example.com".to_string(),
                        title: Some("my title".to_string()),
                        reference: LinkReference::Collapsed,
                    },
                    indoc! {r#"
                        ![hello _world_!][]

                        ***

                        [hello _world_!]: https://example.com "my title""#},
                );
            }

            #[test]
            fn shortcut_with_title() {
                check_image(
                    Link {
                        url: "https://example.com".to_string(),
                        title: Some("my title".to_string()),
                        reference: LinkReference::Shortcut,
                    },
                    indoc! {r#"
                        ![hello _world_!]

                        ***

                        [hello _world_!]: https://example.com "my title""#},
                );
            }

            fn check_image(link: Link, expect: &str) {
                let nodes = vec![
                    MdqNode::Inline(Inline::Image {
                        alt: "hello _world_!".to_string(),
                        link,
                    }),
                    MdqNode::ThematicBreak,
                ];
                check_render(nodes, expect);
            }
        }
    }

    mod footnote {
        use super::*;

        #[test]
        fn single_line() {
            check_render(
                vec![
                    MdqNode::Inline(Inline::Footnote(Footnote {
                        label: "a".to_string(),
                        text: mdq_nodes![Paragraph {
                            body: vec![mdq_inline!("Hello, world.")]
                        }],
                    })),
                    MdqNode::ThematicBreak,
                ],
                indoc! {r#"
                    [^a]

                    ***

                    [^a]: Hello, world."#},
            )
        }

        #[test]
        fn two_lines() {
            check_render(
                vec![
                    MdqNode::Inline(Inline::Footnote(Footnote {
                        label: "a".to_string(),
                        text: mdq_nodes![Paragraph {
                            body: vec![mdq_inline!("Hello,\nworld.")]
                        }],
                    })),
                    MdqNode::ThematicBreak,
                ],
                indoc! {r#"
                    [^a]

                    ***

                    [^a]: Hello,
                      world."#},
            )
        }
    }

    mod annotation_and_footnote_layouts {
        use super::*;
        use crate::fmt_md::ReferencePlacement;

        #[test]
        fn link_and_footnote() {
            check_render(
                mdq_nodes![Paragraph {
                    body: vec![
                        mdq_inline!("Hello, "),
                        Inline::Link {
                            text: vec![mdq_inline!("world"),],
                            link: Link {
                                url: "https://example.com".to_string(),
                                title: None,
                                reference: LinkReference::Full("1".to_string()),
                            }
                        },
                        mdq_inline!("! This is interesting"),
                        Inline::Footnote(Footnote {
                            label: "a".to_string(),
                            text: mdq_nodes![Paragraph {
                                body: vec![mdq_inline!("this is my note")]
                            }],
                        }),
                        mdq_inline!("."),
                    ],
                }],
                indoc! {r#"
                    Hello, [world][1]! This is interesting[^a].

                    [1]: https://example.com
                    [^a]: this is my note"#},
            );
        }

        #[test]
        fn both_in_sections() {
            check_render_with(
                &MdOptions {
                    link_reference_options: ReferencePlacement::BottomOfSection,
                    footnote_reference_options: ReferencePlacement::BottomOfSection,
                },
                link_and_footnote_markdown(),
                indoc! {r#"
                    # First section

                    [link description][1] and then a thought[^a].

                    [1]: https://exampl.com
                    [^a]: the footnote

                    # Second section

                    Second section contents."#},
            );
        }

        #[test]
        fn only_link_in_section() {
            check_render_with(
                &MdOptions {
                    link_reference_options: ReferencePlacement::BottomOfSection,
                    footnote_reference_options: ReferencePlacement::BottomOfDoc,
                },
                link_and_footnote_markdown(),
                indoc! {r#"
                    # First section

                    [link description][1] and then a thought[^a].

                    [1]: https://exampl.com

                    # Second section

                    Second section contents.

                    [^a]: the footnote"#},
            );
        }

        #[test]
        fn only_footnote_in_section() {
            check_render_with(
                &MdOptions {
                    link_reference_options: ReferencePlacement::BottomOfDoc,
                    footnote_reference_options: ReferencePlacement::BottomOfSection,
                },
                link_and_footnote_markdown(),
                indoc! {r#"
                    # First section

                    [link description][1] and then a thought[^a].

                    [^a]: the footnote

                    # Second section

                    Second section contents.

                    [1]: https://exampl.com"#},
            );
        }

        #[test]
        fn both_bottom_of_doc() {
            check_render_with(
                &MdOptions {
                    link_reference_options: ReferencePlacement::BottomOfDoc,
                    footnote_reference_options: ReferencePlacement::BottomOfDoc,
                },
                link_and_footnote_markdown(),
                indoc! {r#"
                    # First section

                    [link description][1] and then a thought[^a].

                    # Second section

                    Second section contents.

                    [1]: https://exampl.com
                    [^a]: the footnote"#},
            );
        }

        fn link_and_footnote_markdown() -> Vec<MdqNode> {
            mdq_nodes![
                Section {
                    depth: 1,
                    title: vec![mdq_inline!("First section")],
                    body: mdq_nodes![Paragraph {
                        body: vec![
                            Inline::Link {
                                text: vec![mdq_inline!("link description")],
                                link: Link {
                                    url: "https://exampl.com".to_string(),
                                    title: None,
                                    reference: LinkReference::Full("1".to_string()),
                                },
                            },
                            mdq_inline!(" and then a thought"),
                            Inline::Footnote(Footnote {
                                label: "a".to_string(),
                                text: mdq_nodes![Paragraph {
                                    body: vec![mdq_inline!("the footnote")]
                                }],
                            }),
                            mdq_inline!("."),
                        ],
                    }],
                },
                Section {
                    depth: 1,
                    title: vec![mdq_inline!("Second section")],
                    body: mdq_nodes![Paragraph {
                        body: vec![mdq_inline!("Second section contents.")]
                    }],
                },
            ]
        }
    }

    fn check_render(nodes: Vec<MdqNode>, expect: &str) {
        check_render_with(&MdOptions::default(), nodes, expect);
    }

    fn check_render_with(options: &MdOptions, nodes: Vec<MdqNode>, expect: &str) {
        check_render_with_refs(options, MdqNodeRef::wrap_vec(&nodes), expect)
    }

    fn check_render_with_refs<'a>(options: &MdOptions, nodes: Vec<MdqNodeRef<'a>>, expect: &str) {
        nodes.iter().for_each(|n| VARIANTS_CHECKER.see(n));

        let mut out = Output::new(String::default());
        write_md(options, &mut out, nodes.into_iter());
        let actual = out.take_underlying().unwrap();
        assert_eq!(&actual, expect);
    }
}
