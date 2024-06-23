use std::borrow::Borrow;
use std::cmp::max;
use std::collections::{HashMap, HashSet};
use std::fmt::{Alignment, Display};

use crate::fmt_str::inlines_to_plain_string;
use crate::output::{Block, Output, SimpleWrite};
use crate::str_utils::{pad_to, standard_align, CountingWriter};
use crate::tree::*;
use crate::tree_ref::{ListItemRef, MdElemRef};

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
    I: Iterator<Item = MdElemRef<'a>>,
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
    seen_links: HashSet<LinkLabel<'a>>,
    #[allow(dead_code)]
    seen_footnotes: HashSet<&'a String>,
    pending_references: PendingReferences<'a>,
}

struct PendingReferences<'a> {
    links: HashMap<LinkLabel<'a>, ReifiedLink<'a>>,
    #[allow(dead_code)]
    footnotes: HashMap<&'a String, &'a Vec<MdElem>>,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
struct ReifiedLink<'a> {
    url: &'a String,
    title: &'a Option<String>,
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
enum LinkLabel<'a> {
    Identifier(&'a String),
    Inline(&'a Vec<Inline>),
}

impl<'a> LinkLabel<'a> {
    fn write_to<'b, W: SimpleWrite>(&self, writer: &mut MdWriterState<'b>, out: &mut Output<W>)
    where
        'a: 'b,
    {
        match self {
            LinkLabel::Identifier(text) => out.write_str(text),
            LinkLabel::Inline(text) => writer.write_line(out, text),
        }
    }
}

impl<'a> Display for LinkLabel<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LinkLabel::Identifier(s) => f.write_str(*s),
            LinkLabel::Inline(inlines) => f.write_str(&inlines_to_plain_string(*inlines)),
        }
    }
}

impl<'a> MdWriterState<'a> {
    fn write_md<I, W>(&mut self, out: &mut Output<W>, nodes: I)
    where
        I: Iterator<Item = MdElemRef<'a>>,
        W: SimpleWrite,
    {
        for node in nodes {
            self.write_one_md(out, node);
        }
    }

    pub fn write_one_md<W>(&mut self, out: &mut Output<W>, node_ref: MdElemRef<'a>)
    where
        W: SimpleWrite,
    {
        match node_ref {
            MdElemRef::Section(Section { depth, title, body }) => {
                out.with_block(Block::Plain, |out| {
                    for _ in 0..*depth {
                        out.write_str("#");
                    }
                    if !title.is_empty() {
                        out.write_str(" ");
                        self.write_line(out, title);
                    }
                });
                self.write_md(out, MdElemRef::wrap_vec(body).into_iter());
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
            MdElemRef::ListItem(ListItemRef(idx, item)) => {
                self.write_list_item(out, &idx, item);
            }
            MdElemRef::ThematicBreak => {
                out.with_block(Block::Plain, |out| out.write_str("***"));
            }
            MdElemRef::CodeBlock(block) => {
                self.write_code_block(out, block);
            }
            MdElemRef::Paragraph(para) => self.write_paragraph(out, para),
            MdElemRef::BlockQuote(block) => self.write_block_quote(out, block),
            MdElemRef::List(list) => self.write_list(out, list),
            MdElemRef::Table(table) => self.write_table(out, table),
            MdElemRef::Inline(inline) => {
                self.write_inline_element(out, inline);
            }
            MdElemRef::Link(link) => {
                self.write_link_inline_portion(out, LinkLabel::Inline(&link.text), &link.link_definition);
            }
            MdElemRef::Image(image) => {
                out.write_char('!');
                self.write_link_inline_portion(out, LinkLabel::Identifier(&image.alt), &image.link);
            }
        }
    }

    fn write_paragraph<W: SimpleWrite>(&mut self, out: &mut Output<W>, paragraph: &'a Paragraph) {
        out.with_block(Block::Plain, |out| {
            self.write_line(out, &paragraph.body);
        });
    }

    fn write_block_quote<W: SimpleWrite>(&mut self, out: &mut Output<W>, block: &'a BlockQuote) {
        out.with_block(Block::Quote, |out| {
            self.write_md(out, MdElemRef::wrap_vec(&block.body).into_iter());
        });
    }

    fn write_list<W: SimpleWrite>(&mut self, out: &mut Output<W>, list: &'a List) {
        out.with_block(Block::Plain, |out| {
            let mut index = list.starting_index;
            // let mut prefix = String::with_capacity(8); // enough for "12. [ ] "
            for item in &list.items {
                self.write_list_item(out, &index, item);
                if let Some(idx) = index.as_mut() {
                    *idx += 1;
                }
            }
        });
    }

    fn write_table<W: SimpleWrite>(&mut self, out: &mut Output<W>, table: &'a Table) {
        let Table { alignments, rows } = table;

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

    fn write_code_block<W: SimpleWrite>(&mut self, out: &mut Output<W>, block: &CodeBlock) {
        let CodeBlock { variant, value } = block;
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
            self.write_md(out, MdElemRef::wrap_vec(&item.item).into_iter());
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
            Inline::Formatting(Formatting { variant, children }) => {
                let surround = match variant {
                    FormattingVariant::Delete => "~~",
                    FormattingVariant::Emphasis => "_",
                    FormattingVariant::Strong => "**",
                };
                out.write_str(surround);
                self.write_line(out, children);
                out.write_str(surround);
            }
            Inline::Text(Text { variant, value }) => {
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
            Inline::Link(link) => self.write_one_md(out, MdElemRef::Link(link)),
            Inline::Image(image) => self.write_one_md(out, MdElemRef::Image(image)),
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
    fn write_link_inline_portion<W>(&mut self, out: &mut Output<W>, label: LinkLabel<'a>, link: &'a LinkDefinition)
    where
        W: SimpleWrite,
    {
        out.write_char('[');
        label.write_to(self, out);
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
                Some(LinkLabel::Identifier(identifier))
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
                let mut defs_to_write: Vec<_> = self.pending_references.links.drain().collect();
                defs_to_write.sort_by_key(|(k, _)| k.to_string());

                for (link_ref, link_def) in defs_to_write {
                    out.write_char('[');
                    match link_ref {
                        LinkLabel::Identifier(identifier) => out.write_str(identifier),
                        LinkLabel::Inline(text) => self.write_line(out, text),
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
                        self.write_md(out, MdElemRef::wrap_vec(text).into_iter());
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
    use crate::m_node;
    use crate::md_elem;
    use crate::md_elems;
    use crate::mdq_inline;
    use crate::output::Output;
    use crate::tree::*;
    use crate::tree_ref::MdElemRef;

    use super::write_md;

    crate::variants_checker!(VARIANTS_CHECKER = MdElemRef {
        Section(_),
        ListItem(..),
        Link(..),
        Image(..),

        Inline(Inline::Formatting(Formatting{variant: FormattingVariant::Delete, ..})),
        Inline(Inline::Formatting(Formatting{variant: FormattingVariant::Emphasis, ..})),
        Inline(Inline::Formatting(Formatting{variant: FormattingVariant::Strong, ..})),

        Inline(Inline::Text(Text{variant: TextVariant::Plain, ..})),
        Inline(Inline::Text(Text{variant: TextVariant::Code, ..})),
        Inline(Inline::Text(Text{variant: TextVariant::Math, ..})),
        Inline(Inline::Text(Text{variant: TextVariant::Html, ..})),

        Inline(Inline::Link(Link{link_definition: LinkDefinition{title: None, reference: LinkReference::Inline, ..}, ..})),
        Inline(Inline::Link(Link{link_definition: LinkDefinition{title: None, reference: LinkReference::Full(_), ..}, ..})),
        Inline(Inline::Link(Link{link_definition: LinkDefinition{title: None, reference: LinkReference::Collapsed, ..}, ..})),
        Inline(Inline::Link(Link{link_definition: LinkDefinition{title: None, reference: LinkReference::Shortcut, ..}, ..})),
        Inline(Inline::Link(Link{link_definition: LinkDefinition{title: Some(_), reference: LinkReference::Inline, ..}, ..})),
        Inline(Inline::Link(Link{link_definition: LinkDefinition{title: Some(_), reference: LinkReference::Full(_), ..}, ..})),
        Inline(Inline::Link(Link{link_definition: LinkDefinition{title: Some(_), reference: LinkReference::Collapsed, ..}, ..})),
        Inline(Inline::Link(Link{link_definition: LinkDefinition{title: Some(_), reference: LinkReference::Shortcut, ..}, ..})),

        Inline(Inline::Image(Image{link: LinkDefinition{title: None, reference: LinkReference::Inline, ..}, ..})),
        Inline(Inline::Image(Image{link: LinkDefinition{title: None, reference: LinkReference::Full(_), ..}, ..})),
        Inline(Inline::Image(Image{link: LinkDefinition{title: None, reference: LinkReference::Collapsed, ..}, ..})),
        Inline(Inline::Image(Image{link: LinkDefinition{title: None, reference: LinkReference::Shortcut, ..}, ..})),
        Inline(Inline::Image(Image{link: LinkDefinition{title: Some(_), reference: LinkReference::Inline, ..}, ..})),
        Inline(Inline::Image(Image{link: LinkDefinition{title: Some(_), reference: LinkReference::Full(_), ..}, ..})),
        Inline(Inline::Image(Image{link: LinkDefinition{title: Some(_), reference: LinkReference::Collapsed, ..}, ..})),
        Inline(Inline::Image(Image{link: LinkDefinition{title: Some(_), reference: LinkReference::Shortcut, ..}, ..})),

        Inline(Inline::Footnote{..}),

        ThematicBreak,
        CodeBlock(CodeBlock{variant: CodeVariant::Code(None), ..}),
        CodeBlock(CodeBlock{variant: CodeVariant::Code(Some(CodeOpts{metadata: None, ..})), ..}),
        CodeBlock(CodeBlock{variant: CodeVariant::Code(Some(CodeOpts{metadata: Some(_), ..})), ..}),
        CodeBlock(CodeBlock{variant: CodeVariant::Math{metadata: None}, ..}),
        CodeBlock(CodeBlock{variant: CodeVariant::Math{metadata: Some(_)}, ..}),
        CodeBlock(CodeBlock{variant: CodeVariant::Toml, ..}),
        CodeBlock(CodeBlock{variant: CodeVariant::Yaml, ..}),
        Paragraph(_),
        BlockQuote(_),
        List(_),
        Table(_),
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
                md_elems!["Hello, world"],
                indoc! {r#"
                Hello, world"#},
            );
        }

        #[test]
        fn two_paragraphs() {
            check_render(
                md_elems!["First", "Second",],
                indoc! {r#"
                First

                Second"#},
            )
        }
    }

    mod header {
        use crate::mdq_inline;

        use super::*;

        #[test]
        fn totally_empty() {
            check_render(
                md_elems![Block::Container::Section {
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
                md_elems![Block::Container::Section {
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
                md_elems![Block::Container::Section {
                    depth: 3,
                    title: vec![],
                    body: md_elems!["Hello, world."],
                }],
                indoc! {r#"
                    ###

                    Hello, world."#},
            )
        }

        #[test]
        fn title_and_body() {
            check_render(
                md_elems![Block::Container::Section {
                    depth: 1,
                    title: vec![mdq_inline!("My title")],
                    body: md_elems![Block::Container::BlockQuote {
                        body: md_elems!["Hello, world."],
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
                md_elems!["Hello, world"],
                indoc! {r#"
                Hello, world"#},
            );
        }
    }

    mod block_quote {

        use super::*;

        #[test]
        fn single_level() {
            check_render(
                md_elems![Block::Container::BlockQuote {
                    body: md_elems!["Hello, world"]
                }],
                indoc! {
                    r#"> Hello, world"#
                },
            );
        }

        #[test]
        fn two_levels() {
            check_render(
                md_elems![Block::Container::BlockQuote {
                    body: md_elems![
                        "Outer",
                        Block::Container::BlockQuote {
                            body: md_elems!["Inner"],
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
                md_elems![Block::Container::List {
                    starting_index: Some(3),
                    items: vec![
                        ListItem {
                            checked: None,
                            item: md_elems!("normal")
                        },
                        ListItem {
                            checked: Some(true),
                            item: md_elems!("checked")
                        },
                        ListItem {
                            checked: Some(false),
                            item: md_elems!("unchecked")
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
                md_elems![Block::Container::List {
                    starting_index: None,
                    items: vec![
                        ListItem {
                            checked: None,
                            item: md_elems!("normal")
                        },
                        ListItem {
                            checked: Some(true),
                            item: md_elems!("checked")
                        },
                        ListItem {
                            checked: Some(false),
                            item: md_elems!("unchecked")
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
                md_elems![Block::Container::List {
                    starting_index: None,
                    items: vec![
                        ListItem {
                            checked: None,
                            item: md_elems!["first paragraph", "second paragraph"],
                        },
                        ListItem {
                            checked: None,
                            item: md_elems!(Block::Container::BlockQuote {
                                body: md_elems!["quoted block one", "quoted block two"]
                            })
                        },
                        ListItem {
                            checked: None,
                            item: md_elems!(Block::LeafBlock::CodeBlock {
                                variant: CodeVariant::Code(None),
                                value: "line 1\nline 2".to_string(),
                            })
                        },
                        ListItem {
                            checked: Some(false),
                            item: md_elems![
                                "closing argument",
                                Block::Container::BlockQuote {
                                    body: md_elems!["supporting evidence"]
                                },
                                Block::LeafBlock::CodeBlock {
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
        use crate::tree_ref::{ListItemRef, MdElemRef};

        use super::*;

        #[test]
        fn unordered_no_checkbox() {
            create_li_singleton(None, None, md_elems!("plain text"), "- plain text");
        }

        #[test]
        fn unordered_unchecked() {
            create_li_singleton(None, Some(false), md_elems!("plain text"), "- [ ] plain text");
        }

        #[test]
        fn unordered_checked() {
            create_li_singleton(None, Some(true), md_elems!("plain text"), "- [x] plain text");
        }

        #[test]
        fn ordered_no_checkbox() {
            create_li_singleton(Some(3), None, md_elems!("plain text"), "3. plain text");
        }

        #[test]
        fn ordered_unchecked() {
            create_li_singleton(Some(3), Some(false), md_elems!("plain text"), "3. [ ] plain text");
        }

        #[test]
        fn ordered_checked() {
            create_li_singleton(Some(3), Some(true), md_elems!("plain text"), "3. [x] plain text");
        }

        fn create_li_singleton<'a>(idx: Option<u32>, checked: Option<bool>, item: Vec<MdElem>, expected: &str) {
            let li = ListItem { checked, item };
            check_render_refs(vec![MdElemRef::ListItem(ListItemRef(idx, &li))], expected)
        }
    }

    mod table {
        use markdown::mdast;

        use super::*;

        #[test]
        fn simple() {
            check_render(
                md_elems![Block::LeafBlock::Table {
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
                md_elems![Block::LeafBlock::Table {
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
                md_elems![Block::LeafBlock::Table {
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
                md_elems![Block::LeafBlock::Table {
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

        #[test]
        fn by_itself() {
            check_render(
                vec![m_node!(MdElem::Block::LeafBlock::ThematicBreak)],
                indoc! {r#"
                ***"#},
            );
        }

        #[test]
        fn with_paragraphs() {
            check_render(
                vec![
                    md_elem!("before"),
                    m_node!(MdElem::Block::LeafBlock::ThematicBreak),
                    md_elem!("after"),
                ],
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
                md_elems![Block::LeafBlock::CodeBlock {
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
                md_elems![Block::LeafBlock::CodeBlock {
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
                md_elems![Block::LeafBlock::CodeBlock {
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
                md_elems![Block::LeafBlock::CodeBlock {
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
                md_elems![Block::LeafBlock::CodeBlock {
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
                md_elems![Block::LeafBlock::CodeBlock {
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
                md_elems![Block::LeafBlock::CodeBlock {
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
                    vec![MdElem::Inline(mdq_inline!(span Delete [mdq_inline!("hello world")]))],
                    indoc! {"~~hello world~~"},
                );
            }

            #[test]
            fn emphasis() {
                check_render(
                    vec![MdElem::Inline(mdq_inline!(span Emphasis [mdq_inline!("hello world")]))],
                    indoc! {"_hello world_"},
                );
            }

            #[test]
            fn strong() {
                check_render(
                    vec![MdElem::Inline(mdq_inline!(span Strong [mdq_inline!("hello world")]))],
                    indoc! {"**hello world**"},
                );
            }

            #[test]
            fn mixed() {
                check_render(
                    vec![MdElem::Inline(mdq_inline!(span Emphasis [
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
                    vec![MdElem::Inline(Inline::Text(Text {
                        variant: TextVariant::Plain,
                        value: "hello world".to_string(),
                    }))],
                    indoc! {"hello world"},
                );
            }

            #[test]
            fn code() {
                check_render(
                    vec![MdElem::Inline(Inline::Text(Text {
                        variant: TextVariant::Code,
                        value: "hello world".to_string(),
                    }))],
                    indoc! {"`hello world`"},
                );
            }

            #[test]
            fn math() {
                check_render(
                    vec![MdElem::Inline(Inline::Text(Text {
                        variant: TextVariant::Math,
                        value: "hello world".to_string(),
                    }))],
                    indoc! {"$hello world$"},
                );
            }

            #[test]
            fn html() {
                check_render(
                    vec![MdElem::Inline(Inline::Text(Text {
                        variant: TextVariant::Html,
                        value: "<a hello />".to_string(),
                    }))],
                    indoc! {"<a hello />"},
                );
            }
        }

        mod link {
            use crate::tree::{Inline, LinkDefinition, MdElem};

            use super::*;

            #[test]
            fn inline_no_title() {
                check_link(
                    LinkDefinition {
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
                    LinkDefinition {
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
                    LinkDefinition {
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
                    LinkDefinition {
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
                    LinkDefinition {
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
                    LinkDefinition {
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
                    LinkDefinition {
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
                    LinkDefinition {
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

            fn check_link(link: LinkDefinition, expect: &str) {
                let nodes = vec![
                    MdElem::Inline(Inline::Link(Link {
                        text: vec![
                            mdq_inline!("hello "),
                            mdq_inline!(span Emphasis [mdq_inline!("world")]),
                            mdq_inline!("!"),
                        ],
                        link_definition: link,
                    })),
                    m_node!(MdElem::Block::LeafBlock::ThematicBreak),
                ];
                check_render(nodes, expect);
            }
        }

        mod image {
            use crate::tree::{Inline, LinkDefinition, MdElem};

            use super::*;

            #[test]
            fn inline_no_title() {
                check_image(
                    LinkDefinition {
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
                    LinkDefinition {
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
                    LinkDefinition {
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
                    LinkDefinition {
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
                    LinkDefinition {
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
                    LinkDefinition {
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
                    LinkDefinition {
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
                    LinkDefinition {
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

            fn check_image(link: LinkDefinition, expect: &str) {
                let nodes = vec![
                    MdElem::Inline(Inline::Image(Image {
                        alt: "hello _world_!".to_string(),
                        link,
                    })),
                    m_node!(MdElem::Block::LeafBlock::ThematicBreak),
                ];
                check_render(nodes, expect);
            }
        }
    }

    mod link {
        use super::*;

        #[test]
        fn single_link() {
            check_render_refs(
                vec![MdElemRef::Link(&Link {
                    text: vec![mdq_inline!("link text")],
                    link_definition: LinkDefinition {
                        url: "https://example.com".to_string(),
                        title: None,
                        reference: LinkReference::Full("1".to_string()),
                    },
                })],
                indoc! {r#"
                    [link text][1]

                    [1]: https://example.com"#},
            );
        }

        #[test]
        fn two_links() {
            check_render_refs(
                vec![
                    MdElemRef::Link(&Link {
                        text: vec![mdq_inline!("link text one")],
                        link_definition: LinkDefinition {
                            url: "https://example.com/1".to_string(),
                            title: None,
                            reference: LinkReference::Full("1".to_string()),
                        },
                    }),
                    MdElemRef::Link(&Link {
                        text: vec![mdq_inline!("link text two")],
                        link_definition: LinkDefinition {
                            url: "https://example.com/2".to_string(),
                            title: None,
                            reference: LinkReference::Full("2".to_string()),
                        },
                    }),
                ],
                indoc! {r#"
                    [link text one][1][link text two][2]

                    [1]: https://example.com/1
                    [2]: https://example.com/2"#},
            );
        }

        #[test]
        fn two_links_inline() {
            check_render_refs(
                vec![
                    MdElemRef::Link(&Link {
                        text: vec![mdq_inline!("link text one")],
                        link_definition: LinkDefinition {
                            url: "https://example.com/1".to_string(),
                            title: None,
                            reference: LinkReference::Inline,
                        },
                    }),
                    MdElemRef::Link(&Link {
                        text: vec![mdq_inline!("link text two")],
                        link_definition: LinkDefinition {
                            url: "https://example.com/2".to_string(),
                            title: None,
                            reference: LinkReference::Inline,
                        },
                    }),
                ],
                indoc! {r#"
                [link text one](https://example.com/1)[link text two](https://example.com/2)"#},
            );
        }
    }

    mod image {
        use super::*;

        #[test]
        fn single_image() {
            check_render_refs(
                vec![MdElemRef::Image(&Image {
                    alt: "alt text".to_string(),
                    link: LinkDefinition {
                        url: "https://example.com".to_string(),
                        title: None,
                        reference: LinkReference::Full("1".to_string()),
                    },
                })],
                indoc! {r#"
                    ![alt text][1]

                    [1]: https://example.com"#},
            );
        }

        #[test]
        fn two_images() {
            check_render_refs(
                vec![
                    MdElemRef::Image(&Image {
                        alt: "alt text one".to_string(),
                        link: LinkDefinition {
                            url: "https://example.com/1.png".to_string(),
                            title: None,
                            reference: LinkReference::Full("1".to_string()),
                        },
                    }),
                    MdElemRef::Image(&Image {
                        alt: "alt text two".to_string(),
                        link: LinkDefinition {
                            url: "https://example.com/2.png".to_string(),
                            title: None,
                            reference: LinkReference::Full("2".to_string()),
                        },
                    }),
                ],
                indoc! {r#"
                    ![alt text one][1]![alt text two][2]

                    [1]: https://example.com/1.png
                    [2]: https://example.com/2.png"#},
            );
        }
    }

    mod footnote {
        use super::*;

        #[test]
        fn single_line() {
            check_render(
                vec![
                    MdElem::Inline(Inline::Footnote(Footnote {
                        label: "a".to_string(),
                        text: md_elems!["Hello, world."],
                    })),
                    m_node!(MdElem::Block::LeafBlock::ThematicBreak),
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
                    MdElem::Inline(Inline::Footnote(Footnote {
                        label: "a".to_string(),
                        text: md_elems!["Hello,\nworld."],
                    })),
                    m_node!(MdElem::Block::LeafBlock::ThematicBreak),
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
                md_elems![Block::LeafBlock::Paragraph {
                    body: vec![
                        mdq_inline!("Hello, "),
                        m_node!(Inline::Link {
                            text: vec![mdq_inline!("world"),],
                            link_definition: LinkDefinition {
                                url: "https://example.com".to_string(),
                                title: None,
                                reference: LinkReference::Full("1".to_string()),
                            }
                        }),
                        mdq_inline!("! This is interesting"),
                        Inline::Footnote(Footnote {
                            label: "a".to_string(),
                            text: md_elems!["this is my note"],
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

        #[test]
        fn ordering() {
            check_render_with(
                &MdOptions {
                    link_reference_options: ReferencePlacement::BottomOfDoc,
                    footnote_reference_options: ReferencePlacement::BottomOfDoc,
                },
                // Define them in the opposite order that we'd expect them
                md_elems![Block::LeafBlock::Paragraph {
                    body: vec![
                        Inline::Footnote(Footnote {
                            label: "d".to_string(),
                            text: md_elems!["footnote 1"]
                        }),
                        Inline::Footnote(Footnote {
                            label: "c".to_string(),
                            text: md_elems!["footnote 2"]
                        }),
                        m_node!(Inline::Link {
                            text: vec![mdq_inline!("b-text")],
                            link_definition: LinkDefinition {
                                url: "https://example.com/b".to_string(),
                                title: None,
                                reference: LinkReference::Full("b".to_string()),
                            },
                        }),
                        m_node!(Inline::Link {
                            text: vec![mdq_inline!("a-text")],
                            link_definition: LinkDefinition {
                                url: "https://example.com/a".to_string(),
                                title: None,
                                reference: LinkReference::Full("a".to_string()),
                            },
                        }),
                    ]
                }],
                indoc! {r#"
                    [^d][^c][b-text][b][a-text][a]

                    [a]: https://example.com/a
                    [b]: https://example.com/b
                    [^c]: footnote 2
                    [^d]: footnote 1"#},
            );
        }

        fn link_and_footnote_markdown() -> Vec<MdElem> {
            md_elems![
                Block::Container::Section {
                    depth: 1,
                    title: vec![mdq_inline!("First section")],
                    body: md_elems![Block::LeafBlock::Paragraph {
                        body: vec![
                            m_node!(Inline::Link {
                                text: vec![mdq_inline!("link description")],
                                link_definition: LinkDefinition {
                                    url: "https://exampl.com".to_string(),
                                    title: None,
                                    reference: LinkReference::Full("1".to_string()),
                                },
                            }),
                            mdq_inline!(" and then a thought"),
                            Inline::Footnote(Footnote {
                                label: "a".to_string(),
                                text: md_elems!["the footnote"],
                            }),
                            mdq_inline!("."),
                        ],
                    }],
                },
                Block::Container::Section {
                    depth: 1,
                    title: vec![mdq_inline!("Second section")],
                    body: md_elems!["Second section contents."],
                },
            ]
        }
    }

    fn check_render(nodes: Vec<MdElem>, expect: &str) {
        check_render_with(&MdOptions::default(), nodes, expect);
    }

    fn check_render_with(options: &MdOptions, nodes: Vec<MdElem>, expect: &str) {
        check_render_refs_with(options, MdElemRef::wrap_vec(&nodes), expect)
    }

    fn check_render_refs(nodes: Vec<MdElemRef>, expect: &str) {
        check_render_refs_with(&MdOptions::default(), nodes, expect)
    }

    fn check_render_refs_with<'a>(options: &MdOptions, nodes: Vec<MdElemRef<'a>>, expect: &str) {
        nodes.iter().for_each(|n| VARIANTS_CHECKER.see(n));

        let mut out = Output::new(String::default());
        write_md(options, &mut out, nodes.into_iter());
        let actual = out.take_underlying().unwrap();
        assert_eq!(&actual, expect);
    }
}
