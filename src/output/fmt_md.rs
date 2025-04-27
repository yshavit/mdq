use crate::md_elem::elem::*;
use crate::md_elem::*;
use crate::output::fmt_md_inlines::{InlineElemOptions, MdInlinesWriter};
use crate::output::link_transform::LinkLabel;
use crate::util::output::{Block, Output, SimpleWrite};
use crate::util::str_utils::{pad_to, CountingWriter};
use clap::ValueEnum;
use derive_builder::Builder;
use std::borrow::Cow;
use std::cmp::max;
use std::ops::Deref;

#[derive(Copy, Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Builder)]
pub struct MdWriterOptions {
    /// Where to put link references (for non-inline links).
    pub link_reference_placement: ReferencePlacement,
    /// Where to put footnote references.
    pub footnote_reference_placement: ReferencePlacement,
    /// Inline Markdown options.
    pub inline_options: InlineElemOptions,
    /// Whether to include thematic breaks between top-level elements.
    ///
    /// If `true`, each top-level element will be separated by a thematic break:
    ///
    /// ```markdown
    ///    -----
    /// ```
    ///
    /// Note that "top-level" refers to the nodes you pass into [`MdWriter::write`], not to what might
    /// otherwise look "top level" to a user. For example, if you were to construct an [`MdElem::Section`] with depth 2,
    /// and within its body another `Section` with depth 1, only the outer depth-2 `Section` would count as top-level
    /// for these breaks.
    ///
    /// [`MdWriter::write`]: crate::output::MdWriter
    pub include_thematic_breaks: bool,
    /// Optional text wrapping.
    ///
    /// Code blocks will never wrap, and certain inline elements (like URLs) will be treated as atomic: if any part of
    /// them wraps, the whole thing will wrap, and if it starts a line then it will never wrap.
    ///
    /// For example:
    ///
    /// <pre>
    /// ┌────────────────────────────────┐
    /// │This is a long line that will   │
    /// │wrap. But                       │
    /// │[this](https://example.com/will/not/wrap)
    /// |because it's a URL.             │
    /// └────────────────────────────────┘
    /// </pre>
    ///
    ///
    pub text_width: Option<usize>,
}

/// Whether to put link definitions at the end of each section, or at the bottom of the whole document.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default, ValueEnum)]
pub enum ReferencePlacement {
    /// Show link definitions in the first section that uses the link.
    ///
    /// ```md
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
    #[default]
    Section,

    /// Show link definitions at the bottom of the document.
    ///
    /// ```md
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
    Doc,
}

pub(crate) fn write_md<'md, I, W>(options: MdWriterOptions, out: &mut Output<W>, ctx: &'md MdContext, nodes: I)
where
    I: Iterator<Item = &'md MdElem>,
    W: SimpleWrite,
{
    let mut writer_state = MdWriterState {
        ctx,
        opts: options,
        prev_was_thematic_break: false,
        inlines_writer: &mut MdInlinesWriter::new(ctx, options.inline_options),
    };
    let nodes_count = writer_state.write_md(out, nodes.into_iter(), true);

    // Always write the pending definitions at the end of the doc. If there were no sections, then BottomOfSection
    // won't have been triggered, but we still want to write them. We'll add a thematic break before the links if there
    // was more than one top-level node: if there's one node, then the whole thing looks like a single document and
    // should just have the links there; but if there are multiple, then each would have been separated by a thematic
    // break, and we want to add another such break so that it's clear that the reference definitions don't just go with
    // the last item.
    writer_state.write_definitions(out, DefinitionsToWrite::Both, nodes_count > 1);
}

struct MdWriterState<'s, 'md> {
    ctx: &'md MdContext,
    opts: MdWriterOptions,
    prev_was_thematic_break: bool,
    inlines_writer: &'s mut MdInlinesWriter<'md>,
}

impl<'md> MdWriterState<'_, 'md> {
    fn write_md<I, W>(&mut self, out: &mut Output<W>, nodes: I, add_break: bool) -> usize
    where
        I: Iterator<Item = &'md MdElem>,
        W: SimpleWrite,
    {
        let mut count = 0;
        let mut iter = nodes.into_iter().peekable();
        while let Some(node) = iter.next() {
            count += 1;
            self.write_one_md(out, node);
            if add_break {
                self.write_link_refs_as_needed(out);
                if iter.peek().is_some() {
                    self.print_separator(out);
                }
            }
        }
        count
    }

    fn print_separator(&mut self, out: &mut Output<impl SimpleWrite>) {
        if self.opts.include_thematic_breaks {
            self.write_one_md(out, &MdElem::ThematicBreak);
        } else {
            out.write_char('\n');
        }
    }

    pub fn write_one_md<W>(&mut self, out: &mut Output<W>, node_ref: &'md MdElem)
    where
        W: SimpleWrite,
    {
        let prev_was_thematic_break = self.prev_was_thematic_break;
        self.prev_was_thematic_break = false;

        match node_ref {
            MdElem::Doc(items) => {
                self.write_md(out, items.iter(), false);
            }
            MdElem::Section(Section { depth, title, body }) => {
                out.with_block(Block::Plain, |out| {
                    out.without_wrapping(|out| {
                        for _ in 0..*depth {
                            out.write_str("#");
                        }
                        if !title.is_empty() {
                            out.write_str(" ");
                            self.inlines_writer.write_line(out, title);
                        }
                    });
                });
                self.write_md(out, body.iter(), false);
                self.write_link_refs_as_needed(out);
            }
            MdElem::ThematicBreak => {
                if !prev_was_thematic_break {
                    out.with_block(Block::Plain, |out| {
                        out.without_wrapping(|out| out.write_str("   -----"));
                    });
                }
                self.prev_was_thematic_break = true;
            }
            MdElem::CodeBlock(block) => {
                self.write_code_block(out, block);
            }
            MdElem::Paragraph(para) => self.write_paragraph(out, para),
            MdElem::BlockQuote(block) => self.write_block_quote(out, block),
            MdElem::List(list) => self.write_list(out, list),
            MdElem::Table(table) => self.write_table(out, table),
            MdElem::Inline(inline) => {
                self.inlines_writer.write_inline_element(out, inline);
            }
            MdElem::BlockHtml(html) => out.with_block(Block::Plain, |out| {
                out.write_str(&html.value);
            }),
        }
    }

    fn write_link_refs_as_needed<W: SimpleWrite>(&mut self, out: &mut Output<W>) {
        let which_defs_to_write = match (
            &self.opts.link_reference_placement,
            &self.opts.footnote_reference_placement,
        ) {
            (ReferencePlacement::Section, ReferencePlacement::Section) => DefinitionsToWrite::Both,
            (_, ReferencePlacement::Section) => DefinitionsToWrite::Footnotes,
            (ReferencePlacement::Section, _) => DefinitionsToWrite::Links,
            (_, _) => DefinitionsToWrite::Neither,
        };
        self.write_definitions(out, which_defs_to_write, false);
    }

    fn write_paragraph<W: SimpleWrite>(&mut self, out: &mut Output<W>, paragraph: &'md Paragraph) {
        out.with_block(Block::Plain, |out| {
            self.inlines_writer.write_line(out, &paragraph.body);
        });
    }

    fn write_block_quote<W: SimpleWrite>(&mut self, out: &mut Output<W>, block: &'md BlockQuote) {
        out.with_block(Block::Quote, |out| {
            let body = &block.body;
            self.write_md(out, body.iter(), false);
        });
    }

    fn write_list<W: SimpleWrite>(&mut self, out: &mut Output<W>, list: &'md List) {
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

    fn write_table<W: SimpleWrite>(&mut self, out: &mut Output<W>, table: &'md Table) {
        let alignments = table.alignments();
        let rows = table.rows();

        let mut row_strs = Vec::with_capacity(alignments.len());

        let mut column_widths = vec![0; alignments.len()];
        if !alignments.is_empty() {
            for (idx, alignment) in alignments.iter().enumerate() {
                let width = match *alignment {
                    Some(ColumnAlignment::Left | ColumnAlignment::Right) => 2,
                    Some(ColumnAlignment::Center) => 3,
                    None => 1,
                };
                column_widths[idx] = width;
            }
        }

        // Pre-calculate all the cells, and also how wide each column needs to be
        for row in rows {
            let mut col_strs = Vec::with_capacity(row.len());
            for (idx, cell) in row.iter().enumerate() {
                let col_str = self.line_to_string(cell);
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
                    col,
                    *column_widths.get(idx).unwrap_or(&0) - left_padding_count - 1, // -1 for right padding
                    alignments.get(idx).copied().flatten(),
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
            for (idx, &align) in alignments.iter().enumerate() {
                let width = column_widths
                    .get(idx)
                    .unwrap_or(match align {
                        Some(ColumnAlignment::Left | ColumnAlignment::Right) => &2,
                        Some(ColumnAlignment::Center) => &3,
                        None => &1,
                    })
                    .to_owned();
                match align {
                    Some(ColumnAlignment::Left) => {
                        out.write_char(':');
                        out.write_str(&"-".repeat(width - 1));
                    }
                    Some(ColumnAlignment::Right) => {
                        out.write_str(&"-".repeat(width - 1));
                        out.write_char(':');
                    }
                    Some(ColumnAlignment::Center) => {
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

    fn write_code_block<W: SimpleWrite>(&mut self, out: &mut Output<W>, block: &'md CodeBlock) {
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
                let leading_backticks_count = Self::count_longest_opening_backticks(value);
                let surround_backticks = if leading_backticks_count < 3 {
                    Cow::Borrowed("```")
                } else {
                    Cow::Owned("`".repeat(leading_backticks_count + 1))
                };
                (surround_backticks, meta)
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
                (Cow::Borrowed("$$"), meta)
            }
            CodeVariant::Toml => (Cow::Borrowed("+++"), None),
            CodeVariant::Yaml => (Cow::Borrowed("---"), None),
        };

        out.with_pre_block(|out| {
            out.write_str(&surround);
            if let Some(meta) = meta {
                out.write_str(&meta);
            }
            out.write_char('\n');
            out.write_str(value);
            out.write_char('\n');
            out.write_str(&surround);
        });
    }

    fn count_longest_opening_backticks(contents: &str) -> usize {
        let mut max_len = 0;
        for line in contents.split('\n') {
            let mut len_for_line = 0;
            for ch in line.chars() {
                if ch != '`' {
                    break;
                }
                len_for_line += 1;
            }
            max_len = max(max_len, len_for_line);
        }
        max_len
    }

    fn write_list_item<W: SimpleWrite>(&mut self, out: &mut Output<W>, index: &Option<u32>, item: &'md ListItem) {
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
        out.with_block(Block::Indent(count), |out| {
            let body = &item.item;
            self.write_md(out, body.iter(), false);
        });
    }

    fn write_definitions<W>(&mut self, out: &mut Output<W>, which: DefinitionsToWrite, add_break: bool)
    where
        W: SimpleWrite,
    {
        let any_pending = match which {
            DefinitionsToWrite::Links => self.inlines_writer.has_pending_links(),
            DefinitionsToWrite::Footnotes => self.inlines_writer.has_pending_footnotes(),
            DefinitionsToWrite::Both => {
                self.inlines_writer.has_pending_links() || self.inlines_writer.has_pending_footnotes()
            }
            DefinitionsToWrite::Neither => false,
        };
        if !any_pending {
            return;
        }
        if add_break {
            self.print_separator(out);
        }
        out.with_block(Block::Plain, move |out| {
            let mut remaining_defs = 0;
            if matches!(which, DefinitionsToWrite::Links | DefinitionsToWrite::Both) {
                remaining_defs += self.inlines_writer.count_pending_links();
            }
            if matches!(which, DefinitionsToWrite::Footnotes | DefinitionsToWrite::Both) {
                remaining_defs += self.inlines_writer.count_pending_footnotes();
            }
            let mut newline = |out: &mut Output<W>| {
                if remaining_defs > 1 {
                    out.write_char('\n');
                }
                remaining_defs -= 1;
            };

            if matches!(which, DefinitionsToWrite::Links | DefinitionsToWrite::Both) {
                let mut defs_to_write: Vec<_> = self.inlines_writer.drain_pending_links();
                defs_to_write.sort_by_key(|(k, _)| k.get_sort_string(self.ctx));

                for (link_ref, link_def) in defs_to_write {
                    out.write_char('[');
                    match link_ref {
                        LinkLabel::Text(identifier) => out.write_str(identifier.deref()),
                        LinkLabel::Inline(text) => self.inlines_writer.write_line(out, text),
                    }
                    out.without_wrapping(|out| {
                        out.write_str("]: ");
                        out.write_str(link_def.url);
                        self.inlines_writer.write_url_title(out, link_def.title);
                    });
                    newline(out);
                }
            }
            if matches!(which, DefinitionsToWrite::Footnotes | DefinitionsToWrite::Both) {
                let mut defs_to_write: Vec<_> = self.inlines_writer.drain_pending_footnotes();
                defs_to_write.sort_unstable_by(|a, b| a.0.cmp(&b.0));

                for (link_ref, text) in defs_to_write {
                    out.write_str("[^");
                    out.write_str(&link_ref);
                    out.write_str("]: ");
                    out.with_block(Block::Indent(2), |out| {
                        self.write_md(out, text.iter(), false);
                    });
                }
            }
        });
    }

    fn line_to_string(&mut self, line: &'md TableCell) -> String {
        let mut out = Output::without_text_wrapping(String::with_capacity(line.len() * 10)); // rough guess
        self.inlines_writer.write_line(&mut out, line);
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

    use super::*;
    use crate::output::fmt_md::MdWriterOptions;
    use crate::output::link_transform::LinkTransform;
    use crate::util::output::Output;
    use crate::util::utils_for_test::*;

    use super::{write_md, ReferencePlacement};

    variants_checker!(VARIANTS_CHECKER = MdElem {
        Doc(..),
        Section(_),
        BlockHtml(..),

        Inline(Inline::Span(Span{variant: SpanVariant::Delete, ..})),
        Inline(Inline::Span(Span{variant: SpanVariant::Emphasis, ..})),
        Inline(Inline::Span(Span{variant: SpanVariant::Strong, ..})),

        Inline(Inline::Text(Text{variant: TextVariant::Plain, ..})),
        Inline(Inline::Text(Text{variant: TextVariant::Code, ..})),
        Inline(Inline::Text(Text{variant: TextVariant::Math, ..})),
        Inline(Inline::Text(Text{variant: TextVariant::InlineHtml, ..})),

        Inline(Inline::Link(Link{link: LinkDefinition{title: None, reference: LinkReference::Inline, ..}, ..})),
        Inline(Inline::Link(Link{link: LinkDefinition{title: None, reference: LinkReference::Full(_), ..}, ..})),
        Inline(Inline::Link(Link{link: LinkDefinition{title: None, reference: LinkReference::Collapsed, ..}, ..})),
        Inline(Inline::Link(Link{link: LinkDefinition{title: None, reference: LinkReference::Shortcut, ..}, ..})),
        Inline(Inline::Link(Link{link: LinkDefinition{title: Some(_), reference: LinkReference::Inline, ..}, ..})),
        Inline(Inline::Link(Link{link: LinkDefinition{title: Some(_), reference: LinkReference::Full(_), ..}, ..})),
        Inline(Inline::Link(Link{link: LinkDefinition{title: Some(_), reference: LinkReference::Collapsed, ..}, ..})),
        Inline(Inline::Link(Link{link: LinkDefinition{title: Some(_), reference: LinkReference::Shortcut, ..}, ..})),

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
    fn reference_placement_default() {
        assert_eq!(ReferencePlacement::default(), ReferencePlacement::Section);
    }

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

                   -----

                Second"#},
            )
        }

        #[test]
        fn two_paragraphs_in_one_doc() {
            check_render_refs(
                vec![MdElem::Doc(md_elems!["First", "Second"])],
                indoc! {r#"
                First

                Second"#},
            )
        }
    }

    mod header {
        use super::*;

        #[test]
        fn totally_empty() {
            check_render(
                md_elems![Section {
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
                md_elems![Section {
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
                md_elems![Section {
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
                md_elems![Section {
                    depth: 1,
                    title: vec![mdq_inline!("My title")],
                    body: md_elems![BlockQuote {
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

        #[test]
        fn two_paragraphs_with_thematic_break() {
            check_render(
                md_elems!["Alpha", "Bravo"],
                indoc! {r#"
                    Alpha

                       -----

                    Bravo"#},
            );
        }

        #[test]
        fn two_paragraphs_no_thematic_break() {
            let mut options = MdWriterOptions::default_for_tests();
            options.include_thematic_breaks = false;
            check_render_with(
                options,
                md_elems!["Alpha", "Bravo"],
                indoc! {r#"
                    Alpha

                    Bravo"#},
            );
        }
    }

    mod block_quote {
        use super::*;

        #[test]
        fn single_level() {
            check_render(
                md_elems![BlockQuote {
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
                md_elems![BlockQuote {
                    body: md_elems![
                        "Outer",
                        BlockQuote {
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
                md_elems![List {
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
                md_elems![List {
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
                md_elems![List {
                    starting_index: None,
                    items: vec![
                        ListItem {
                            checked: None,
                            item: md_elems!["first paragraph", "second paragraph"],
                        },
                        ListItem {
                            checked: None,
                            item: md_elems!(BlockQuote {
                                body: md_elems!["quoted block one", "quoted block two"]
                            })
                        },
                        ListItem {
                            checked: None,
                            item: md_elems!(CodeBlock {
                                variant: CodeVariant::Code(None),
                                value: "line 1\nline 2".to_string(),
                            })
                        },
                        ListItem {
                            checked: Some(false),
                            item: md_elems![
                                "closing argument",
                                BlockQuote {
                                    body: md_elems!["supporting evidence"]
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
        use crate::md_elem::md_elems;

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

        #[test]
        fn multiple_with_thematic_breaks() {
            let li1 = ListItem {
                checked: Some(false),
                item: md_elems!("first item"),
            };
            let li2 = ListItem {
                checked: Some(true),
                item: md_elems!("second item"),
            };
            check_render_refs(
                vec![
                    MdElem::List(List {
                        starting_index: None,
                        items: vec![li1],
                    }),
                    MdElem::List(List {
                        starting_index: None,
                        items: vec![li2],
                    }),
                ],
                indoc! {r#"
                - [ ] first item

                   -----

                - [x] second item"#},
            );
        }

        #[test]
        fn multiple_no_thematic_breaks() {
            let li1 = ListItem {
                checked: Some(false),
                item: md_elems!("first item"),
            };
            let li2 = ListItem {
                checked: Some(true),
                item: md_elems!("second item"),
            };
            let mut options = MdWriterOptions::default_for_tests();
            options.include_thematic_breaks = false;
            check_render_refs_with(
                options,
                vec![
                    MdElem::List(List {
                        starting_index: None,
                        items: vec![li1],
                    }),
                    MdElem::List(List {
                        starting_index: None,
                        items: vec![li2],
                    }),
                ],
                indoc! {r#"
                - [ ] first item

                - [x] second item"#},
            );
        }

        fn create_li_singleton(idx: Option<u32>, checked: Option<bool>, item: Vec<MdElem>, expected: &str) {
            let li = ListItem { checked, item };
            check_render_refs(
                vec![MdElem::List(List {
                    starting_index: idx,
                    items: vec![li],
                })],
                expected,
            )
        }
    }

    mod table {
        use super::*;

        #[test]
        fn simple() {
            check_render(
                md_elems![Table {
                    alignments: vec![
                        Some(ColumnAlignment::Left),
                        Some(ColumnAlignment::Right),
                        Some(ColumnAlignment::Center),
                        None,
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
                md_elems![Table {
                    alignments: vec![
                        Some(ColumnAlignment::Left),
                        Some(ColumnAlignment::Right),
                        Some(ColumnAlignment::Center),
                        None,
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
                md_elems![Table {
                    alignments: vec![
                        Some(ColumnAlignment::Left),
                        Some(ColumnAlignment::Right),
                        Some(ColumnAlignment::Center),
                        None,
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
                md_elems![Table {
                    alignments: vec![None, None],
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

        /// Test of a table slice, instead of the table ref directly. This is just a smoke test,
        /// because the implementations are the same (one forwards to the other); this test is
        /// here just to validate that the delegation happens, as opposed to "oops, I forgot to
        /// actually implement the delegation."
        #[test]
        fn slice() {
            let table = Table {
                alignments: vec![Some(ColumnAlignment::Left), Some(ColumnAlignment::Right)],
                rows: vec![
                    // Header row
                    vec![
                        // columns
                        vec![mdq_inline!("Left")],
                        vec![mdq_inline!("Right")],
                    ],
                    // Data row
                    vec![
                        // columns
                        vec![mdq_inline!("a")],
                        vec![mdq_inline!("b")],
                    ],
                ],
            };
            check_render_refs(
                vec![MdElem::Table(table)],
                indoc! {r#"
                    | Left | Right |
                    |:-----|------:|
                    | a    |     b |"#},
            );
        }
    }

    mod thematic_break {
        use super::*;

        #[test]
        fn by_itself() {
            check_render(vec![m_node!(MdElem::ThematicBreak)], "   -----");
        }

        #[test]
        fn with_paragraphs() {
            check_render(
                vec![md_elem!("before"), m_node!(MdElem::ThematicBreak), md_elem!("after")],
                indoc! {r#"
                before

                   -----

                after"#},
            );
        }
    }

    mod code_block {
        use super::*;

        #[test]
        fn code_no_lang() {
            check_render(
                md_elems![CodeBlock {
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
                md_elems![CodeBlock {
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
                md_elems![CodeBlock {
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
                md_elems![CodeBlock {
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
                md_elems![CodeBlock {
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
                md_elems![CodeBlock {
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
                md_elems![CodeBlock {
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

        #[test]
        fn nested_block() {
            check_render(
                md_elems![CodeBlock {
                    variant: CodeVariant::Code(None),
                    value: indoc! {r#"
                        For example:
                        ```nested
                        nested contents
                        ```"#}
                    .to_string(),
                }],
                indoc! {r#"
                ````
                For example:
                ```nested
                nested contents
                ```
                ````"#},
            );
        }

        #[test]
        fn two_blocks_with_thematic_break() {
            check_render(
                md_elems![
                    CodeBlock {
                        variant: CodeVariant::Code(None),
                        value: "one".to_string(),
                    },
                    CodeBlock {
                        variant: CodeVariant::Code(None),
                        value: "two".to_string(),
                    }
                ],
                indoc! {r#"
                ```
                one
                ```

                   -----

                ```
                two
                ```"#},
            );
        }

        #[test]
        fn two_blocks_no_thematic_break() {
            let mut options = MdWriterOptions::default_for_tests();
            options.include_thematic_breaks = false;
            check_render_with(
                options,
                md_elems![
                    CodeBlock {
                        variant: CodeVariant::Code(None),
                        value: "one".to_string(),
                    },
                    CodeBlock {
                        variant: CodeVariant::Code(None),
                        value: "two".to_string(),
                    }
                ],
                indoc! {r#"
                ```
                one
                ```

                ```
                two
                ```"#},
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

            #[test]
            fn with_thematic_break() {
                check_render(
                    vec![MdElem::Inline(mdq_inline!("one")), MdElem::Inline(mdq_inline!("two"))],
                    indoc! {r"
                    one

                       -----

                    two"},
                );
            }

            #[test]
            fn no_thematic_break() {
                let mut options = MdWriterOptions::default_for_tests();
                options.include_thematic_breaks = false;
                check_render_with(
                    options,
                    vec![MdElem::Inline(mdq_inline!("one")), MdElem::Inline(mdq_inline!("two"))],
                    indoc! {r"
                    one
                    two"},
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
                        variant: TextVariant::InlineHtml,
                        value: "<a hello />".to_string(),
                    }))],
                    indoc! {"<a hello />"},
                );
            }
        }

        mod link {
            use super::*;

            #[test]
            fn inline_no_title() {
                check_link_and_thematic_break(
                    LinkDefinition {
                        url: "https://example.com".to_string(),
                        title: None,
                        reference: LinkReference::Inline,
                    },
                    indoc! {r#"
                        [hello _world_!](https://example.com)

                           -----"#},
                );
            }

            #[test]
            fn full_no_title() {
                check_link_and_thematic_break(
                    LinkDefinition {
                        url: "https://example.com".to_string(),
                        title: None,
                        reference: LinkReference::Full("1".to_string()),
                    },
                    indoc! {r#"
                        [hello _world_!][1]

                        [1]: https://example.com

                           -----"#},
                );
            }

            #[test]
            fn collapsed_no_title() {
                check_link_and_thematic_break(
                    LinkDefinition {
                        url: "https://example.com".to_string(),
                        title: None,
                        reference: LinkReference::Collapsed,
                    },
                    indoc! {r#"
                        [hello _world_!][]

                        [hello _world_!]: https://example.com

                           -----"#},
                );
            }

            #[test]
            fn shortcut_no_title() {
                check_link_and_thematic_break(
                    LinkDefinition {
                        url: "https://example.com".to_string(),
                        title: None,
                        reference: LinkReference::Shortcut,
                    },
                    indoc! {r#"
                        [hello _world_!]

                        [hello _world_!]: https://example.com

                           -----"#},
                );
            }

            #[test]
            fn inline_with_title() {
                check_link_and_thematic_break(
                    LinkDefinition {
                        url: "https://example.com".to_string(),
                        title: Some("my title".to_string()),
                        reference: LinkReference::Inline,
                    },
                    indoc! {r#"
                        [hello _world_!](https://example.com "my title")

                           -----"#},
                );
            }

            #[test]
            fn full_with_title() {
                check_link_and_thematic_break(
                    LinkDefinition {
                        url: "https://example.com".to_string(),
                        title: Some("my title".to_string()),
                        reference: LinkReference::Full("1".to_string()),
                    },
                    indoc! {r#"
                        [hello _world_!][1]

                        [1]: https://example.com "my title"

                           -----"#},
                );
            }

            #[test]
            fn collapsed_with_title() {
                check_link_and_thematic_break(
                    LinkDefinition {
                        url: "https://example.com".to_string(),
                        title: Some("my title".to_string()),
                        reference: LinkReference::Collapsed,
                    },
                    indoc! {r#"
                        [hello _world_!][]

                        [hello _world_!]: https://example.com "my title"

                           -----"#},
                );
            }

            #[test]
            fn shortcut_with_title() {
                check_link_and_thematic_break(
                    LinkDefinition {
                        url: "https://example.com".to_string(),
                        title: Some("my title".to_string()),
                        reference: LinkReference::Shortcut,
                    },
                    indoc! {r#"
                        [hello _world_!]

                        [hello _world_!]: https://example.com "my title"

                           -----"#},
                );
            }

            fn check_link_and_thematic_break(link: LinkDefinition, expect: &str) {
                let nodes = vec![
                    MdElem::Inline(Inline::Link(Link {
                        display: vec![
                            mdq_inline!("hello "),
                            mdq_inline!(span Emphasis [mdq_inline!("world")]),
                            mdq_inline!("!"),
                        ],
                        link,
                    })),
                    m_node!(MdElem::ThematicBreak),
                ];
                check_render(nodes, expect);
            }
        }

        mod image {
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

                           -----"#},
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

                        [1]: https://example.com

                           -----"#},
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

                        [hello _world_!]: https://example.com

                           -----"#},
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

                        [hello _world_!]: https://example.com

                           -----"#},
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

                           -----"#},
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

                        [1]: https://example.com "my title"

                           -----"#},
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

                        [hello _world_!]: https://example.com "my title"

                           -----"#},
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

                        [hello _world_!]: https://example.com "my title"

                           -----"#},
                );
            }

            fn check_image(link: LinkDefinition, expect: &str) {
                let nodes = vec![
                    MdElem::Inline(Inline::Image(Image {
                        alt: "hello _world_!".to_string(),
                        link,
                    })),
                    m_node!(MdElem::ThematicBreak),
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
                vec![link_elem(Link {
                    display: vec![mdq_inline!("link text")],
                    link: LinkDefinition {
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
                    link_elem(Link {
                        display: vec![mdq_inline!("link text one")],
                        link: LinkDefinition {
                            url: "https://example.com/1".to_string(),
                            title: None,
                            reference: LinkReference::Full("1".to_string()),
                        },
                    }),
                    link_elem(Link {
                        display: vec![mdq_inline!("link text two")],
                        link: LinkDefinition {
                            url: "https://example.com/2".to_string(),
                            title: None,
                            reference: LinkReference::Full("2".to_string()),
                        },
                    }),
                ],
                indoc! {r#"
                    [link text one][1]

                    [1]: https://example.com/1

                       -----

                    [link text two][2]

                    [2]: https://example.com/2"#},
            );
        }

        #[test]
        fn two_links_inline() {
            check_render_refs(
                vec![
                    link_elem(Link {
                        display: vec![mdq_inline!("link text one")],
                        link: LinkDefinition {
                            url: "https://example.com/1".to_string(),
                            title: None,
                            reference: LinkReference::Inline,
                        },
                    }),
                    link_elem(Link {
                        display: vec![mdq_inline!("link text two")],
                        link: LinkDefinition {
                            url: "https://example.com/2".to_string(),
                            title: None,
                            reference: LinkReference::Inline,
                        },
                    }),
                ],
                indoc! {r#"
                [link text one](https://example.com/1)

                   -----

                [link text two](https://example.com/2)"#},
            );
        }

        #[test]
        fn two_links_inline_no_thematic_break() {
            let mut options = MdWriterOptions::default_for_tests();
            options.include_thematic_breaks = false;
            check_render_refs_with(
                options,
                vec![
                    link_elem(Link {
                        display: vec![mdq_inline!("link text one")],
                        link: LinkDefinition {
                            url: "https://example.com/1".to_string(),
                            title: None,
                            reference: LinkReference::Inline,
                        },
                    }),
                    link_elem(Link {
                        display: vec![mdq_inline!("link text two")],
                        link: LinkDefinition {
                            url: "https://example.com/2".to_string(),
                            title: None,
                            reference: LinkReference::Inline,
                        },
                    }),
                ],
                indoc! {r#"
                [link text one](https://example.com/1)
                [link text two](https://example.com/2)"#},
            );
        }

        #[test]
        fn two_links_doc_pos_no_thematic_break() {
            let mut options = MdWriterOptions::default_for_tests();
            options.include_thematic_breaks = false;
            options.inline_options.link_format = LinkTransform::Reference;
            options.link_reference_placement = ReferencePlacement::Doc;
            check_render_refs_with(
                options,
                vec![
                    link_elem(Link {
                        display: vec![mdq_inline!("link text one")],
                        link: LinkDefinition {
                            url: "https://example.com/1".to_string(),
                            title: None,
                            reference: LinkReference::Inline,
                        },
                    }),
                    link_elem(Link {
                        display: vec![mdq_inline!("link text two")],
                        link: LinkDefinition {
                            url: "https://example.com/2".to_string(),
                            title: None,
                            reference: LinkReference::Inline,
                        },
                    }),
                ],
                indoc! {r#"
                [link text one][1]
                [link text two][2]

                [1]: https://example.com/1
                [2]: https://example.com/2"#},
            );
        }

        /// see [crate::output::link_transform::tests] for more extensive tests
        #[test]
        fn reference_transform_smoke_test() {
            check_render_refs_with(
                MdWriterOptions::new_with(|mdo| mdo.inline_options.link_format = LinkTransform::Reference),
                vec![link_elem(Link {
                    display: vec![mdq_inline!("link text")],
                    link: LinkDefinition {
                        url: "https://example.com".to_string(),
                        title: None,
                        reference: LinkReference::Inline, // note! inline, but will be transformed to full
                    },
                })],
                indoc! {r#"
                    [link text][1]

                    [1]: https://example.com"#},
            );
        }

        fn link_elem(link: Link) -> MdElem {
            MdElem::Inline(Inline::Link(link))
        }
    }

    mod image {
        use super::*;

        #[test]
        fn single_image() {
            check_render_refs(
                vec![image_elem(Image {
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
                    image_elem(Image {
                        alt: "alt text one".to_string(),
                        link: LinkDefinition {
                            url: "https://example.com/1.png".to_string(),
                            title: None,
                            reference: LinkReference::Full("1".to_string()),
                        },
                    }),
                    image_elem(Image {
                        alt: "alt text two".to_string(),
                        link: LinkDefinition {
                            url: "https://example.com/2.png".to_string(),
                            title: None,
                            reference: LinkReference::Full("2".to_string()),
                        },
                    }),
                ],
                indoc! {r#"
                    ![alt text one][1]

                    [1]: https://example.com/1.png

                       -----

                    ![alt text two][2]

                    [2]: https://example.com/2.png"#},
            );
        }

        /// see [crate::output::link_transform::tests] for more extensive tests
        #[test]
        fn reference_transform_smoke_test() {
            check_render_refs_with(
                MdWriterOptions::new_with(|mdo| mdo.inline_options.link_format = LinkTransform::Reference),
                vec![image_elem(Image {
                    alt: "alt text".to_string(),
                    link: LinkDefinition {
                        url: "https://example.com".to_string(),
                        title: None,
                        reference: LinkReference::Inline, // note! inline, but will be transformed to full
                    },
                })],
                indoc! {r#"
                    ![alt text][1]

                    [1]: https://example.com"#},
            );
        }

        fn image_elem(link: Image) -> MdElem {
            MdElem::Inline(Inline::Image(link))
        }
    }

    mod footnote {
        use super::*;

        #[test]
        fn single_line() {
            check_render_with_ctx(
                MdWriterOptions::default_for_tests(),
                (
                    MdContext::empty().with("a", md_elems!["Hello, world."]),
                    vec![
                        MdElem::Inline(Inline::Footnote("a".into())),
                        m_node!(MdElem::ThematicBreak),
                    ],
                ),
                indoc! {r#"
                    [^a]

                    [^a]: Hello, world.

                       -----"#},
            )
        }

        #[test]
        fn two_lines() {
            check_render_with_ctx(
                MdWriterOptions::default_for_tests(),
                (
                    MdContext::empty().with("a", md_elems!["Hello,\nworld."]),
                    vec![
                        MdElem::Inline(Inline::Footnote("a".into())),
                        m_node!(MdElem::ThematicBreak),
                    ],
                ),
                indoc! {r#"
                    [^a]

                    [^a]: Hello,
                      world.

                       -----"#},
            )
        }

        /// see [crate::output::footnote_transform::test] for more extensive tests
        #[test]
        fn footnote_transform_smoke_test() {
            let (ctx, graf) = footnote_a_in_paragraph();
            check_render_refs_with_ctx(
                MdWriterOptions::new_with(|mdo| mdo.inline_options.renumber_footnotes = true),
                (ctx, vec![MdElem::Paragraph(graf)]),
                indoc! {r#"
                    [^1]

                    [^1]: the footnote text"#},
            );
        }

        #[test]
        fn footnote_no_transform_smoke_test() {
            let (ctx, graf) = footnote_a_in_paragraph();
            check_render_refs_with_ctx(
                MdWriterOptions::new_with(|mdo| mdo.inline_options.renumber_footnotes = false),
                (ctx, vec![MdElem::Paragraph(graf)]),
                indoc! {r#"
                    [^a]

                    [^a]: the footnote text"#},
            );
        }

        fn footnote_a_in_paragraph() -> (MdContext, Paragraph) {
            (
                MdContext::empty().with("a", md_elems!("the footnote text")),
                Paragraph {
                    body: vec![Inline::Footnote("a".into())],
                },
            )
        }
    }

    mod annotation_and_footnote_layouts {
        use super::*;
        use indoc::indoc;

        #[test]
        fn link_and_footnote() {
            check_render_with_ctx(
                MdWriterOptions::default_for_tests(),
                (
                    MdContext::empty().with("a", md_elems!["this is my note"]),
                    md_elems![Paragraph {
                        body: vec![
                            mdq_inline!("Hello, "),
                            m_node!(Inline::Link {
                                display: vec![mdq_inline!("world"),],
                                link: LinkDefinition {
                                    url: "https://example.com".to_string(),
                                    title: None,
                                    reference: LinkReference::Full("1".to_string()),
                                }
                            }),
                            mdq_inline!("! This is interesting"),
                            Inline::Footnote("a".into()),
                            mdq_inline!("."),
                        ],
                    }],
                ),
                indoc! {r#"
                    Hello, [world][1]! This is interesting[^a].

                    [1]: https://example.com
                    [^a]: this is my note"#},
            );
        }

        #[test]
        fn both_in_sections() {
            check_render_with_ctx(
                MdWriterOptions::new_with(|mdo| {
                    mdo.link_reference_placement = ReferencePlacement::Section;
                    mdo.footnote_reference_placement = ReferencePlacement::Section;
                }),
                link_and_footnote_markdown(),
                indoc! {r#"
                    # First section

                    [link description][1] and then a thought[^a].

                    [1]: https://example.com
                    [^a]: the footnote

                       -----

                    # Second section

                    Second section contents."#},
            );
        }

        #[test]
        fn only_link_in_section() {
            check_render_with_ctx(
                MdWriterOptions::new_with(|mdo| {
                    mdo.link_reference_placement = ReferencePlacement::Section;
                    mdo.footnote_reference_placement = ReferencePlacement::Doc;
                }),
                link_and_footnote_markdown(),
                indoc! {r#"
                    # First section

                    [link description][1] and then a thought[^a].

                    [1]: https://example.com

                       -----

                    # Second section

                    Second section contents.

                       -----

                    [^a]: the footnote"#},
            );
        }

        #[test]
        fn no_sections_but_writing_to_sections() {
            check_render_with(
                MdWriterOptions::new_with(|mdo| {
                    mdo.link_reference_placement = ReferencePlacement::Section;
                    mdo.footnote_reference_placement = ReferencePlacement::Section;
                }),
                md_elems![Paragraph {
                    body: vec![m_node!(Inline::Link {
                        display: vec![mdq_inline!("link description")],
                        link: LinkDefinition {
                            url: "https://example.com".to_string(),
                            title: None,
                            reference: LinkReference::Full("1".to_string()),
                        },
                    }),]
                }],
                indoc! {r#"
                    [link description][1]

                    [1]: https://example.com"#},
            )
        }

        #[test]
        fn only_footnote_in_section() {
            check_render_with_ctx(
                MdWriterOptions::new_with(|mdo| {
                    mdo.link_reference_placement = ReferencePlacement::Doc;
                    mdo.footnote_reference_placement = ReferencePlacement::Section;
                }),
                link_and_footnote_markdown(),
                indoc! {r#"
                    # First section

                    [link description][1] and then a thought[^a].

                    [^a]: the footnote

                       -----

                    # Second section

                    Second section contents.

                       -----

                    [1]: https://example.com"#},
            );
        }

        #[test]
        fn both_bottom_of_doc() {
            check_render_with_ctx(
                MdWriterOptions::new_with(|mdo| {
                    mdo.link_reference_placement = ReferencePlacement::Doc;
                    mdo.footnote_reference_placement = ReferencePlacement::Doc;
                }),
                link_and_footnote_markdown(),
                indoc! {r#"
                    # First section

                    [link description][1] and then a thought[^a].

                       -----

                    # Second section

                    Second section contents.

                       -----

                    [1]: https://example.com
                    [^a]: the footnote"#},
            );
        }

        #[test]
        fn ordering() {
            check_render_with_ctx(
                MdWriterOptions::new_with(|mdo| {
                    mdo.link_reference_placement = ReferencePlacement::Doc;
                    mdo.footnote_reference_placement = ReferencePlacement::Doc;
                }),
                (
                    MdContext::empty()
                        .with("d", md_elems!["footnote 1"])
                        .with("c", md_elems!["footnote 2"]),
                    // Define them in the opposite order that we'd expect them
                    md_elems![Paragraph {
                        body: vec![
                            Inline::Footnote("d".into()),
                            Inline::Footnote("c".into()),
                            m_node!(Inline::Link {
                                display: vec![mdq_inline!("b-text")],
                                link: LinkDefinition {
                                    url: "https://example.com/b".to_string(),
                                    title: None,
                                    reference: LinkReference::Full("b".to_string()),
                                },
                            }),
                            m_node!(Inline::Link {
                                display: vec![mdq_inline!("a-text")],
                                link: LinkDefinition {
                                    url: "https://example.com/a".to_string(),
                                    title: None,
                                    reference: LinkReference::Full("a".to_string()),
                                },
                            }),
                        ]
                    }],
                ),
                indoc! {r#"
                    [^d][^c][b-text][b][a-text][a]

                    [a]: https://example.com/a
                    [b]: https://example.com/b
                    [^c]: footnote 2
                    [^d]: footnote 1"#},
            );
        }

        fn link_and_footnote_markdown() -> (MdContext, Vec<MdElem>) {
            let elems = md_elems![
                Section {
                    depth: 1,
                    title: vec![mdq_inline!("First section")],
                    body: md_elems![Paragraph {
                        body: vec![
                            m_node!(Inline::Link {
                                display: vec![mdq_inline!("link description")],
                                link: LinkDefinition {
                                    url: "https://example.com".to_string(),
                                    title: None,
                                    reference: LinkReference::Full("1".to_string()),
                                },
                            }),
                            mdq_inline!(" and then a thought"),
                            Inline::Footnote("a".into()),
                            mdq_inline!("."),
                        ],
                    }],
                },
                Section {
                    depth: 1,
                    title: vec![mdq_inline!("Second section")],
                    body: md_elems!["Second section contents."],
                },
            ];
            let ctx = MdContext::empty().with("a", md_elems!["the footnote"]);
            (ctx, elems)
        }
    }

    mod html {
        use super::*;

        #[test]
        fn inline() {
            check_render(
                md_elems![Paragraph {
                    body: vec![
                        Inline::Text(Text {
                            variant: TextVariant::Plain,
                            value: "Hello ".to_string()
                        }),
                        Inline::Text(Text {
                            variant: TextVariant::InlineHtml,
                            value: "<span class=\"greeting\">".to_string()
                        }),
                        Inline::Text(Text {
                            variant: TextVariant::Plain,
                            value: "world".to_string()
                        }),
                        Inline::Text(Text {
                            variant: TextVariant::InlineHtml,
                            value: "</span>".to_string()
                        }),
                    ]
                }],
                indoc! {r#"
                Hello <span class="greeting">world</span>"#},
            )
        }

        #[test]
        fn block_single_line() {
            check_render(vec![MdElem::BlockHtml("<div>".into())], indoc! {r#"<div>"#})
        }

        #[test]
        fn block_multi_line() {
            check_render(
                vec![MdElem::BlockHtml("<div\nselected>".into())],
                indoc! {r#"
                <div
                selected>"#},
            )
        }
    }

    fn check_render(nodes: Vec<MdElem>, expect: &str) {
        check_render_with(MdWriterOptions::default_for_tests(), nodes, expect);
    }

    fn check_render_with(options: MdWriterOptions, nodes: Vec<MdElem>, expect: &str) {
        check_render_with_ctx(options, (MdContext::empty(), nodes), expect)
    }

    fn check_render_with_ctx(options: MdWriterOptions, inputs: (MdContext, Vec<MdElem>), expect: &str) {
        let (ctx, nodes) = inputs;
        let mut wrapped = Vec::with_capacity(nodes.len());
        for node in nodes {
            wrapped.push(node);
        }
        check_render_refs_with_ctx(options, (ctx, wrapped), expect)
    }

    fn check_render_refs(nodes: Vec<MdElem>, expect: &str) {
        check_render_refs_with(MdWriterOptions::default_for_tests(), nodes, expect)
    }

    fn check_render_refs_with(options: MdWriterOptions, nodes: Vec<MdElem>, expect: &str) {
        check_render_refs_with_ctx(options, (MdContext::empty(), nodes), expect)
    }

    fn check_render_refs_with_ctx(options: MdWriterOptions, inputs: (MdContext, Vec<MdElem>), expect: &str) {
        let (ctx, nodes) = inputs;
        nodes.iter().for_each(|n| VARIANTS_CHECKER.see(n));

        let mut out = Output::without_text_wrapping(String::default());
        write_md(options, &mut out, &ctx, nodes.iter());
        let actual = out.take_underlying().unwrap();
        assert_eq!(&actual, expect);
    }
}
