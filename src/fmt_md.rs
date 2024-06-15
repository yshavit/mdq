use std::borrow::Borrow;
use std::cmp::max;
use std::collections::{HashMap, HashSet};
use std::fmt::Alignment;
use std::io::Write;

use crate::output::{Block, Output};
use crate::str_utils::{pad_to, standard_align};
use crate::tree::*;

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

pub fn write_md<N, W>(options: &MdOptions, out: &mut Output<W>, nodes: &[N])
where
    N: Borrow<MdqNode>,
    W: Write,
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
    writer_state.write_link_definitions(out);
    writer_state.write_footnote_definitions(out);
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
    fn write_md<N, W>(&mut self, out: &mut Output<W>, nodes: &'a [N])
    where
        N: Borrow<MdqNode>,
        W: Write,
    {
        for node in nodes {
            self.write_one_md(out, node.borrow());
        }
    }

    pub fn write_one_md<W>(&mut self, out: &mut Output<W>, node: &'a MdqNode)
    where
        W: Write,
    {
        match node {
            MdqNode::Root(Root { body }) => self.write_md(out, body),
            MdqNode::Header(Header { depth, title, body }) => {
                out.with_block(Block::Plain, |out| {
                    for _ in 0..*depth {
                        out.write_str("#");
                    }
                    out.write_str(" ");
                    self.write_line(out, title);
                });
                self.write_md(out, body);
                if matches!(self.opts.link_reference_options, ReferencePlacement::BottomOfSection) {
                    self.write_link_definitions(out);
                }
                if matches!(
                    self.opts.footnote_reference_options,
                    ReferencePlacement::BottomOfSection
                ) {
                    self.write_footnote_definitions(out);
                }
            }
            MdqNode::Paragraph(Paragraph { body }) => {
                out.with_block(Block::Plain, |out| {
                    self.write_line(out, body);
                });
            }
            MdqNode::BlockQuote(BlockQuote { body }) => {
                out.with_block(Block::Quote, |out| {
                    self.write_md(out, body);
                });
            }
            MdqNode::List(List { starting_index, items }) => {
                out.with_block(Block::Plain, |out| {
                    let mut index = starting_index.clone();
                    let mut prefix = String::with_capacity(8); // enough for "12. [ ] "
                    for item in items {
                        prefix.clear();
                        match &mut index {
                            None => prefix.push_str("- "),
                            Some(i) => {
                                std::fmt::Write::write_fmt(&mut prefix, format_args!("{}. ", &i)).unwrap();
                                *i += 1;
                            }
                        };
                        if let Some(checked) = &item.checked {
                            prefix.push('[');
                            prefix.push(if *checked { 'x' } else { ' ' });
                            prefix.push_str("] ");
                        }
                        out.write_str(&prefix);
                        out.with_block(Block::Inlined(prefix.len()), |out| {
                            self.write_md(out, &item.item);
                        });
                    }
                });
            }
            MdqNode::Table(Table { alignments, rows }) => {
                let mut row_strs = Vec::with_capacity(rows.len());

                let mut column_widths = [0].repeat(alignments.len());
                if !alignments.is_empty() {
                    for (idx, alignment) in alignments.iter().enumerate() {
                        let width = match standard_align(alignment) {
                            Alignment::Left | Alignment::Right => 2,
                            Alignment::Center => 3,
                        };
                        column_widths[idx] = width;
                    }
                }

                // Pre-calculate all the cells, and also how wide each column needs to be
                for row in rows {
                    let mut col_strs = Vec::with_capacity(row.len());
                    for (idx, col) in row.iter().enumerate() {
                        let col_str = self.line_to_string(col);
                        // Extend the row_sizes if needed. This happens if we had fewer alignments than columns in any row.
                        // I'm not sure if that's possible, but it's easy to guard against
                        while column_widths.len() < idx {
                            column_widths.push(0);
                        }
                        column_widths[idx] = max(column_widths[idx], col_str.len());
                        col_strs.push(col_str);
                    }
                    row_strs.push(col_strs);
                }

                // Create column formatters for each column
                let write_row = |out: &mut Output<W>, row: Vec<String>| {
                    if row.is_empty() {
                        out.write_str("||\n");
                        return;
                    }
                    out.write_char('|');
                    for (idx, col) in row.iter().enumerate() {
                        out.write_char(' ');
                        pad_to(out, &col, *column_widths.get(idx).unwrap_or(&0), alignments.get(idx));
                        out.write_str(" |");
                    }
                    out.write_char('\n');
                };

                let mut rows_iter = row_strs.into_iter();

                // First row
                let Some(first_row) = rows_iter.next() else {
                    return; // unexpected!
                };
                write_row(out, first_row);

                // Headers
                if !alignments.is_empty() {
                    out.write_char('|');
                    for (idx, align) in alignments.iter().enumerate() {
                        let width = column_widths
                            .get(idx)
                            .unwrap_or_else(|| match standard_align(align) {
                                Alignment::Left | Alignment::Right => &2,
                                Alignment::Center => &3,
                            })
                            .to_owned()
                            + 2; // +2 for the ' ' padding on either side
                        match standard_align(align) {
                            Alignment::Left => {
                                out.write_char(':');
                                out.write_str(&"-".repeat(width - 1));
                            }
                            Alignment::Right => {
                                out.write_str(&"-".repeat(width - 1));
                                out.write_char(':');
                            }
                            Alignment::Center => {
                                out.write_char(':');
                                out.write_str(&"-".repeat(width - 2));
                                out.write_char(':');
                            }
                        };
                        out.write_char('|');
                    }
                    out.write_char('\n');
                }

                // And finally, the rows
                for row in rows_iter {
                    write_row(out, row);
                }
            }
            MdqNode::ThematicBreak => {
                // out.with_block(Block::Plain, |out| out.write_str("***"));
            }
            MdqNode::CodeBlock(CodeBlock { variant, value }) => {
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
                            Some(meta.to_string())
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
            MdqNode::Inline(inline) => {
                self.write_inline_element(out, inline);
            }
        }
    }

    pub fn write_line<E, W>(&mut self, out: &mut Output<W>, elems: &'a [E])
    where
        E: Borrow<Inline>,
        W: Write,
    {
        for elem in elems {
            self.write_inline_element(out, elem.borrow());
        }
    }

    pub fn write_inline_element<W>(&mut self, out: &mut Output<W>, elem: &'a Inline)
    where
        W: Write,
    {
        match elem {
            Inline::Span { variant, children } => {
                let surround = match variant {
                    SpanVariant::Delete => "~~",
                    SpanVariant::Emphasis => "*",
                    SpanVariant::Strong => "**",
                };
                out.write_str(surround);
                self.write_line(out, children);
                out.write_str(surround);
            }
            Inline::Text { variant, value } => {
                let surround = match variant {
                    InlineVariant::Text => "",
                    InlineVariant::Code => "`",
                    InlineVariant::Math => "$",
                    InlineVariant::Html => "",
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
        W: Write,
        F: FnOnce(&mut Self, &mut Output<W>),
    {
        out.write_str("![");
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

    fn write_link_definitions<W>(&mut self, out: &mut Output<W>)
    where
        W: Write,
    {
        if self.pending_references.links.is_empty() && self.seen_footnotes.is_empty() {
            return;
        }
        out.with_block(Block::Plain, move |out| {
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
                out.write_char('\n');
            }
        });
    }

    fn write_footnote_definitions<W>(&mut self, out: &mut Output<W>)
    where
        W: Write,
    {
        // TODO combine this block with the link definitions
        out.with_block(Block::Plain, move |out| {
            // TODO sort them
            let mut defs_to_write: Vec<_> = self.pending_references.footnotes.drain().collect();
            defs_to_write.sort_by_key(|&kv| kv.0);

            for (link_ref, text) in defs_to_write {
                out.write_str("[^");
                out.write_str(link_ref);
                out.write_str("]: ");
                out.with_block(Block::Inlined(0), |out| {
                    self.write_md(out, text);
                    out.write_char('\n');
                });
            }
        });
    }

    fn write_url_title<W>(&mut self, out: &mut Output<W>, title: &Option<String>)
    where
        W: Write,
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
        let bytes: Vec<u8> = Vec::with_capacity(line.len() * 10); // rough guess
        let mut out = Output::new(bytes);
        self.write_line(&mut out, line);
        let bytes = out.take_underlying().unwrap();
        String::from_utf8(bytes).unwrap()
    }
}

#[cfg(test)]
pub mod tests {
    use std::fmt::Error;

    use indoc::indoc;
    use lazy_static::lazy_static;

    use crate::fmt_md::MdOptions;
    use crate::mdq_node;
    use crate::mdq_nodes;
    use crate::output::Output;
    use crate::tree::*;
    use crate::tree_test_utils::*;

    use super::write_md;

    lazy_static! {
        static ref VARIANTS_CHECKER: MdqVariantsChecker = MdqVariantsChecker::new();
    }

    #[test]
    fn empty() {
        check_render(vec![], indoc! {r#""#});
    }

    #[test]
    fn root() {
        {
            // empty body
            check_render(mdq_nodes![Root { body: vec![] }], indoc! {r#""#});
        }
        {
            // one paragraph
            check_render(
                mdq_nodes![Root {
                    body: mdq_nodes![Paragraph {
                        body: vec![Inline::Text {
                            variant: InlineVariant::Text,
                            value: "Hello, world".to_string()
                        }]
                    }]
                }],
                indoc! {r#"
                Hello, world"#},
            );
        }
        {
            // one paragraph
            check_render(
                mdq_nodes![Root {
                    body: mdq_nodes![
                        Paragraph {
                            body: vec![Inline::Text {
                                variant: InlineVariant::Text,
                                value: "First".to_string()
                            }]
                        },
                        Paragraph {
                            body: vec![Inline::Text {
                                variant: InlineVariant::Text,
                                value: "Second".to_string()
                            }]
                        },
                    ]
                }],
                indoc! {r#"
                    First

                    Second"#},
            );
        }
    }

    #[test]
    fn block_quote() {
        {
            // single level
            check_render(
                mdq_nodes![BlockQuote {
                    body: mdq_nodes![Paragraph {
                        body: vec![Inline::Text {
                            variant: InlineVariant::Text,
                            value: "Hello, world".to_string()
                        }]
                    }]
                }],
                indoc! {
                    r#"> Hello, world"#
                },
            );
        }
        {
            todo!("two levels")
        }
    }

    #[test]
    fn all_variants_checked() {
        VARIANTS_CHECKER.wait_for_all();
    }

    fn check_render(nodes: Vec<MdqNode>, expect: &str) {
        check_render_with(&MdOptions::default(), nodes, expect);
    }

    fn check_render_with(options: &MdOptions, nodes: Vec<MdqNode>, expect: &str) {
        nodes.iter().for_each(|n| VARIANTS_CHECKER.see(n));

        let mut out = Output::new(vec![]);
        write_md(options, &mut out, &nodes);
        let vec = out.take_underlying().unwrap();
        let actual = String::from_utf8(vec).map_err(|_| Error).unwrap();
        assert_eq!(&actual, expect);
    }
}
