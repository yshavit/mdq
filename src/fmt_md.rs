use std::borrow::Borrow;
use std::cmp::max;
use std::collections::HashSet;
use std::fmt::Alignment;
use std::io::Write;

use crate::fmt_str::{pad_to, standard_align};
use crate::output::{Block, Output};
use crate::output::Block::Inlined;
use crate::tree::{CodeVariant, Footnote, Inline, InlineVariant, Link, LinkReference, MdqNode, SpanVariant};

#[derive(Default)]
pub struct MdOptions {
    link_reference_options: LinkReferencePlacement,
    footnote_reference_options: FootnoteReferencePlacement,
}

pub enum LinkReferencePlacement {
    /// Show link URLs inline with their usages.
    ///
    /// ```
    /// [foo](https://example.com)
    ///      ^^^^^^^^^^^^^^^^^^^^^
    /// ```
    Inline,

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
    BottomOfSection(LinkReferenceStyle),

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
    BottomOfDoc(LinkReferenceStyle),
}

pub enum FootnoteReferencePlacement {
    /// See [LinkReferencePlacement::BottomOfSection]
    BottomOfSection,
    /// See [LinkReferencePlacement::BottomOfDoc]
    BottomOfDoc,
}

enum LinkReferenceStyle {
    KeepOriginal,
    // TODO add "always use id" option
}

impl Default for FootnoteReferencePlacement {
    fn default() -> Self {
        Self::BottomOfDoc
    }
}

impl Default for LinkReferencePlacement {
    fn default() -> Self {
        Self::BottomOfDoc(Default::default())
    }
}

impl Default for LinkReferenceStyle {
    fn default() -> Self {
        Self::KeepOriginal
    }
}

pub fn write_md<N, W>(options: &MdOptions, out: &mut Output<W>, nodes: &[N])
where
    N: Borrow<MdqNode>,
    W: Write,
{
    let pending_refs_capacity = match options.link_reference_options {
        LinkReferencePlacement::Inline => 0,
        _ => 8, // just a guess
    };

    let mut writer_state = MdWriterState {
        opts: options,
        seen_links: HashSet::with_capacity(pending_refs_capacity),
        seen_footnotes: HashSet::with_capacity(pending_refs_capacity),
        pending_references: PendingReferences {
            links: Vec::with_capacity(pending_refs_capacity),
            footnotes: Vec::with_capacity(8), // footnotes are never inline (as above, 8 is just a guess here)
        },
    };
    writer_state.write_md(out, nodes);
}

struct MdWriterState<'a> {
    opts: &'a MdOptions,
    seen_links: HashSet<&'a String>,
    seen_footnotes: HashSet<&'a String>,
    pending_references: PendingReferences<'a>,
}

struct PendingReferences<'a> {
    links: Vec<&'a Link>,
    footnotes: Vec<&'a Footnote>,
}

impl<'a> MdWriterState<'a> {
    fn write_md<N, W>(&mut self, out: &mut Output<W>, nodes: &[N])
    where
        N: Borrow<MdqNode>,
        W: Write,
    {
        for node in nodes {
            self.write_one_md(out, node.borrow());
        }
    }

    pub fn write_one_md<W>(&mut self, out: &mut Output<W>, node: &MdqNode)
    where
        W: Write,
    {
        match node {
            MdqNode::Root { body } => self.write_md(out, body),
            MdqNode::Header { depth, title, body } => {
                out.with_block(Block::Plain, |out| {
                    for _ in 0..*depth {
                        out.write_str("#");
                    }
                    out.write_str(" ");
                    self.write_line(out, title);
                });
                self.write_md(out, body);
            }
            MdqNode::Paragraph { body } => {
                out.with_block(Block::Plain, |out| {
                    self.write_line(out, body);
                });
            }
            MdqNode::BlockQuote { body } => {
                out.with_block(Block::Quote, |out| {
                    self.write_md(out, body);
                });
            }
            MdqNode::List { starting_index, items } => {
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
                        out.with_block(Inlined(prefix.len()), |out| {
                            self.write_md(out, &item.item);
                        });
                    }
                });
            }
            MdqNode::Table { alignments, rows } => {
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
            MdqNode::CodeBlock { variant, value } => {
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

    pub fn write_line<E, W>(&mut self, out: &mut Output<W>, elems: &[E])
    where
        E: Borrow<Inline>,
        W: Write,
    {
        for elem in elems {
            self.write_inline_element(out, elem.borrow());
        }
    }

    pub fn write_inline_element<W>(&mut self, out: &mut Output<W>, elem: &Inline)
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
                self.write_link_inline(out, link, |me, out| me.write_line(out, text));
            }
            Inline::Image { alt, link } => {
                out.write_char('!');
                self.write_link_inline(out, link, |_, out| out.write_str(alt));
            }
            Inline::Footnote(Footnote { label, .. }) => {
                out.write_str("[^");
                out.write_str(label);
                out.write_char(']');
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
    fn write_link_inline<W, F>(&mut self, out: &mut Output<W>, link: &Link, contents: F)
    where
        W: Write,
        F: FnOnce(&mut Self, &mut Output<W>),
    {
        out.write_str("![");
        contents(self, out);
        out.write_char(']');
        match &link.reference {
            LinkReference::Inline => {
                out.write_char('(');
                out.write_str(&link.url);
                if let Some(title) = &link.title {
                    out.write_str(" \"");
                    self.escape_title_to(out, title);
                    out.write_char('"');
                }
                out.write_char(')');
            }
            LinkReference::Full(identifier) => {
                out.write_char('[');
                out.write_str(identifier);
                out.write_char(']');
            }
            LinkReference::Collapsed => {
                out.write_str("[]");
            }
            LinkReference::Shortcut => {
                // nothing
            }
        }
    }

    fn escape_title_to<W>(&mut self, out: &mut Output<W>, title: &String)
    where
        W: Write,
    {
        // TODO escaping
        out.write_str(title);
    }

    fn line_to_string<E>(&mut self, line: &[E]) -> String
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
