use crate::fmt_str::{pad_to, standard_align, Paddable};
use crate::output::Block::Inlined;
use crate::output::{Block, Output};
use crate::tree::{CodeVariant, Inline, InlineVariant, MdqNode, SpanVariant};
use std::borrow::Borrow;
use std::cmp::max;
use std::fmt::Alignment;
use std::io::Write;

pub fn write_md<N, W>(out: &mut Output<W>, nodes: &[N])
where
    N: Borrow<MdqNode>,
{
    let mut iter = nodes.iter().peekable();
    while let Some(node) = iter.next() {
        write_one_md(out, node.borrow());
        if iter.peek().is_some() {
            write_one_md(out, &MdqNode::ThematicBreak)
        }
    }
}

pub fn write_one_md<W>(out: &mut Output<W>, node: &MdqNode)
where
    W: Write,
{
    match node {
        MdqNode::Root { body } => write_md(out, body),
        MdqNode::Header { depth, title, body } => {
            out.with_block(Block::Plain, |out| {
                for _ in 0..*depth {
                    out.write_str("#");
                }
                out.write_str(" ");
                write_line(out, title);
            });
            write_md(out, body);
        }
        MdqNode::Paragraph { body } => {
            out.with_block(Block::Plain, |out| {
                write_line(out, body);
            });
        }
        MdqNode::BlockQuote { body } => {
            out.with_block(Block::Quote, |out| {
                write_md(out, body);
            });
        }
        MdqNode::List {
            starting_index,
            items,
        } => {
            out.with_block(Block::Plain, |out| {
                let mut index = starting_index.clone();
                let mut prefix = String::with_capacity(8); // enough for "12. [ ] "
                for item in items {
                    prefix.clear();
                    match &mut index {
                        None => prefix.push_str("- "),
                        Some(i) => {
                            std::fmt::Write::write_fmt(&mut prefix, format_args!("{}. ", &i))
                                .unwrap();
                            *i += 1;
                        }
                    };
                    if let Some(checked) = &item.checked {
                        prefix.push('[');
                        prefix.push(if *checked { 'x' } else { ' ' });
                        prefix.push_str("] ");
                    }
                    out.with_block(Inlined(prefix.len()), |out| {
                        out.write_str(&prefix);
                        write_md(out, &item.item);
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
                    let col_str = line_to_string(col);
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
                    pad_to(
                        out,
                        &col,
                        *column_widths.get(idx).unwrap_or(&0),
                        alignments.get(idx),
                    );
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
            write_inline_element(out, inline);
        }
    }
}

pub fn write_line<E, W>(out: &mut Output<W>, elems: &[E])
where
    E: Borrow<Inline>,
    W: Write,
{
    for elem in elems {
        write_inline_element(out, elem.borrow());
    }
}

pub fn write_inline_element<W>(out: &mut Output<W>, elem: &Inline)
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
            write_line(out, children);
            out.write_str(surround);
        }
        Inline::Text { variant, value } => {
            let surround = match variant {
                InlineVariant::Text => "",
                InlineVariant::Code => "`",
                InlineVariant::Math => "$",
            };
            out.write_str(surround);
            out.write_str(value);
            out.write_str(surround);
        }
    }
}

fn line_to_string<E>(line: &[E]) -> String
where
    E: Borrow<Inline>,
{
    let bytes: Vec<u8> = Vec::with_capacity(line.len() * 10); // rough guess
    let mut out = Output::new(bytes);
    write_line(&mut out, line);
    let bytes = out.take_underlying().unwrap();
    String::from_utf8(bytes).unwrap()
}
