use crate::fmt_plain_writer::NewlineCollapser;
use crate::tree::{Formatting, Image, Inline, Line, Link, List, Text};
use crate::tree_ref::{ListItemRef, MdElemRef};
use std::io::{Error, LineWriter, Write};

pub fn write_plain<'md, I, W>(out: &mut W, nodes: I)
where
    I: Iterator<Item = MdElemRef<'md>>,
    W: Write,
{
    write_top_level(out, nodes).expect("while writing output");
}

fn write_top_level<'md, I, W>(out: &mut W, nodes: I) -> std::io::Result<()>
where
    I: Iterator<Item = MdElemRef<'md>>,
    W: Write,
{
    let mut writer = NewlineCollapser::new(LineWriter::new(out), 2);
    write_plain_result(&mut writer, nodes)?;
    writer.flush()?;
    writeln!(writer.take_underlying())
}

pub fn write_plain_result<'md, I, W>(out: &mut W, mut nodes: I) -> Result<(), Error>
where
    I: Iterator<Item = MdElemRef<'md>>,
    W: Write,
{
    while let Some(node) = nodes.next() {
        write_node(out, node)?;
        writeln!(out)?;
        writeln!(out)?;
    }
    Ok(())
}

fn write_node<W>(out: &mut W, node: MdElemRef) -> Result<(), Error>
where
    W: Write,
{
    match node {
        MdElemRef::Doc(doc) => write_plain_result(out, doc.iter().map(|e| e.into())),
        MdElemRef::BlockQuote(block) => write_plain_result(out, block.body.iter().map(|e| e.into())),
        MdElemRef::CodeBlock(block) => writeln!(out, "{}", block.value),
        MdElemRef::Inline(inline) => write_inline(out, inline),
        MdElemRef::List(List { items, .. }) => {
            for item in items {
                write_plain_result(out, item.item.iter().map(|e| e.into()))?;
                writeln!(out, "")?;
            }
            Ok(())
        }
        MdElemRef::Paragraph(p) => write_inlines(out, &p.body),
        MdElemRef::Section(s) => {
            write_inlines(out, &s.title)?;
            writeln!(out, "")?;
            writeln!(out, "")?;
            write_plain_result(out, s.body.iter().map(|e| e.into()))
        }
        MdElemRef::Table(t) => {
            for row in &t.rows {
                let cols = row.iter().peekable();
                write_table_line(out, cols)?;
            }
            Ok(())
        }
        MdElemRef::Html(_) => Ok(()),
        MdElemRef::ThematicBreak => Ok(()),
        MdElemRef::ListItem(ListItemRef(_, item)) => write_plain_result(out, item.item.iter().map(|e| e.into())),
        MdElemRef::Link(Link { text, .. }) => write_inlines(out, text),
        MdElemRef::Image(Image { alt, .. }) => writeln!(out, "{alt}"),
        MdElemRef::TableSlice(t) => {
            for row in t.rows() {
                let cols = row.iter().filter_map(|c| c.as_deref()).peekable();
                write_table_line(out, cols)?;
            }
            Ok(())
        }
    }
}

fn write_table_line<'a, W, I>(out: &mut W, line: I) -> Result<(), Error>
where
    W: Write,
    I: Iterator<Item = &'a Line>,
{
    let mut line = line.peekable();
    while let Some(col) = line.next() {
        write_inlines(out, col)?;
        if line.peek().is_some() {
            write!(out, " ")?;
        }
    }
    writeln!(out, "")
}

fn write_inlines<W>(out: &mut W, inlines: &[Inline]) -> Result<(), Error>
where
    W: Write,
{
    for child in inlines {
        write_inline(out, child)?;
    }
    Ok(())
}

fn write_inline<W>(out: &mut W, inline: &Inline) -> Result<(), Error>
where
    W: Write,
{
    match inline {
        Inline::Footnote(_) => Ok(()),
        Inline::Formatting(Formatting { children, .. }) => write_inlines(out, children),
        Inline::Image(Image { alt, .. }) => write!(out, "{alt}"),
        Inline::Link(Link { text, .. }) => write_inlines(out, text),
        Inline::Text(Text { value, .. }) => write!(out, "{value}"),
    }
}
