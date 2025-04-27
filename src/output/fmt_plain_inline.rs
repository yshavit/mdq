use crate::md_elem::elem::*;
use crate::md_elem::*;
use crate::output::fmt_plain_writer::NewlineCollapser;
use std::io::{Error, LineWriter, Write};

#[derive(Copy, Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PlainWriterOptions {
    pub include_breaks: bool,
}

/// A struct for writing [MdElem]s as plain text (as per `--output plain`)
///
/// This generally strips all Markdown-y aspects of from the elements, and leaves only the core text. For example,
/// `_emphasized text_` will be rendered simply as `"emphasized text"`, and `- a list` will be rendered just as
/// "`a list"`.
///
/// Links and images will have their URLs removed, leaving only the display/alt text.
#[derive(Copy, Clone, Default, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PlainWriter {
    options: PlainWriterOptions,
}

impl PlainWriter {
    /// Create a new `PlainWriter` with the given options
    pub fn with_options(options: PlainWriterOptions) -> Self {
        Self { options }
    }

    /// Writes the given nodes to the given writer.
    pub fn write<'md, I, W>(&self, nodes: I, out: &mut W)
    where
        I: IntoIterator<Item = &'md MdElem>,
        W: Write,
    {
        write_plain(out, self.options, nodes.into_iter())
    }
}

fn write_plain<'md, I, W>(out: &mut W, opts: PlainWriterOptions, nodes: I)
where
    I: Iterator<Item = &'md MdElem>,
    W: Write,
{
    write_top_level(out, opts, nodes).expect("while writing output");
}

fn write_top_level<'md, I, W>(out: &mut W, opts: PlainWriterOptions, nodes: I) -> std::io::Result<()>
where
    I: Iterator<Item = &'md MdElem>,
    W: Write,
{
    let newlines_max = if opts.include_breaks { 2 } else { 1 };
    let mut writer = NewlineCollapser::new(LineWriter::new(out), newlines_max);
    write_plain_result(&mut writer, nodes)?;
    writer.flush()?;
    if writer.have_pending_newlines() {
        writeln!(writer.take_underlying())?;
    }
    Ok(())
}

fn write_plain_result<'md, I, W>(out: &mut W, nodes: I) -> Result<(), Error>
where
    I: Iterator<Item = &'md MdElem>,
    W: Write,
{
    let mut saw_any = false;
    for node in nodes {
        saw_any = true;
        write_node(out, node)?;
    }
    if saw_any {
        writeln!(out)?;
    }
    Ok(())
}

fn write_node<W>(out: &mut W, node: &MdElem) -> Result<(), Error>
where
    W: Write,
{
    match node {
        MdElem::Doc(doc) => write_plain_result(out, doc.iter()),
        MdElem::BlockQuote(block) => write_plain_result(out, block.body.iter()),
        MdElem::CodeBlock(block) => {
            if !block.value.is_empty() {
                writeln!(out, "{}", block.value)?;
                writeln!(out)?;
            }
            Ok(())
        }
        MdElem::Inline(inline) => write_inline(out, inline),
        MdElem::List(List { items, .. }) => {
            for item in items {
                write_plain_result(out, item.item.iter())?;
                writeln!(out)?;
            }
            Ok(())
        }
        MdElem::Paragraph(p) => {
            write_inlines(out, &p.body)?;
            if !p.body.is_empty() {
                writeln!(out)?;
                writeln!(out)?;
            }
            Ok(())
        }
        MdElem::Section(s) => {
            write_inlines(out, &s.title)?;
            writeln!(out)?;
            writeln!(out)?;
            write_plain_result(out, s.body.iter())
        }
        MdElem::Table(t) => {
            for row in &t.rows {
                let cols = row.iter().peekable();
                write_table_line(out, cols)?;
            }
            Ok(())
        }
        MdElem::BlockHtml(h) => {
            writeln!(out, "{}", h)?;
            writeln!(out)
        }
        MdElem::ThematicBreak => Ok(()),
    }
}

fn write_table_line<'md, W, I>(out: &mut W, line: I) -> Result<(), Error>
where
    W: Write,
    I: Iterator<Item = &'md TableCell>,
{
    let mut last_was_nonempty = false;
    for cell in line {
        if cell.is_empty() {
            continue;
        }
        if last_was_nonempty {
            write!(out, " ")?;
        }
        last_was_nonempty = true; // checked cell.is_empty() above
        for span in cell {
            write_inline(out, span)?;
        }
    }
    writeln!(out)?;
    Ok(())
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
        Inline::Span(Span { children, .. }) => write_inlines(out, children),
        Inline::Image(Image { alt, .. }) => write!(out, "{alt}"),
        Inline::Link(Link { display: text, .. }) => write_inlines(out, text),
        Inline::Text(Text { value, .. }) => write!(out, "{value}"),
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::util::utils_for_test::*;
    use indoc::indoc;

    variants_checker!(VARIANTS_CHECKER = MdElem {
        Doc(_),
        BlockQuote(_),
        CodeBlock(_),
        Inline(_),
        List(_),
        Paragraph(_),
        Section(_),
        Table(_),
        BlockHtml(_),
        ThematicBreak,
    });

    #[test]
    fn doc_empty() {
        let empty = vec![];
        let doc = MdElem::Doc(empty);
        check_plain(
            doc,
            Expect {
                with_breaks: "",
                no_breaks: "",
            },
        );
    }

    #[test]
    fn doc_not_empty() {
        let body = md_elems!("hello", "world");
        let doc = MdElem::Doc(body);
        check_plain(
            doc,
            Expect {
                with_breaks: "hello\n\nworld\n",
                no_breaks: "hello\nworld\n",
            },
        );
    }

    #[test]
    fn block_quote_empty() {
        let md_elem = md_elem!(BlockQuote { body: vec![] });
        check_plain(
            match_or_panic!(md_elem => MdElem::BlockQuote(_)),
            Expect {
                with_breaks: "",
                no_breaks: "",
            },
        )
    }

    #[test]
    fn block_quote_not_empty() {
        let md_elem = md_elem!(BlockQuote {
            body: md_elems!("hello, world")
        });
        check_plain(
            match_or_panic!(md_elem => MdElem::BlockQuote(_)),
            Expect {
                with_breaks: "hello, world\n",
                no_breaks: "hello, world\n",
            },
        )
    }

    #[test]
    fn code_block_empty() {
        let md_elem = md_elem!(CodeBlock {
            variant: CodeVariant::Code(None),
            value: "".to_string()
        });
        check_plain(
            match_or_panic!(md_elem => MdElem::CodeBlock(_)),
            Expect {
                with_breaks: "",
                no_breaks: "",
            },
        )
    }

    #[test]
    fn code_block_not_empty() {
        let md_elem = md_elem!(CodeBlock {
            variant: CodeVariant::Code(None),
            value: "hello, world".to_string()
        });
        check_plain(
            match_or_panic!(md_elem => MdElem::CodeBlock(_)),
            Expect {
                with_breaks: "hello, world\n",
                no_breaks: "hello, world\n",
            },
        )
    }

    #[test]
    fn inline() {
        let inline =
            mdq_inline!(span Emphasis [mdq_inline!("hello, "), mdq_inline!(span Strong [mdq_inline!("world")])]);
        check_plain(
            MdElem::Inline(inline),
            Expect {
                with_breaks: "hello, world\n",
                no_breaks: "hello, world\n",
            },
        );
    }

    #[test]
    fn list_empty() {
        let md_elem = md_elem!(List {
            starting_index: None,
            items: vec![]
        });
        check_plain(
            match_or_panic!(md_elem => MdElem::List(_)),
            Expect {
                with_breaks: "",
                no_breaks: "",
            },
        );
    }

    #[test]
    fn list_one_item() {
        let md_elem = md_elem!(List {
            starting_index: None,
            items: vec![ListItem {
                checked: None,
                item: md_elems!("only item"),
            }]
        });
        check_plain(
            match_or_panic!(md_elem => MdElem::List(_)),
            Expect {
                with_breaks: "only item\n",
                no_breaks: "only item\n",
            },
        );
    }

    #[test]
    fn list_two_items() {
        let md_elem = md_elem!(List {
            starting_index: Some(1),
            items: vec![
                ListItem {
                    checked: None,
                    item: md_elems!("first item"),
                },
                ListItem {
                    checked: Some(true),
                    item: md_elems!("second item"),
                }
            ]
        });
        check_plain(
            match_or_panic!(md_elem => MdElem::List(_)),
            Expect {
                with_breaks: "first item\n\nsecond item\n",
                no_breaks: "first item\nsecond item\n",
            },
        );
    }

    #[test]
    fn paragraph() {
        let md_elem = md_elem!("hello, world");
        check_plain(
            match_or_panic!(md_elem => MdElem::Paragraph(_)),
            Expect {
                with_breaks: "hello, world\n",
                no_breaks: "hello, world\n",
            },
        );
    }

    #[test]
    fn section() {
        let md_elem = md_elem!(Section {
            depth: 1,
            title: vec![mdq_inline!("section heading")],
            body: md_elems!("section body"),
        });
        check_plain(
            match_or_panic!(md_elem => MdElem::Section(_)),
            Expect {
                with_breaks: "section heading\n\nsection body\n",
                no_breaks: "section heading\nsection body\n",
            },
        );
    }

    #[test]
    fn table() {
        let md_elem = md_elem!(Table {
            alignments: vec![None, Some(ColumnAlignment::Center)],
            rows: vec![
                vec![vec![mdq_inline!("1A")], vec![mdq_inline!("1B")]],
                vec![vec![mdq_inline!("2A")], vec![mdq_inline!("2B")]],
            ]
        });
        check_plain(
            match_or_panic!(md_elem => MdElem::Table(_)),
            Expect {
                with_breaks: "1A 1B\n2A 2B\n",
                no_breaks: "1A 1B\n2A 2B\n",
            },
        );
    }

    #[test]
    fn table_with_empty_cells() {
        let md_elem = md_elem!(Table {
            alignments: vec![None, Some(ColumnAlignment::Center)],
            rows: vec![
                vec![cell(["a"]), cell(["b"]), cell(["c"])],
                vec![cell(["a"]), cell([]), cell(["c"])],
                vec![cell(["a"]), cell([]), cell([]), cell(["d"])],
                vec![cell([]), cell(["b"]), cell([])],
            ]
        });
        check_plain(
            match_or_panic!(md_elem => MdElem::Table(_)),
            Expect {
                with_breaks: "a b c\na c\na d\nb\n",
                no_breaks: "a b c\na c\na d\nb\n",
            },
        );
    }

    #[test]
    fn html() {
        let md_elem = MdElem::BlockHtml("<div>".into());
        check_plain(
            match_or_panic!(md_elem => MdElem::BlockHtml(_)),
            Expect {
                with_breaks: "<div>\n",
                no_breaks: "<div>\n",
            },
        );
    }

    #[test]
    fn thematic_break() {
        let md_elem = MdElem::ThematicBreak;
        check_plain(
            match_or_panic!(md_elem => MdElem::ThematicBreak),
            Expect {
                with_breaks: "",
                no_breaks: "",
            },
        );
    }

    #[test]
    fn list_item_one_paragraph() {
        let list_item = ListItem {
            checked: None,
            item: md_elems!("hello, world"),
        };
        check_plain(
            MdElem::List(List {
                starting_index: None,
                items: vec![list_item],
            }),
            Expect {
                with_breaks: "hello, world\n",
                no_breaks: "hello, world\n",
            },
        );
    }

    #[test]
    fn list_item_two_paragraphs() {
        let list_item = ListItem {
            checked: None,
            item: md_elems!("first", "second"),
        };
        check_plain(
            MdElem::List(List {
                starting_index: None,
                items: vec![list_item],
            }),
            Expect {
                with_breaks: "first\n\nsecond\n",
                no_breaks: "first\nsecond\n",
            },
        );
    }

    #[test]
    fn link() {
        let link = Link {
            display: vec![mdq_inline!("display text")],
            link: LinkDefinition {
                url: "https://example.com".to_string(),
                title: Some("the title".to_string()),
                reference: LinkReference::Inline,
            },
        };
        check_plain(
            MdElem::Inline(Inline::Link(link)),
            Expect {
                with_breaks: "display text\n",
                no_breaks: "display text\n",
            },
        );
    }

    #[test]
    fn image() {
        let image = Image {
            alt: "alt text".to_string(),
            link: LinkDefinition {
                url: "https://example.com".to_string(),
                title: Some("the title".to_string()),
                reference: LinkReference::Inline,
            },
        };
        check_plain(
            MdElem::Inline(Inline::Image(image)),
            Expect {
                with_breaks: "alt text\n",
                no_breaks: "alt text\n",
            },
        );
    }

    #[test]
    fn table_slice() {
        let table = Table {
            alignments: vec![None, Some(ColumnAlignment::Center)],
            rows: vec![
                vec![vec![mdq_inline!("1A")], vec![mdq_inline!("1B")]],
                vec![vec![mdq_inline!("2A")], vec![mdq_inline!("2B")]],
            ],
        };
        check_plain(
            MdElem::Table(table),
            Expect {
                with_breaks: "1A 1B\n2A 2B\n",
                no_breaks: "1A 1B\n2A 2B\n",
            },
        );
    }

    #[test]
    fn blocks_with_inlines() {
        let md = md_elem!(Paragraph {
            body: vec![
                mdq_inline!("hello "),
                mdq_inline!(span Emphasis [mdq_inline!("world")]),
                mdq_inline!("! sponsored by "),
                Inline::Link(Link {
                    display: vec![mdq_inline!("Example Corp")],
                    link: LinkDefinition {
                        url: "https://example.com".to_string(),
                        title: None,
                        reference: LinkReference::Inline
                    }
                }),
                mdq_inline!("."),
            ]
        });
        check_plain(
            md,
            Expect {
                with_breaks: "hello world! sponsored by Example Corp.\n",
                no_breaks: "hello world! sponsored by Example Corp.\n",
            },
        );
    }

    #[test]
    fn multiple_blocks() {
        let md = vec![
            md_elem!("paragraph 1"),
            md_elem!("paragraph 2"),
            md_elem!(BlockQuote {
                body: md_elems!("block quote paragraph 1", "block quote paragraph 2"),
            }),
            md_elem!(CodeBlock {
                variant: CodeVariant::Toml,
                value: "code block 1 line 1\ncode block 1 line 2".to_string()
            }),
            MdElem::BlockHtml("<hr>".into()),
            md_elem!("paragraph 3"),
            md_elem!(CodeBlock {
                variant: CodeVariant::Code(None),
                value: "code block 2".to_string()
            }),
            md_elem!(Section {
                depth: 2,
                title: vec![mdq_inline!("next section")],
                body: vec![md_elem!(CodeBlock {
                    variant: CodeVariant::Code(None),
                    value: "code block 3".to_string()
                }),],
            }),
        ];
        check_plain(
            MdElem::Doc(md),
            Expect {
                with_breaks: indoc! {r"
                paragraph 1

                paragraph 2

                block quote paragraph 1

                block quote paragraph 2

                code block 1 line 1
                code block 1 line 2

                <hr>

                paragraph 3

                code block 2

                next section

                code block 3
            "},
                no_breaks: indoc! {r"
                paragraph 1
                paragraph 2
                block quote paragraph 1
                block quote paragraph 2
                code block 1 line 1
                code block 1 line 2
                <hr>
                paragraph 3
                code block 2
                next section
                code block 3
            "},
            },
        )
    }

    struct Expect {
        with_breaks: &'static str,
        no_breaks: &'static str,
    }

    fn check_plain(input: MdElem, expect: Expect) {
        VARIANTS_CHECKER.see(&input);
        let mut bytes = Vec::with_capacity(expect.with_breaks.len());
        write_plain(
            &mut bytes,
            PlainWriterOptions { include_breaks: true },
            [&input].into_iter(),
        );
        let actual = String::from_utf8(bytes).expect("got invalid utf8");
        assert_eq!(actual, expect.with_breaks);

        let mut bytes = Vec::with_capacity(expect.no_breaks.len());
        write_plain(&mut bytes, PlainWriterOptions { include_breaks: false }, [input].iter());
        let actual = String::from_utf8(bytes).expect("got invalid utf8");
        assert_eq!(actual, expect.no_breaks);
    }

    fn cell<const N: usize>(texts: [&str; N]) -> TableCell {
        texts
            .map(|t| {
                Inline::Text(Text {
                    variant: TextVariant::Plain,
                    value: t.to_string(),
                })
            })
            .into_iter()
            .collect()
    }
}
