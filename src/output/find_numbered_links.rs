use crate::md_elem::elem::{Inline, Link, LinkReference, StandardLink};
use crate::md_elem::{MdContext, MdElem};
use crate::output::{inlines_to_string, InlineElemOptions, LinkTransform, MdInlinesWriter};
use std::collections::BTreeSet;
use std::str::FromStr;

/// Find all numbers that should not be used when generating `[full links][123]`.
pub(crate) fn find_reserved_link_numbers<'md>(
    nodes: impl IntoIterator<Item = &'md MdElem>,
    ctx: &'md MdContext,
) -> BTreeSet<u64> {
    let mut finder = ReservedLinkNumbers {
        result: BTreeSet::new(),
        ctx,
    };
    finder.build_from_nodes(nodes);
    finder.result
}

struct ReservedLinkNumbers<'md> {
    result: BTreeSet<u64>,
    ctx: &'md MdContext,
}

impl<'md> ReservedLinkNumbers<'md> {
    fn build_from_nodes(&mut self, nodes: impl IntoIterator<Item = &'md MdElem>) {
        for node in nodes.into_iter() {
            match node {
                MdElem::Doc(doc) => self.build_from_nodes(doc),
                MdElem::BlockQuote(block) => self.build_from_nodes(&block.body),
                MdElem::List(list) => {
                    for li in &list.items {
                        self.build_from_nodes(&li.item);
                    }
                }
                MdElem::Section(section) => {
                    self.build_from_inlines(&section.title);
                    self.build_from_nodes(&section.body);
                }
                MdElem::Paragraph(p) => self.build_from_inlines(&p.body),
                MdElem::Table(table) => {
                    for row in &table.rows {
                        for col in row {
                            self.build_from_inlines(col);
                        }
                    }
                }
                MdElem::Inline(inline) => self.build_from_inlines(std::iter::once(inline)),
                MdElem::ThematicBreak | MdElem::CodeBlock(_) | MdElem::FrontMatter(_) | MdElem::BlockHtml(_) => {}
            }
        }
    }

    fn build_from_inlines(&mut self, inline: impl IntoIterator<Item = &'md Inline>) {
        for inline in inline.into_iter() {
            match inline {
                Inline::Footnote(footnote) => {
                    self.build_from_nodes(self.ctx.get_footnote(footnote));
                }
                Inline::Span(span) => self.build_from_inlines(&span.children),
                Inline::Link(link) => {
                    match link {
                        Link::Standard(link) => self.build_from_link(link),
                        Link::Autolink(_) => {} // autolinks are never just numeric
                    }
                }
                Inline::Image(_) | Inline::Text(_) => {}
            }
        }
    }

    fn build_from_link(&mut self, link: &StandardLink) {
        match &link.link.reference {
            LinkReference::Inline => {
                // inline links are never numeric
            }
            LinkReference::Full(_) | LinkReference::Collapsed | LinkReference::Shortcut => {
                // For full links, collapsed links, and shortcuts: if the display text is numeric, we'll reserve it.
                // (that will prevent output like `[123][4]`, which could be confusing. Otherwise, we won't.
                // Note that something like `[foo bar][1]` will not reserve the 1. That's because we'll be renumbering
                // it anyway.
                let options = InlineElemOptions {
                    link_format: LinkTransform::Keep,
                    renumber_footnotes: false,
                };
                let tmp_ctx = MdContext::empty();
                let mut writer = MdInlinesWriter::new(&tmp_ctx, options, &[]);
                let text = inlines_to_string(&mut writer, &link.display);
                self.build_from_text(&text);
            }
        }
    }

    fn build_from_text(&mut self, text: &str) {
        if let Ok(num) = u64::from_str(text) {
            self.result.insert(num);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::md_elem::{MdDoc, ParseOptions};
    use indoc::indoc;

    #[test]
    fn no_links() {
        check_link_nums("some markdown", []);
    }

    #[test]
    fn autolink() {
        check_link_nums("<https://example.com>", []);
    }

    #[test]
    fn collapsed_not_num() {
        check_link_nums(
            indoc! {r#"
            [apple][]

            [apple]: https://example.com"#},
            [],
        );
    }

    #[test]
    fn collapsed_with_num() {
        check_link_nums(
            indoc! {r#"
            [123][]

            [123]: https://example.com"#},
            [123],
        );
    }

    #[test]
    fn shortcut_not_num() {
        check_link_nums(
            indoc! {r#"
            [apple]

            [apple]: https://example.com"#},
            [],
        );
    }

    #[test]
    fn shortcut_with_num() {
        check_link_nums(
            indoc! {r#"
            [123]

            [123]: https://example.com"#},
            [123],
        );
    }

    #[test]
    fn full_not_num() {
        check_link_nums(
            indoc! {r#"
            [display text][a]

            [a]: https://example.com"#},
            [],
        );
    }

    #[test]
    fn full_with_num() {
        check_link_nums(
            indoc! {r#"
            [display text][123]

            [123]: https://example.com"#},
            [],
        );
    }

    #[test]
    fn full_with_num_display() {
        check_link_nums(
            indoc! {r#"
            [123][a]

            [456][7]

            [a]: https://example.com
            [7]: https://example.com"#},
            [123, 456], // but not 7; that link will turn into a collapsed
        );
    }

    #[test]
    fn within_some_nested_items() {
        check_link_nums(
            indoc! {r#"
            > Some quoting
            >
            > - list item including [123][] collapsed link

            # Section with [456] shortcut link

            Some text under it.

            [123]: https://example.com/a
            [456]: https://example.com/b
            "#},
            [123, 456],
        );
    }

    /// Simple little check based on parsed markdown. This makes it not quite unit-y, but very easy to create tests.
    fn check_link_nums<const N: usize>(md_text: &str, expected: [u64; N]) {
        let MdDoc { roots, ctx } = MdDoc::parse(md_text, &ParseOptions::gfm()).unwrap();
        let actual = find_reserved_link_numbers(&roots, &ctx);
        assert_eq!(actual, expected.into());
    }
}
