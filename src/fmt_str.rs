use crate::tree::{Formatting, Inline, Text, TextVariant};
use std::borrow::Borrow;

pub fn inlines_to_plain_string<N: Borrow<Inline>>(inlines: &[N]) -> String {
    let mut result = String::with_capacity(inlines.len() * 5); // random guess
    build_inlines(&mut result, inlines);
    result
}

fn build_inlines<N: Borrow<Inline>>(out: &mut String, inlines: &[N]) {
    for inline in inlines {
        build_inline(out, inline.borrow());
    }
}

fn build_inline(out: &mut String, elem: &Inline) {
    match elem {
        Inline::Formatting(Formatting { children, .. }) => build_inlines(out, children),
        Inline::Text(Text { variant, value, .. }) => {
            if !matches!(variant, TextVariant::Html) {
                out.push_str(value)
            }
        }
        Inline::Link { text, .. } => build_inlines(out, text),
        Inline::Image { alt, .. } => out.push_str(alt),
        Inline::Footnote(footnote) => {
            out.push_str("[^");
            out.push_str(&footnote.label);
            out.push(']');
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use indoc::indoc;

    use crate::tree::{Block, FormattingVariant, Inline, LeafBlock, MdElem, ReadOptions, TextVariant};
    use crate::unwrap;
    use markdown::ParseOptions;

    crate::variants_checker!(VARIANTS_CHECKER = Inline {
        Formatting(Formatting{ variant: FormattingVariant::Delete, .. }),
        Formatting(Formatting{ variant: FormattingVariant::Emphasis, .. }),
        Formatting(Formatting{ variant: FormattingVariant::Strong, .. }),
        Text(Text { variant: TextVariant::Plain, .. }),
        Text(Text { variant: TextVariant::Code, .. }),
        Text(Text { variant: TextVariant::Math, .. }),
        Text(Text { variant: TextVariant::Html, .. }),
        Link { .. },
        Image { .. },
        Footnote(_),
    });

    #[test]
    fn spans() {
        check("_hello world_", "hello world");
        check("**hello world**", "hello world");
        check("~~hello world~~", "hello world");
    }

    #[test]
    fn texts() {
        check("hello world", "hello world");
        check("`hello world`", "hello world");
        check("$hello world$", "hello world");
        // html is covered separately, since it isn't wrapped in a paragraph: see issue #34
    }

    #[test]
    fn text_html() {
        let node = markdown::to_mdast("<foo>", &ParseOptions::gfm()).unwrap();
        let md_elems = MdElem::read(node, &ReadOptions::default()).unwrap();
        unwrap!(&md_elems[0], MdElem::Inline(inline));
        VARIANTS_CHECKER.see(inline);
        let actual = inlines_to_plain_string(&[inline]);
        assert_eq!(&actual, "");
    }

    #[test]
    fn links() {
        check("[foo](https://example.com)", "foo");
        check("[foo _with emphasis_](https://example.com)", "foo with emphasis");
        check(
            indoc! {r#"
            [foo][1]

            [1]: https://example.com"#},
            "foo",
        )
    }

    #[test]
    fn images() {
        check("![foo](https://example.com)", "foo");
        check("![foo _with emphasis_](https://example.com)", "foo with emphasis"); // md is ignored in alt
        check(
            indoc! {r#"
            ![foo][1]

            [1]: https://example.com"#},
            "foo",
        )
    }

    #[test]
    fn footnote() {
        check(
            indoc! {r#"
            [^1]

            [^1]: my footnote"#},
            "[^1]",
        )
    }

    /// Because this is such simple functionality, we're just going to do a simple end-to-end test from original
    /// markdown to plain text.
    fn check(md: &str, expect: &str) {
        let mut options = ParseOptions::gfm();
        options.constructs.math_text = true;
        let node = markdown::to_mdast(md, &options).unwrap();
        let md_elems = MdElem::read(node, &ReadOptions::default()).unwrap();
        unwrap!(&md_elems[0], MdElem::Block(Block::LeafBlock(LeafBlock::Paragraph(p)))); // TODO can I use m_node here?
        p.body.iter().for_each(|inline| VARIANTS_CHECKER.see(inline));
        let actual = inlines_to_plain_string(&p.body);
        assert_eq!(&actual, expect);
    }
}
