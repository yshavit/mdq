use crate::tree::{Inline, TextVariant};
use std::borrow::Borrow;

pub fn inlines_to_plain_string<N: Borrow<Inline>>(inlines: &[N]) -> String {
    let mut result = String::with_capacity(inlines.len() * 10); // random guess
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
        Inline::Span { children, .. } => build_inlines(out, children),
        Inline::Text { variant, value, .. } => {
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

    use crate::tree::{Inline, MdqNode, ReadOptions, SpanVariant, TextVariant};
    use crate::unwrap;
    use crate::utils_for_test::VariantsChecker;
    use lazy_static::lazy_static;
    use markdown::ParseOptions;

    lazy_static! {
        static ref VARIANTS_CHECKER: VariantsChecker<Inline> = crate::new_variants_checker!(Inline {
            Span { variant: SpanVariant::Delete, .. },
            Span { variant: SpanVariant::Emphasis, .. },
            Span { variant: SpanVariant::Strong, .. },
            Text { variant: TextVariant::Plain, .. },
            Text { variant: TextVariant::Code, .. },
            Text { variant: TextVariant::Math, .. },
            Text { variant: TextVariant::Html, .. },
            Link { .. },
            Image { .. },
            Footnote(_),
        });
    }

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
        let mdq_nodes = MdqNode::read(node, &ReadOptions::default()).unwrap();
        unwrap!(&mdq_nodes[0], MdqNode::Inline(inline));
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

    #[test]
    fn all_variants_checked() {
        VARIANTS_CHECKER.wait_for_all();
    }

    /// Because this is such simple functionality, we're just going to do a simple end-to-end test from original
    /// markdown to plain text.
    fn check(md: &str, expect: &str) {
        let mut options = ParseOptions::gfm();
        options.constructs.math_text = true;
        let node = markdown::to_mdast(md, &options).unwrap();
        let mdq_nodes = MdqNode::read(node, &ReadOptions::default()).unwrap();
        unwrap!(&mdq_nodes[0], MdqNode::Paragraph(p));
        p.body.iter().for_each(|inline| VARIANTS_CHECKER.see(inline));
        let actual = inlines_to_plain_string(&p.body);
        assert_eq!(&actual, expect);
    }
}
