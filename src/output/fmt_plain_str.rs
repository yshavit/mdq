use crate::md_elem::elem::*;
use std::borrow::Borrow;

pub(crate) fn inlines_to_plain_string<N: Borrow<Inline>>(inlines: &[N]) -> String {
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
        Inline::Span(Span { children, .. }) => build_inlines(out, children),
        Inline::Text(Text { value, .. }) => out.push_str(value),
        Inline::Link(Link { display: text, .. }) => build_inlines(out, text),
        Inline::Image(Image { alt, .. }) => out.push_str(alt),
        Inline::Footnote(footnote) => {
            out.push_str("[^");
            out.push_str(footnote.as_str());
            out.push(']');
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::md_elem::*;
    use indoc::indoc;

    use crate::util::utils_for_test::*;

    variants_checker!(VARIANTS_CHECKER = Inline {
        Span(Span{ variant: SpanVariant::Delete, .. }),
        Span(Span{ variant: SpanVariant::Emphasis, .. }),
        Span(Span{ variant: SpanVariant::Strong, .. }),
        Text(Text { variant: TextVariant::Plain, .. }),
        Text(Text { variant: TextVariant::Code, .. }),
        Text(Text { variant: TextVariant::Math, .. }),
        Text(Text { variant: TextVariant::InlineHtml, .. }),
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
    fn inline_html() {
        let md_elems = MdDoc::parse("Hello <foo> world", &ParseOptions::gfm()).unwrap().roots;
        unwrap!(&md_elems[0], MdElem::Paragraph(contents));
        unwrap!(&contents.body[1], inline @ Inline::Text(_));
        VARIANTS_CHECKER.see(inline);
        let actual = inlines_to_plain_string(&contents.body);
        assert_eq!(&actual, "Hello <foo> world");
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
        options.mdast_options.constructs.math_text = true;
        let md_elems = MdDoc::parse(md, &options).unwrap().roots;
        unwrap!(&md_elems[0], MdElem::Paragraph(p));
        p.body.iter().for_each(|inline| VARIANTS_CHECKER.see(inline));
        let actual = inlines_to_plain_string(&p.body);
        assert_eq!(&actual, expect);
    }
}
