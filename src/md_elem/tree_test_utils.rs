#[cfg(test)]
pub(crate) use test_utils::*;

#[cfg(test)]
mod test_utils {
    macro_rules! md_elem {
        ( $($node_names:ident)::* {$($attr:ident: $val:expr),* $(,)?}) => {
            crate::md_elem::m_node!(MdElem::$($node_names)::* {$($attr: $val),*})
        };
        ($paragraph_text:literal) => {
            crate::md_elem::m_node!(MdElem::Paragraph{body: vec![mdq_inline!($paragraph_text)]})
        };
    }
    pub(crate) use md_elem;

    macro_rules! md_elems {
        [$($first:tt $( $(:: $($rest:ident)::* )? {$($attr:ident: $val:expr),*$(,)?})? ),*$(,)?] => {
            vec![$(
                md_elem!($first$( $(:: $($rest)::*)? { $($attr: $val),* })?)
                ),*
            ]
        };
    }
    pub(crate) use md_elems;

    macro_rules! mdq_inline {
        // todo replace this with the `inlines!` macro below
        (span $which:ident [$($contents:expr),*$(,)?]) => {
            crate::md_elem::elem::Inline::Span(Span {
                variant: crate::md_elem::elem::SpanVariant::$which,
                children: vec![$($contents),*],
            })
        };
        ($text:literal) => {
            crate::md_elem::elem::Inline::Text(Text {
                variant: crate::md_elem::elem::TextVariant::Plain,
                value: $text.to_string(),
            })
        };
    }
    use crate::md_elem::elem::BlockHtml;
    pub(crate) use mdq_inline;

    macro_rules! inlines {
        // Empty case
        [] => {
            Vec::<crate::md_elem::tree::elem::Inline>::new()
        };

        // String literal (optionally followed by more content)
        [$text:literal $(, $($rest:tt)*)?] => {
            {
                #[allow(unused_mut)]
                let mut result = vec![
                    crate::md_elem::tree::elem::Inline::Text(crate::md_elem::tree::elem::Text {
                        variant: crate::md_elem::tree::elem::TextVariant::Plain,
                        value: $text.to_string(),
                    })
                ];
                $(result.extend(inlines![$($rest)*]);)?
                result
            }
        };

        // Emphasis (optionally followed by more content)
        [em[$($content:tt)*] $(, $($rest:tt)*)?] => {
            {
                #[allow(unused_mut)]
                let mut result = vec![
                    crate::md_elem::tree::elem::Inline::Span(crate::md_elem::tree::elem::Span {
                        variant: crate::md_elem::tree::elem::SpanVariant::Emphasis,
                        children: inlines![$($content)*],
                    })
                ];
                $(result.extend(inlines![$($rest)*]);)?
                result
            }
        };

        // Strong (optionally followed by more content)
        [strong[$($content:tt)*] $(, $($rest:tt)*)?] => {
            {
                #[allow(unused_mut)]
                let mut result = vec![
                    crate::md_elem::tree::elem::Inline::Span(crate::md_elem::tree::elem::Span {
                        variant: crate::md_elem::tree::elem::SpanVariant::Strong,
                        children: inlines![$($content)*],
                    })
                ];
                $(result.extend(inlines![$($rest)*]);)?
                result
            }
        };

        // Link (optionally followed by more content)
        [link[$($display:tt)*] ($url:literal) $(, $($rest:tt)*)?] => {
            {
                #[allow(unused_mut)]
                let mut result = vec![
                    crate::md_elem::tree::elem::Inline::Link(crate::md_elem::tree::elem::Link::Standard(
                        crate::md_elem::tree::elem::StandardLink {
                            display: inlines![$($display)*],
                            link: crate::md_elem::tree::elem::LinkDefinition {
                                url: $url.to_string(),
                                title: None,
                                reference: crate::md_elem::tree::elem::LinkReference::Inline,
                            },
                        }
                    ))
                ];
                $(result.extend(inlines![$($rest)*]);)?
                result
            }
        };

        // Footnote, like `footnote["^1"]`
        [footnote[$val:literal] $(, $($rest:tt)*)?] => {
            {
                #[allow(unused_mut)]
                let mut result = vec![
                    crate::md_elem::tree::elem::Inline::Footnote(crate::md_elem::tree::elem::FootnoteId{
                        id: $val.to_string(),
                    })
                ];
                $(result.extend(inlines![$($rest)*]);)?
                result
            }
        };
    }
    pub(crate) use inlines;

    impl From<&str> for BlockHtml {
        fn from(value: &str) -> Self {
            Self {
                value: value.to_string(),
            }
        }
    }
}
