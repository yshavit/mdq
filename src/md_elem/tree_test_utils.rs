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
        (span $which:ident [$($contents:expr),*$(,)?]) => {
            crate::md_elem::Inline::Span(Formatting {
                variant: crate::md_elem::SpanVariant::$which,
                children: vec![$($contents),*],
            })
        };
        ($text:literal) => {
            crate::md_elem::Inline::Text(Text {
                variant: crate::md_elem::TextVariant::Plain,
                value: $text.to_string(),
            })
        };
    }
    pub(crate) use mdq_inline;
}
