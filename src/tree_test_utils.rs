#[cfg(test)]
mod test_utils {
    #[macro_export]
    macro_rules! md_elem {
        ( $($node_names:ident)::* {$($attr:ident: $val:expr),*}) => {
            crate::m_node!(MdElem::$($node_names)::* {$($attr: $val),*})
        };
        ($paragraph_text:literal) => {
            crate::m_node!(MdElem::LeafBlock::Paragraph{body: vec![crate::mdq_inline!($paragraph_text)]})
        };
    }

    #[macro_export]
    macro_rules! md_elems {
        [$($first:tt $( $(:: $($rest:ident)::* )? {$($attr:ident: $val:expr),*$(,)?})? ),*$(,)?] => {
            vec![$(
                crate::md_elem!($first$( $(:: $($rest)::*)? { $($attr: $val),* })?)
                ),*
            ]
        };
    }

    #[macro_export]
    macro_rules! mdq_inline {
        (span $which:ident [$($contents:expr),*$(,)?]) => {
            crate::tree::Inline::Formatting(Formatting {
                variant: crate::tree::FormattingVariant::$which,
                children: vec![$($contents),*],
            })
        };
        ($text:literal) => {
            crate::tree::Inline::Text(Text {
                variant: crate::tree::TextVariant::Plain,
                value: $text.to_string(),
            })
        };
    }
}
