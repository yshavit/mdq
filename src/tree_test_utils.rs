#[cfg(test)]
mod test_utils {

    #[macro_export]
    macro_rules! mdq_node {
        ($node_type:tt {$($attr:ident: $val:expr),*}) => {
            MdqNode::$node_type($node_type{$($attr: $val),*})
        };
        ($paragraph_text:literal) => {
            crate::mdq_node!(Paragraph{body: vec![crate::mdq_inline!($paragraph_text)]})
        };
    }

    #[macro_export]
    macro_rules! mdq_nodes {
        [$($node_type:tt {$($attr:ident: $val:expr),*$(,)?}),*$(,)?] => {
            vec![$(
                crate::mdq_node!($node_type {
                    $($attr: $val),*
                })
                ),*
            ]
        };
        [$($paragraph_text:literal),*$(,)?] => {
            vec![$(
                    crate::mdq_node!($paragraph_text)
                ),*
            ]
        }
    }

    #[macro_export]
    macro_rules! mdq_inline {
        (span $which:ident [$($contents:expr),*$(,)?]) => {
            crate::tree::Inline::Span {
                variant: crate::tree::SpanVariant::$which,
                children: vec![$($contents),*],
            }
        };
        ($text:literal) => {
            crate::tree::Inline::Text {
                variant: crate::tree::TextVariant::Plain,
                value: $text.to_string(),
            }
        };
    }
}
