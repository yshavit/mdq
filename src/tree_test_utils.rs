pub use test_utils::*;

// We this file's contents from prod by putting them in a submodule guarded by cfg(test), but then "pub use" it to
// export its contents.
#[cfg(test)]
mod test_utils {
    use std::collections::HashSet;
    use std::sync::{Arc, Mutex};
    use std::{thread, time};

    use regex::Regex;

    use crate::tree::MdqNode;

    macro_rules! nodes_matcher {
        {$($variant:ident$(($payload:pat))?),* $(,)?} => {
            {
                let variant_to_name = Regex::new("\\(.*").unwrap();
                None.map(|n: MdqNode| match n {
                    $(MdqNode::$variant$(($payload))? => {},)*
                });
                vec![$(variant_to_name.replace(stringify!($variant), "").to_string(),)*].into_iter().collect()
            }
        };
    }

    #[macro_export]
    macro_rules! mdq_node {
        ($node_type:tt {$($attr:ident: $val:expr),*}) => {
            MdqNode::$node_type($node_type{$($attr: $val),*})
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
                variant: crate::tree::InlineVariant::Text,
                value: $text.to_string(),
            }
        };
    }

    pub struct MdqVariantsChecker {
        require: Arc<Mutex<HashSet<String>>>,
    }

    // TODO unify this with the one in tree.rs.
    // I can do that by making this generic, as well as making nodes_matcher! work with generics by having it pass in
    // the type as an arg (e.g. `mdast::Node` or `MdqNode`
    impl MdqVariantsChecker {
        pub fn new() -> Self {
            let all_node_names = nodes_matcher! {
                Root(_),
                Header(_),
                Paragraph(_),
                BlockQuote(_),
                List(_),
                Table(_),
                ThematicBreak,
                CodeBlock(_),
                Inline(_),
            };
            Self {
                require: Arc::new(Mutex::new(all_node_names)),
            }
        }

        pub fn see(&self, node: &MdqNode) {
            let node_debug = format!("{:?}", node);
            let re = Regex::new(r"^\w+").unwrap();
            let node_name = re.find(&node_debug).unwrap().as_str();
            self.require.lock().map(|mut set| set.remove(node_name)).unwrap();
        }

        pub fn wait_for_all(&self) {
            let timeout = time::Duration::from_millis(500);
            let retry_delay = time::Duration::from_millis(50);
            let start = time::Instant::now();
            loop {
                if self.require.lock().map(|set| set.is_empty()).unwrap() {
                    break;
                }
                if start.elapsed() >= timeout {
                    let mut remaining: Vec<String> = self
                        .require
                        .lock()
                        .map(|set| set.iter().map(|s| s.to_owned()).collect())
                        .unwrap();
                    remaining.sort();
                    panic!(
                        "Timed out, and missing {} variants:\n- {}",
                        remaining.len(),
                        remaining.join("\n- ")
                    )
                }
                thread::sleep(retry_delay);
            }
        }
    }
}
