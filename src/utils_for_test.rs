// We this file's contents from prod by putting them in a submodule guarded by cfg(test), but then "pub use" it to
// export its contents.
#[cfg(test)]
mod test_utils {
    /// Turn a pattern match into an `if let ... { else panic! }`.
    #[macro_export]
    macro_rules! unwrap {
        ($enum_value:expr, $enum_variant:pat) => {
            let node = $enum_value;
            let node_debug = format!("{:?}", node);
            let $enum_variant = node else {
                panic!("Expected {} but saw {}", stringify!($enum_variant), node_debug);
            };
        };
    }

    #[macro_export]
    macro_rules! test_delay_ms {
        ($i:literal) => {{
            time::Duration::from_millis(
                $i * option_env!("TEST_TIMEOUT_MULTIPLIER")
                    .map(|s| s.parse::<u64>().expect("bad value for TEST_TIMEOUT_MULTIPLIER"))
                    .unwrap_or(1),
            )
        }};
    }

    /// Creates a static object named `$name` that looks for all the variants of enum `E`.
    ///
    /// ```
    /// variants_checker(CHECKER_NAME = MyEnum { Variant1, Variant2(_), ... })
    /// ```
    ///
    /// You can also mark some variants as ignored; these will be added to the pattern match, but not be required to
    /// be seen:
    ///
    /// ```
    /// variants_checker(CHECKER_NAME = MyEnum { Variant1, ... } ignore { Variant2, ... } )
    /// ```
    ///
    /// If you see a compilation failure here, it means the call site is missing variants (or has an unknown
    /// variant).
    ///
    /// We can't use strum to do this for mdast::Node, because we don't own the Node code. Instead, we rely on a bit of
    /// trickery: we pass in a bunch of arms, and each gets [stringify!]'d and added to a set. Whenever we [see] an
    /// item, we remove the corresponding string from the set.
    ///
    /// This requires that each pattern matches exactly one shape of item; in other words, that there aren't any
    /// dead-code branches.
    #[macro_export]
    macro_rules! variants_checker {
        ($name:ident = $enum_type:ty { $($variant:pat),* $(,)? } $(ignore { $($ignore_variant:pat),* $(,)? })?) => {

            paste::paste!{
                pub struct [<VariantsChecker $name:lower:camel>] {
                    require: std::sync::Arc<std::sync::Mutex<std::collections::HashSet<String>>>,
                }

                impl [<VariantsChecker $name:lower:camel>] {
                    fn see(&self, node: &$enum_type) {
                        let node_str = match node {
                            $($enum_type::$variant => stringify!($variant),)*
                            $($($enum_type::$ignore_variant => {
                                panic!("unexpected variant: {}", stringify!($ignore_variant));
                            },)*)?
                        };
                        self.require.lock().map(|mut set| set.remove(node_str)).unwrap();
                    }

                    fn wait_for_all(&self) {
                        use std::{thread, time};

                        let timeout = crate::test_delay_ms!(500);
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

                lazy_static::lazy_static! {
                    static ref $name: [<VariantsChecker $name:lower:camel>] = [<VariantsChecker $name:lower:camel>] {
                        require: std::sync::Arc::new(
                            std::sync::Mutex::new(
                                vec![$(stringify!($variant).to_string(),)*].into_iter().collect()
                            )
                        )
                    };
                }

                #[test]
                fn [<all_variants_checked_for_ $name:lower:snake>]() {
                    $name.wait_for_all();
                }
            }
        };
    }
}
