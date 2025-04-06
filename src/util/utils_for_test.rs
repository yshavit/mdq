#[cfg(test)]
pub(crate) use test_utils::*;

// We this file's contents from prod by putting them in a submodule guarded by cfg(test), but then "pub use" it to
// export its contents.
#[cfg(test)]
mod test_utils {
    use crate::fmt_md::{MdOptions, ReferencePlacement};
    use crate::fmt_md_inlines::MdInlinesWriterOptions;
    use crate::link_transform::LinkTransform;
    use std::fmt::Debug;

    impl LinkTransform {
        pub fn default_for_tests() -> Self {
            Self::Keep
        }
    }

    impl ReferencePlacement {
        pub fn default_for_tests() -> Self {
            Self::Section
        }
    }

    impl MdOptions {
        pub fn default_for_tests() -> Self {
            Self {
                link_reference_placement: ReferencePlacement::default_for_tests(),
                footnote_reference_placement: ReferencePlacement::default_for_tests(),
                inline_options: MdInlinesWriterOptions {
                    link_format: LinkTransform::default_for_tests(),
                    renumber_footnotes: false,
                },
                include_thematic_breaks: true,
            }
        }

        pub fn new_with<F>(init: F) -> Self
        where
            F: FnOnce(&mut MdOptions),
        {
            let mut mdo = Self::default_for_tests();
            init(&mut mdo);
            mdo
        }
    }

    pub fn get_only<T: Debug, C: IntoIterator<Item = T>>(col: C) -> T {
        let mut iter = col.into_iter();
        let Some(result) = iter.next() else {
            panic!("expected an element, but was empty");
        };
        match iter.next() {
            None => result,
            Some(extra) => {
                let mut all = Vec::new();
                all.push(result);
                all.push(extra);
                all.extend(iter);
                panic!("expected exactly one element, but found {}: {all:?}", all.len());
            }
        }
    }

    /// Turn a pattern match into an `if let ... { else panic! }`.
    macro_rules! unwrap {
        ($enum_value:expr, $enum_variant:pat) => {
            let node = $enum_value;
            let node_debug = format!("{:?}", node);
            let $enum_variant = node else {
                panic!("Expected {} but saw {}", stringify!($enum_variant), node_debug);
            };
        };
    }
    pub(crate) use unwrap;

    macro_rules! test_delay_ms {
        ($i:literal) => {{
            time::Duration::from_millis(
                $i * option_env!("TEST_TIMEOUT_MULTIPLIER")
                    .map(|s| s.parse::<u64>().expect("bad value for TEST_TIMEOUT_MULTIPLIER"))
                    .unwrap_or(1),
            )
        }};
    }
    pub(crate) use test_delay_ms;

    /// Converts an `MdElem` into an `MdElemRef`, checking that it got converted to the right one
    macro_rules! checked_elem_ref {
        ($input:expr => $variant:pat) => {{
            let as_ref: crate::md_elem::MdElemRef = (&($input)).into();
            if !matches!(as_ref, $variant) {
                panic!(
                    "{} should have been {}, was {:?}",
                    stringify!($input),
                    stringify!($variant),
                    as_ref
                );
            }
            as_ref
        }};
    }
    pub(crate) use checked_elem_ref;

    /// Creates a static object named `$name` that looks for all the variants of enum `E`.
    ///
    /// ```
    /// use mdq::variants_checker;
    ///
    /// enum MyEnum {
    ///   Variant1,
    ///   Variant2(usize)
    /// }
    /// variants_checker!(CHECKER_NAME = MyEnum { Variant1, Variant2(_) });
    /// ```
    ///
    /// You can also mark some variants as ignored; these will be added to the pattern match, but not be required to
    /// be seen:
    ///
    /// ```
    /// use mdq::variants_checker;
    ///
    /// enum MyEnum {
    ///   Variant1,
    ///   Variant2(usize)
    /// }
    /// variants_checker!(CHECKER_NAME = MyEnum { Variant1 } ignore { Variant2(_) });
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

                        let timeout = test_delay_ms!(500);
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
    pub(crate) use variants_checker;
}
