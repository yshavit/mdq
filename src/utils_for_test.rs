#[cfg(test)]
pub use test_utils::*;

// We this file's contents from prod by putting them in a submodule guarded by cfg(test), but then "pub use" it to
// export its contents.
#[cfg(test)]
mod test_utils {
    use std::{thread, time};

    // TODO unify this with the one in tree.rs.
    pub struct VariantsChecker<E> {
        require: std::sync::Arc<std::sync::Mutex<std::collections::HashSet<String>>>,
        resolver: fn(&E) -> &str,
    }

    impl<E> VariantsChecker<E> {
        pub fn new<I>(require: I, resolver: fn(&E) -> &str) -> Self
        where
            I: IntoIterator<Item = String>,
        {
            Self {
                require: std::sync::Arc::new(std::sync::Mutex::new(require.into_iter().collect())),
                resolver,
            }
        }

        pub fn see(&self, node: &E) {
            let node_str = (self.resolver)(node);
            self.require.lock().map(|mut set| set.remove(node_str)).unwrap();
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

    #[macro_export]
    macro_rules! new_variants_checker {
        {$enum_type:ty : $($variant:pat),* $(,)?} => {
            {
                use $enum_type::*;

                VariantsChecker::new(
                    vec![$(stringify!($variant).to_string(),)*],
                    {|elem| match elem {
                        $($variant => stringify!($variant),)*
                    }}
                )
            }
        };
    }
}
