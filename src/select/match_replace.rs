use crate::select::Matcher;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MatchReplace {
    pub matcher: Matcher,
    pub replacement: Option<String>,
}

#[cfg(test)]
mod test_utils {
    use super::*;
    use crate::select::{Matcher, Regex};

    impl MatchReplace {
        pub(crate) fn match_any() -> MatchReplace {
            Self::build(|b| b)
        }

        pub(crate) fn build(f: impl FnOnce(&mut MatchReplaceBuilder) -> &mut MatchReplaceBuilder) -> MatchReplace {
            let mut builder = MatchReplaceBuilder {
                matcher: None,
                replacement: None,
            };
            f(&mut builder);
            MatchReplace {
                matcher: builder.matcher.unwrap_or(Matcher::Any { explicit: false }),
                replacement: builder.replacement,
            }
        }
    }

    pub(crate) struct MatchReplaceBuilder {
        matcher: Option<Matcher>,
        replacement: Option<String>,
    }

    impl MatchReplaceBuilder {
        pub(crate) fn match_regex(&mut self, pattern: impl AsRef<str>) -> &mut Self {
            let re = fancy_regex::Regex::new(pattern.as_ref()).expect("test error: bad regex");
            self.matcher = Some(Matcher::Regex(Regex { re }));
            self
        }

        pub(crate) fn replacement(&mut self, replacement: impl Into<String>) -> &mut Self {
            self.replacement = Some(replacement.into());
            self
        }
    }
}
