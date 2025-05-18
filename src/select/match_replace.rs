use crate::select::Matcher;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct MatchReplace {
    pub matcher: Matcher,
    pub replacement: Option<String>,
}
