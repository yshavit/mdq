use std::cmp::Ordering;
use std::hash::{Hash, Hasher};

/// A type for matching against expected strings.
///
/// Given a selector like `# hello world` (for a section selector), this defines the `hello world` portion.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Matcher {
    /// Quoted or unquoted text, with optional anchoring.
    Text {
        case_sensitive: bool,
        anchor_start: bool,
        text: String,
        anchor_end: bool,
    },
    /// A regex match. This will match any substring by default, though you can use `^` and `$` anchors.
    ///
    /// The actual regex library is intentionally obscured so that it can change in the future without breaking the API.
    Regex(Regex),

    /// Any string. This can be implicit (an empty matcher in a query string, like `# | ...`), or explicit
    /// (a `*` in a query string, like `# * | ...`).
    Any { explicit: bool },
}

/// An opaque wrapper around a regular expression.
///
/// The actual regex library is intentionally obscured so that it can change in the future without breaking the API.
#[derive(Debug, Clone)]
pub struct Regex {
    pub(crate) re: fancy_regex::Regex,
}

impl PartialEq for Regex {
    fn eq(&self, other: &Self) -> bool {
        self.re.as_str() == other.re.as_str()
    }
}

impl Eq for Regex {}

impl PartialOrd for Regex {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Regex {
    fn cmp(&self, other: &Self) -> Ordering {
        Ord::cmp(self.re.as_str(), other.re.as_str())
    }
}

impl Hash for Regex {
    fn hash<H: Hasher>(&self, state: &mut H) {
        Hash::hash(self.re.as_str(), state);
    }
}
