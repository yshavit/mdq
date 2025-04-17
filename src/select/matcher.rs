/// A type for matching against expected strings.
///
/// Given a selector like `# hello world` (for a section selector), this defines the `hello world` portion.
#[derive(Debug, Clone, PartialEq, Eq)]
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
    pub(crate) re: regex::Regex,
}

impl PartialEq for Regex {
    fn eq(&self, other: &Self) -> bool {
        self.re.as_str() == other.re.as_str()
    }
}

impl Eq for Regex {}
