#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Matcher {
    Text {
        case_sensitive: bool,
        anchor_start: bool,
        text: String,
        anchor_end: bool,
    },
    Regex(Regex),
    Any(AnyVariant),
}

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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AnyVariant {
    Implicit,
    Explicit,
}
