use regex::Regex;

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AnyVariant {
    Implicit,
    Explicit,
}

impl PartialEq for Matcher {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Self::Text {
                    case_sensitive: s1,
                    anchor_start: a1,
                    text: t1,
                    anchor_end: e1,
                },
                Self::Text {
                    case_sensitive: s2,
                    anchor_start: a2,
                    text: t2,
                    anchor_end: e2,
                },
            ) => s1 == s2 && a1 == a2 && e1 == e2 && t1 == t2,
            (Self::Regex(r1), Self::Regex(r2)) => r1.as_str() == r2.as_str(),
            (Self::Any(_), Self::Any(_)) => true,
            _ => false,
        }
    }
}

impl Eq for Matcher {}
