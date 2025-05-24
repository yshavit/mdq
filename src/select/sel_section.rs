use crate::md_elem::elem::*;
use crate::select::match_selector::MatchSelector;
use crate::select::string_matcher::{StringMatchError, StringMatcher};
use crate::select::SectionMatcher;

#[derive(Debug, PartialEq)]
pub struct SectionSelector {
    matcher: StringMatcher,
}

impl From<SectionMatcher> for SectionSelector {
    fn from(value: SectionMatcher) -> Self {
        Self {
            matcher: value.title.into(),
        }
    }
}

impl MatchSelector<Section> for SectionSelector {
    fn matches(&self, section: &Section) -> Result<bool, StringMatchError> {
        self.matcher.matches_inlines(&section.title)
    }

    fn name() -> &'static str {
        "section"
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::select::{MatchReplace, Matcher, SelectError};

    #[test]
    fn section_selector_match_error() {
        use crate::md_elem::MdContext;
        use crate::select::TrySelector;

        let section_matcher = SectionMatcher {
            title: MatchReplace {
                matcher: Matcher::Text {
                    case_sensitive: false,
                    anchor_start: false,
                    text: "test".to_string(),
                    anchor_end: false,
                },
                replacement: Some("replacement".to_string()),
            },
        };

        let section = Section {
            depth: 1,
            title: vec![],
            body: vec![],
        };

        let section_selector = SectionSelector::from(section_matcher);

        assert_eq!(section_selector.matches(&section), Err(StringMatchError::NotSupported));

        assert_eq!(
            section_selector.try_select(&MdContext::default(), section).unwrap_err(),
            SelectError::new("section selector does not support string replace"),
        );
    }
}
