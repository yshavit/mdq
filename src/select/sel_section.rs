use crate::md_elem::elem::*;
use crate::md_elem::MdContext;
use crate::select::string_matcher::StringMatcher;
use crate::select::{SectionMatcher, Select, TrySelector};

#[derive(Debug, PartialEq)]
pub(crate) struct SectionSelector {
    matcher: StringMatcher,
}

impl From<SectionMatcher> for SectionSelector {
    fn from(value: SectionMatcher) -> Self {
        Self {
            matcher: value.title.into(),
        }
    }
}

impl TrySelector<Section> for SectionSelector {
    fn try_select(&self, _: &MdContext, item: Section) -> crate::select::Result<Select> {
        match self.matcher.match_replace_inlines(item.title) {
            Ok(replacements) => {
                let result = Section {
                    title: replacements.inlines,
                    depth: item.depth,
                    body: item.body,
                };
                let result = if replacements.matched_any {
                    Select::Hit(vec![result.into()])
                } else {
                    Select::Miss(result.into())
                };
                Ok(result)
            }
            Err(err) => Err(err.to_select_error("section")),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::md_elem::{inlines, MdElem};
    use crate::select::{MatchReplace, Matcher, SelectError};

    #[test]
    fn section_selector_matches_on_title() {
        use crate::md_elem::MdContext;
        use crate::select::TrySelector;

        let section_matcher = SectionMatcher {
            title: MatchReplace {
                matcher: Matcher::Text {
                    case_sensitive: false,
                    anchor_start: false,
                    text: "Some".to_string(),
                    anchor_end: false,
                },
                replacement: Some("Great".to_string()),
            },
        };

        let section = Section {
            depth: 1,
            title: inlines!["Some title, Some section!"],
            body: vec![],
        };

        let section_selector = SectionSelector::from(section_matcher);
        let replaced = section_selector.try_select(&MdContext::default(), section).unwrap();

        assert_eq!(
            replaced,
            Select::Hit(vec![MdElem::Section(Section {
                depth: 1,
                title: inlines!["Great title, Great section!"],
                body: vec![],
            })]),
        );
    }

    #[test]
    fn section_selector_misses_on_title() {
        use crate::md_elem::MdContext;
        use crate::select::TrySelector;

        let section_matcher = SectionMatcher {
            title: MatchReplace {
                matcher: Matcher::Text {
                    case_sensitive: false,
                    anchor_start: false,
                    text: "Unmatched".to_string(),
                    anchor_end: false,
                },
                replacement: Some("Great".to_string()),
            },
        };

        let section = Section {
            depth: 1,
            title: inlines!["Some title, Some section!"],
            body: vec![],
        };

        let section_selector = SectionSelector::from(section_matcher);
        let replaced = section_selector.try_select(&MdContext::default(), section).unwrap();

        assert_eq!(
            replaced,
            Select::Miss(MdElem::Section(Section {
                depth: 1,
                title: inlines!["Some title, Some section!"],
                body: vec![],
            })),
        );
    }

    #[test]
    fn section_selector_invalid_on_title() {
        use crate::md_elem::MdContext;
        use crate::select::TrySelector;

        let section_matcher = SectionMatcher {
            title: MatchReplace {
                matcher: Matcher::Text {
                    case_sensitive: false,
                    anchor_start: false,
                    text: "crosses boundary".to_string(),
                    anchor_end: false,
                },
                replacement: Some("Broken".to_string()),
            },
        };

        let section = Section {
            depth: 1,
            title: inlines!["crosses ", link["boundary"]("https://example.com")],
            body: vec![],
        };

        let section_selector = SectionSelector::from(section_matcher);
        let replaced = section_selector.try_select(&MdContext::default(), section);

        assert_eq!(
            replaced,
            Err(SelectError::new(
                "regex replacement error in section selector: replacement crosses atomic boundary"
            ))
        );
    }
}
