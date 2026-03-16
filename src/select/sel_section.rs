use crate::md_elem::elem::*;
use crate::md_elem::MdContext;
use crate::select::match_selector::make_select_result;
use crate::select::string_matcher::StringMatcher;
use crate::select::{SectionMatcher, Select, TrySelector};

#[derive(Debug, PartialEq)]
pub(crate) struct SectionSelector {
    matcher: StringMatcher,
    level_min: Option<u8>,
    level_max: Option<u8>,
}

impl From<SectionMatcher> for SectionSelector {
    fn from(value: SectionMatcher) -> Self {
        Self {
            matcher: value.title.into(),
            level_min: value.level_min,
            level_max: value.level_max,
        }
    }
}

impl TrySelector<Section> for SectionSelector {
    fn try_select(&self, _: &MdContext, item: Section) -> crate::select::Result<Select> {
        match self.matcher.match_replace_inlines(item.title) {
            Ok(replacements) => {
                let result = Section {
                    title: replacements.item,
                    depth: item.depth,
                    body: item.body,
                };
                if let Some(level_min) = self.level_min {
                    if item.depth < level_min {
                        return Ok(Select::Miss(result.into()));
                    }
                }
                if let Some(level_max) = self.level_max {
                    if item.depth > level_max {
                        return Ok(Select::Miss(result.into()));
                    }
                }
                Ok(make_select_result(result, replacements.matched_any))
            }
            Err(err) => Err(err.to_select_error("section")),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::md_elem::{inlines, MdElem};
    use crate::select::{MatchReplace, SelectError};

    #[test]
    fn section_replacement_matches_on_title() {
        let section_matcher = SectionMatcher {
            title: MatchReplace::build(|b| b.match_regex("Some").replacement("Great")),
            level_min: None,
            level_max: None,
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
    fn section_replacement_misses_on_title() {
        let section_matcher = SectionMatcher {
            title: MatchReplace::build(|b| b.match_regex("Unmatched").replacement("Great")),
            level_min: None,
            level_max: None,
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
    fn section_replacement_invalid_on_title() {
        let section_matcher = SectionMatcher {
            title: MatchReplace::build(|b| b.match_regex("crosses boundary").replacement("Broken")),
            level_min: None,
            level_max: None,
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

    #[test]
    fn section_regex_matches() {
        let section_matcher = SectionMatcher {
            title: MatchReplace::build(|b| b.match_regex("Great")),
            level_min: None,
            level_max: None,
        };

        let section = Section {
            depth: 1,
            title: inlines!["Great title, Great section!"],
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
    fn section_regex_doesnt_match() {
        let section_matcher = SectionMatcher {
            title: MatchReplace::build(|b| b.match_regex("Awesome")),
            level_min: None,
            level_max: None,
        };

        let section = Section {
            depth: 1,
            title: inlines!["Great title, Great section!"],
            body: vec![],
        };

        let section_selector = SectionSelector::from(section_matcher);
        let replaced = section_selector.try_select(&MdContext::default(), section).unwrap();

        assert_eq!(
            replaced,
            Select::Miss(MdElem::Section(Section {
                depth: 1,
                title: inlines!["Great title, Great section!"],
                body: vec![],
            })),
        );
    }

    #[test]
    fn section_with_min_matches() {
        // #{2,}
        let section_matcher = SectionMatcher {
            title: MatchReplace::build(|b| b),
            level_min: Some(2),
            level_max: None,
        };

        let section = Section {
            depth: 2,
            title: inlines!["my title"],
            body: vec![],
        };

        let section_selector = SectionSelector::from(section_matcher);
        let matched = section_selector.try_select(&MdContext::default(), section).unwrap();

        assert_eq!(
            matched,
            Select::Hit(vec![MdElem::Section(Section {
                depth: 2,
                title: inlines!["my title"],
                body: vec![],
            })]),
        );
    }

    #[test]
    fn section_with_min_misses() {
        // #{2,}
        let section_matcher = SectionMatcher {
            title: MatchReplace::build(|b| b),
            level_min: Some(2),
            level_max: None,
        };

        let section = Section {
            depth: 1,
            title: inlines!["my title"],
            body: vec![],
        };

        let section_selector = SectionSelector::from(section_matcher);
        let matched = section_selector.try_select(&MdContext::default(), section).unwrap();

        assert_eq!(
            matched,
            Select::Miss(MdElem::Section(Section {
                depth: 1,
                title: inlines!["my title"],
                body: vec![],
            })),
        );
    }

    #[test]
    fn section_with_max_matches() {
        // #{,2}
        let section_matcher = SectionMatcher {
            title: MatchReplace::build(|b| b),
            level_min: None,
            level_max: Some(2),
        };

        let section = Section {
            depth: 2,
            title: inlines!["my title"],
            body: vec![],
        };

        let section_selector = SectionSelector::from(section_matcher);
        let matched = section_selector.try_select(&MdContext::default(), section).unwrap();

        assert_eq!(
            matched,
            Select::Hit(vec![MdElem::Section(Section {
                depth: 2,
                title: inlines!["my title"],
                body: vec![],
            })]),
        );
    }

    #[test]
    fn section_with_max_misses() {
        // #{,2}
        let section_matcher = SectionMatcher {
            title: MatchReplace::build(|b| b),
            level_min: None,
            level_max: Some(2),
        };

        let section = Section {
            depth: 3,
            title: inlines!["my title"],
            body: vec![],
        };

        let section_selector = SectionSelector::from(section_matcher);
        let matched = section_selector.try_select(&MdContext::default(), section).unwrap();

        assert_eq!(
            matched,
            Select::Miss(MdElem::Section(Section {
                depth: 3,
                title: inlines!["my title"],
                body: vec![],
            })),
        );
    }

    #[test]
    fn section_exact_level_matches() {
        // #{2}
        let section_matcher = SectionMatcher {
            title: MatchReplace::build(|b| b),
            level_min: Some(2),
            level_max: Some(2),
        };

        let section = Section {
            depth: 2,
            title: inlines!["my title"],
            body: vec![],
        };

        let section_selector = SectionSelector::from(section_matcher);
        let matched = section_selector.try_select(&MdContext::default(), section).unwrap();

        assert_eq!(
            matched,
            Select::Hit(vec![MdElem::Section(Section {
                depth: 2,
                title: inlines!["my title"],
                body: vec![],
            })]),
        );
    }

    #[test]
    fn section_exact_level_misses() {
        // #{2}
        let section_matcher = SectionMatcher {
            title: MatchReplace::build(|b| b),
            level_min: Some(2),
            level_max: Some(2),
        };

        let section = Section {
            depth: 1,
            title: inlines!["my title"],
            body: vec![],
        };

        let section_selector = SectionSelector::from(section_matcher);
        let matched = section_selector.try_select(&MdContext::default(), section).unwrap();

        assert_eq!(
            matched,
            Select::Miss(MdElem::Section(Section {
                depth: 1,
                title: inlines!["my title"],
                body: vec![],
            })),
        );
    }
}
