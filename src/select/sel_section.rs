use crate::md_elem::elem::*;
use crate::md_elem::MdContext;
use crate::select::match_selector::make_select_result;
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
                    title: replacements.item,
                    depth: item.depth,
                    body: item.body,
                };
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
}
