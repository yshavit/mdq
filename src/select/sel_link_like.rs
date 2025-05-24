use crate::md_elem::elem::*;
use crate::select::match_selector::MatchSelector;
use crate::select::string_matcher::{StringMatchError, StringMatcher};
use crate::select::LinklikeMatcher;

#[derive(Debug, PartialEq)]
pub struct LinkSelector {
    matchers: LinkMatchers,
}

impl From<LinklikeMatcher> for LinkSelector {
    fn from(value: LinklikeMatcher) -> Self {
        Self { matchers: value.into() }
    }
}

impl MatchSelector<Link> for LinkSelector {
    const NAME: &'static str = "hyperlink";

    fn matches(&self, item: &Link) -> Result<bool, StringMatchError> {
        Ok(self.matchers.display_matcher.matches_inlines(&item.display)?
            && self.matchers.url_matcher.matches(&item.link.url)?)
    }
}

#[derive(Debug, PartialEq)]
pub struct ImageSelector {
    matchers: LinkMatchers,
}

impl From<LinklikeMatcher> for ImageSelector {
    fn from(value: LinklikeMatcher) -> Self {
        Self { matchers: value.into() }
    }
}

impl MatchSelector<Image> for ImageSelector {
    const NAME: &'static str = "image";

    fn matches(&self, item: &Image) -> Result<bool, StringMatchError> {
        Ok(self.matchers.display_matcher.matches(&item.alt)? && self.matchers.url_matcher.matches(&item.link.url)?)
    }
}

#[derive(Debug, PartialEq)]
pub struct LinkMatchers {
    pub display_matcher: StringMatcher,
    pub url_matcher: StringMatcher,
}

impl From<LinklikeMatcher> for LinkMatchers {
    fn from(value: LinklikeMatcher) -> Self {
        Self {
            display_matcher: value.display_matcher.into(),
            url_matcher: value.url_matcher.into(),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::md_elem::MdContext;
    use crate::select::{MatchReplace, Matcher, TrySelector};

    #[test]
    fn link_selector_match_error() {
        let link_matcher = LinklikeMatcher {
            display_matcher: MatchReplace {
                matcher: Matcher::Text {
                    case_sensitive: false,
                    anchor_start: false,
                    text: "test".to_string(),
                    anchor_end: false,
                },
                replacement: Some("replacement".to_string()),
            },
            url_matcher: MatchReplace {
                matcher: Matcher::Any { explicit: false },
                replacement: None,
            },
        };

        let link = Link {
            display: vec![],
            link: LinkDefinition {
                url: "https://example.com".to_string(),
                title: None,
                reference: LinkReference::Inline,
            },
        };

        let link_selector = LinkSelector::from(link_matcher);

        assert_eq!(link_selector.matches(&link), Err(StringMatchError::NotSupported));

        assert_eq!(
            link_selector
                .try_select(&MdContext::default(), link)
                .unwrap_err()
                .to_string(),
            "link selector does not support string replace"
        );
    }

    #[test]
    fn image_selector_match_error() {
        let image_matcher = LinklikeMatcher {
            display_matcher: MatchReplace {
                matcher: Matcher::Text {
                    case_sensitive: false,
                    anchor_start: false,
                    text: "alt text".to_string(),
                    anchor_end: false,
                },
                replacement: Some("replacement".to_string()),
            },
            url_matcher: MatchReplace {
                matcher: Matcher::Any { explicit: false },
                replacement: None,
            },
        };

        let image = Image {
            alt: "alt text".to_string(),
            link: LinkDefinition {
                url: "https://example.com/image.png".to_string(),
                title: None,
                reference: LinkReference::Inline,
            },
        };

        let image_selector = ImageSelector::from(image_matcher);

        // Test that matches() propagates the StringMatchError::NotSupported
        assert_eq!(image_selector.matches(&image), Err(StringMatchError::NotSupported));

        // Test that try_select() converts the error to SelectError with proper selector name
        assert_eq!(
            image_selector
                .try_select(&MdContext::default(), image)
                .unwrap_err()
                .to_string(),
            "image selector does not support string replace"
        );
    }
}
