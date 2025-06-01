use crate::md_elem::elem::*;
use crate::md_elem::MdContext;
use crate::select::string_matcher::{StringMatchError, StringMatcher};
use crate::select::{LinklikeMatcher, Result, Select, TrySelector};

/// A macro for creating the result of a link-like selector.
///
/// This macro handles the common pattern of conditionally reconstructing a link-like item
/// (Link or Image) based on whether both text and URL matching succeed. It
/// handles the URL replacement logic, calling either `do_replace()` or `no_replace()` on
/// the URL match result depending on whether this should be a Hit or Miss.
///
/// # Parameters
///
/// * `$text_matched` - A boolean expression indicating whether the display / alt text matched
/// * `$url_match` - An identifier bound to a `StringMatch` result from URL matching
/// * `$item_expr` - A block containing the item construction expression, where `$url_match`
///   will be bound to the appropriate URL string value
///
/// # Returns
///
/// Returns `Result<Select>` where:
/// - `Ok(Select::Hit(...))` if both text and URL matched (uses `do_replace()`)
/// - `Ok(Select::Miss(...))` if either text or URL didn't match (uses `no_replace()`)
macro_rules! make_result {
    ($text_matched:expr, $url_match:ident, { $($item_expr:tt)* }) => {
        if $text_matched && $url_match.is_match() {
            let $url_match = $url_match.do_replace();
            let item = $($item_expr)*;
            Ok(Select::Hit(vec![item.into()]))
        } else {
            let $url_match = $url_match.no_replace();
            let item = $($item_expr)*;
            Ok(Select::Miss(item.into()))
        }
    };
}

#[derive(Debug, PartialEq)]
pub(crate) struct LinkSelector {
    matchers: LinkMatchers,
}

impl From<LinklikeMatcher> for LinkSelector {
    fn from(value: LinklikeMatcher) -> Self {
        Self { matchers: value.into() }
    }
}

impl TrySelector<Link> for LinkSelector {
    fn try_select(&self, _: &MdContext, item: Link) -> Result<Select> {
        // Check if display matcher has replacement - if so, this should fail
        if self.matchers.display_matcher.has_replacement() {
            return Err(StringMatchError::NotSupported.to_select_error("hyperlink"));
        }

        let display_matched = self
            .matchers
            .display_matcher
            .matches_inlines(&item.display)
            .map_err(|e| e.to_select_error("hyperlink"))?;
        let url_match = self
            .matchers
            .url_matcher
            .match_replace(item.link.url)
            .map_err(|e| e.to_select_error("hyperlink"))?;

        make_result!(display_matched, url_match, {
            Link {
                display: item.display,
                link: LinkDefinition {
                    url: url_match,
                    title: item.link.title,
                    reference: item.link.reference,
                },
            }
        })
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct ImageSelector {
    matchers: LinkMatchers,
}

impl From<LinklikeMatcher> for ImageSelector {
    fn from(value: LinklikeMatcher) -> Self {
        Self { matchers: value.into() }
    }
}

impl TrySelector<Image> for ImageSelector {
    fn try_select(&self, _: &MdContext, item: Image) -> Result<Select> {
        // Check if display matcher has replacement - if so, this should fail
        if self.matchers.display_matcher.has_replacement() {
            return Err(StringMatchError::NotSupported.to_select_error("image"));
        }

        let alt_matched = self
            .matchers
            .display_matcher
            .matches(&item.alt)
            .map_err(|e| e.to_select_error("image"))?;
        let url_match = self
            .matchers
            .url_matcher
            .match_replace(item.link.url)
            .map_err(|e| e.to_select_error("image"))?;

        make_result!(alt_matched, url_match, {
            Image {
                alt: item.alt,
                link: LinkDefinition {
                    url: url_match,
                    title: item.link.title,
                    reference: item.link.reference,
                },
            }
        })
    }
}

#[derive(Debug, PartialEq)]
pub(crate) struct LinkMatchers {
    pub(crate) display_matcher: StringMatcher,
    pub(crate) url_matcher: StringMatcher,
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
    use crate::md_elem::{mdq_inline, MdElem};
    use crate::select::{MatchReplace, Matcher};
    use crate::util::utils_for_test::unwrap;

    #[test]
    fn link_selector_url_replacement() {
        let link_matcher = LinklikeMatcher {
            display_matcher: MatchReplace {
                matcher: Matcher::Any { explicit: false },
                replacement: None,
            },
            url_matcher: MatchReplace {
                matcher: Matcher::Text {
                    case_sensitive: false,
                    anchor_start: false,
                    text: "original.com".to_string(),
                    anchor_end: false,
                },
                replacement: Some("newsite.com".to_string()),
            },
        };

        let link = Link {
            display: vec![],
            link: LinkDefinition {
                url: "https://original.com/path".to_string(),
                title: None,
                reference: LinkReference::Inline,
            },
        };

        let link_selector = LinkSelector::from(link_matcher);

        let result = link_selector.try_select(&MdContext::default(), link);
        unwrap!(result, Ok(Select::Hit(elems)));

        assert_eq!(elems.len(), 1);
        unwrap!(&elems[0], MdElem::Inline(Inline::Link(modified_link)));
        assert_eq!(modified_link.link.url, "https://newsite.com/path");
    }

    #[test]
    fn image_selector_url_replacement() {
        let image_matcher = LinklikeMatcher {
            display_matcher: MatchReplace {
                matcher: Matcher::Any { explicit: false },
                replacement: None,
            },
            url_matcher: MatchReplace {
                matcher: Matcher::Text {
                    case_sensitive: false,
                    anchor_start: false,
                    text: "old-image.png".to_string(),
                    anchor_end: false,
                },
                replacement: Some("new-image.png".to_string()),
            },
        };

        let image = Image {
            alt: "alt text".to_string(),
            link: LinkDefinition {
                url: "https://example.com/old-image.png".to_string(),
                title: None,
                reference: LinkReference::Inline,
            },
        };

        let image_selector = ImageSelector::from(image_matcher);

        let result = image_selector.try_select(&MdContext::default(), image);
        unwrap!(result, Ok(Select::Hit(elems)));
        assert_eq!(elems.len(), 1);
        unwrap!(&elems[0], MdElem::Inline(Inline::Image(modified_image)));
        assert_eq!(modified_image.link.url, "https://example.com/new-image.png");
    }

    #[test]
    fn image_url_replaced_but_alt_does_not_match() {
        let image_matcher = LinklikeMatcher {
            display_matcher: MatchReplace {
                matcher: Matcher::Text {
                    case_sensitive: false,
                    anchor_start: true,
                    text: "wrong alt text".to_string(),
                    anchor_end: true,
                },
                replacement: None,
            },
            url_matcher: MatchReplace {
                matcher: Matcher::Text {
                    case_sensitive: false,
                    anchor_start: false,
                    text: "old-image.png".to_string(),
                    anchor_end: false,
                },
                replacement: Some("new-image.png".to_string()),
            },
        };

        let original_image = Image {
            alt: "original alt text".to_string(),
            link: LinkDefinition {
                url: "https://example.com/old-image.png".to_string(),
                title: None,
                reference: LinkReference::Inline,
            },
        };

        let image_selector = ImageSelector::from(image_matcher);

        let result = image_selector.try_select(&MdContext::default(), original_image.clone());
        unwrap!(result, Ok(Select::Miss(elem)));
        unwrap!(&elem, MdElem::Inline(Inline::Image(result_image)));

        assert_eq!(result_image, &original_image);
    }

    #[test]
    fn link_url_replaced_but_display_does_not_match() {
        let link_matcher = LinklikeMatcher {
            display_matcher: MatchReplace {
                matcher: Matcher::Text {
                    case_sensitive: false,
                    anchor_start: true,
                    text: "wrong display text".to_string(),
                    anchor_end: true,
                },
                replacement: None,
            },
            url_matcher: MatchReplace {
                matcher: Matcher::Text {
                    case_sensitive: false,
                    anchor_start: false,
                    text: "original.com".to_string(),
                    anchor_end: false,
                },
                replacement: Some("newsite.com".to_string()),
            },
        };

        let original_link = Link {
            display: vec![mdq_inline!("original display text")],
            link: LinkDefinition {
                url: "https://original.com/path".to_string(),
                title: None,
                reference: LinkReference::Inline,
            },
        };

        let link_selector = LinkSelector::from(link_matcher);

        let result = link_selector.try_select(&MdContext::default(), original_link.clone());
        unwrap!(result, Ok(Select::Miss(elem)));
        unwrap!(&elem, MdElem::Inline(Inline::Link(result_link)));

        assert_eq!(result_link, &original_link);
    }
}
