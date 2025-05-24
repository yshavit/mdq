use crate::md_elem::elem::*;
use crate::select::match_selector::MatchSelector;
use crate::select::string_matcher::{StringMatchError, StringMatcher};
use crate::select::CodeBlockMatcher;

#[derive(Debug, PartialEq)]
pub struct CodeBlockSelector {
    lang_matcher: StringMatcher,
    contents_matcher: StringMatcher,
}

impl From<CodeBlockMatcher> for CodeBlockSelector {
    fn from(value: CodeBlockMatcher) -> Self {
        Self {
            lang_matcher: value.language.into(),
            contents_matcher: value.contents.into(),
        }
    }
}

impl MatchSelector<CodeBlock> for CodeBlockSelector {
    fn matches(&self, code_block: &CodeBlock) -> Result<bool, StringMatchError> {
        let lang_matches = match &code_block.variant {
            CodeVariant::Code(code_opts) => {
                let actual_lang = match code_opts {
                    Some(co) => &co.language,
                    None => "",
                };
                self.lang_matcher.matches(actual_lang)?
            }
            CodeVariant::Math { .. } => false,
        };
        Ok(lang_matches && self.contents_matcher.matches(&code_block.value)?)
    }

    fn name() -> &'static str {
        "code block"
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::md_elem::MdContext;
    use crate::select::{MatchReplace, Matcher, TrySelector};

    #[test]
    fn code_block_selector_match_error() {
        let code_block_matcher = CodeBlockMatcher {
            language: MatchReplace {
                matcher: Matcher::Text {
                    case_sensitive: false,
                    anchor_start: false,
                    text: "rust".to_string(),
                    anchor_end: false,
                },
                replacement: Some("replacement".to_string()),
            },
            contents: MatchReplace {
                matcher: Matcher::Any { explicit: false },
                replacement: None,
            },
        };

        let code_block = CodeBlock {
            variant: CodeVariant::Code(Some(CodeOpts {
                language: "rust".to_string(),
                metadata: None,
            })),
            value: "fn main() {}".to_string(),
        };

        let code_block_selector = CodeBlockSelector::from(code_block_matcher);

        assert_eq!(
            code_block_selector.matches(&code_block),
            Err(StringMatchError::NotSupported)
        );

        assert_eq!(
            code_block_selector
                .try_select(&MdContext::default(), code_block)
                .unwrap_err()
                .to_string(),
            "code block selector does not support string replace"
        );
    }
}
