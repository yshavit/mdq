use crate::md_elem::elem::*;
use crate::select::match_selector::MatchSelector;
use crate::select::string_matcher::StringMatcher;
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
    fn matches(&self, code_block: &CodeBlock) -> bool {
        let lang_matches = match &code_block.variant {
            CodeVariant::Code(code_opts) => {
                let actual_lang = match code_opts {
                    Some(co) => &co.language,
                    None => "",
                };
                self.lang_matcher.matches(actual_lang)
            }
            CodeVariant::Math { .. } => false,
        };
        lang_matches && self.contents_matcher.matches(&code_block.value)
    }
}
