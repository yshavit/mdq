use crate::md_elem::elem::*;
use crate::select::match_selector::MatchSelector;
use crate::select::string_matcher::{StringMatchError, StringMatcher};
use crate::select::CodeBlockMatcher;

#[derive(Debug, PartialEq)]
pub(crate) struct CodeBlockSelector {
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
    const NAME: &'static str = "code block";

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
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::md_elem::{MdContext, MdElem};
    use crate::select::{MatchReplace, Select, TrySelector};

    #[test]
    fn both_match() {
        let code_block = new_code_block("main()", Some("rust"));

        let result = CodeBlockSelector::from(CodeBlockMatcher {
            language: MatchReplace::match_any(),
            contents: MatchReplace::match_any(),
        })
        .try_select(&MdContext::default(), code_block.clone())
        .unwrap();

        assert_eq!(result, Select::Hit(vec![MdElem::CodeBlock(code_block)]));
    }

    #[test]
    fn language_match_hits() {
        let code_block = new_code_block("main()", Some("rust"));

        let result = CodeBlockSelector::from(CodeBlockMatcher {
            language: MatchReplace::build(|b| b.match_regex("rust")),
            contents: MatchReplace::match_any(),
        })
        .try_select(&MdContext::default(), code_block.clone())
        .unwrap();

        assert_eq!(result, Select::Hit(vec![MdElem::CodeBlock(code_block)]));
    }

    #[test]
    fn language_match_misses() {
        let code_block = new_code_block("main", Some("rust"));

        let result = CodeBlockSelector::from(CodeBlockMatcher {
            language: MatchReplace::build(|b| b.match_regex("python")),
            contents: MatchReplace::match_any(),
        })
        .try_select(&MdContext::default(), code_block.clone())
        .unwrap();

        assert_eq!(result, Select::Miss(MdElem::CodeBlock(code_block)));
    }

    #[test]
    fn contents_match_hits() {
        let code_block = new_code_block("main()", Some("rust"));

        let result = CodeBlockSelector::from(CodeBlockMatcher {
            language: MatchReplace::match_any(),
            contents: MatchReplace::build(|b| b.match_regex("main")),
        })
        .try_select(&MdContext::default(), code_block.clone())
        .unwrap();

        assert_eq!(result, Select::Hit(vec![MdElem::CodeBlock(code_block)]));
    }

    #[test]
    fn contents_match_misses() {
        let code_block = new_code_block("main()", Some("rust"));

        let result = CodeBlockSelector::from(CodeBlockMatcher {
            language: MatchReplace::match_any(),
            contents: MatchReplace::build(|b| b.match_regex("println")),
        })
        .try_select(&MdContext::default(), code_block.clone())
        .unwrap();

        assert_eq!(result, Select::Miss(MdElem::CodeBlock(code_block)));
    }

    #[test]
    fn language_replacement_with_match() {
        let code_block = new_code_block("def main", Some("python"));

        let result = CodeBlockSelector::from(CodeBlockMatcher {
            language: MatchReplace::build(|b| b.match_regex("python").replacement("py")),
            contents: MatchReplace::match_any(),
        })
        .try_select(&MdContext::default(), code_block)
        .unwrap();

        assert_eq!(
            result,
            Select::Miss(MdElem::CodeBlock(new_code_block("def main", Some("py"))))
        );
    }

    #[test]
    fn language_replacement_with_miss() {
        let code_block = new_code_block("main()", Some("rust"));

        let result = CodeBlockSelector::from(CodeBlockMatcher {
            language: MatchReplace::build(|b| b.match_regex("python").replacement("py")),
            contents: MatchReplace::match_any(),
        })
        .try_select(&MdContext::default(), code_block.clone())
        .unwrap();

        assert_eq!(result, Select::Miss(MdElem::CodeBlock(code_block)));
    }

    #[test]
    fn contents_replacement_with_match() {
        let code_block = new_code_block("main()", Some("rust"));

        let result = CodeBlockSelector::from(CodeBlockMatcher {
            language: MatchReplace::match_any(),
            contents: MatchReplace::build(|b| b.match_regex("main").replacement("start")),
        })
        .try_select(&MdContext::default(), code_block)
        .unwrap();

        assert_eq!(
            result,
            Select::Miss(MdElem::CodeBlock(new_code_block("start()", Some("rust"))))
        );
    }

    #[test]
    fn contents_replacement_with_miss() {
        let code_block = new_code_block("main()", Some("rust"));

        let result = CodeBlockSelector::from(CodeBlockMatcher {
            language: MatchReplace::match_any(),
            contents: MatchReplace::build(|b| b.match_regex("println").replacement("print")),
        })
        .try_select(&MdContext::default(), code_block.clone())
        .unwrap();

        assert_eq!(result, Select::Miss(MdElem::CodeBlock(code_block)));
    }

    #[test]
    fn both_replacement_with_match() {
        let code_block = new_code_block("def main:", Some("python"));

        let result = CodeBlockSelector::from(CodeBlockMatcher {
            language: MatchReplace::build(|b| b.match_regex("python").replacement("py")),
            contents: MatchReplace::build(|b| b.match_regex("def (\\w+):").replacement("$1()")),
        })
        .try_select(&MdContext::default(), code_block)
        .unwrap();

        assert_eq!(
            result,
            Select::Miss(MdElem::CodeBlock(new_code_block("main()", Some("py"))))
        );
    }

    #[test]
    fn no_language_code_but_match_includes_one() {
        let code_block = new_code_block("main()", None);

        let result = CodeBlockSelector::from(CodeBlockMatcher {
            language: MatchReplace::build(|b| b.match_regex("rust")),
            contents: MatchReplace::match_any(),
        })
        .try_select(&MdContext::default(), code_block.clone())
        .unwrap();

        assert_eq!(result, Select::Miss(MdElem::CodeBlock(code_block)));
    }

    #[test]
    fn no_language_code_and_match_doesnt_include_one() {
        let code_block = new_code_block("main()", None);

        let result = CodeBlockSelector::from(CodeBlockMatcher {
            language: MatchReplace::match_any(),
            contents: MatchReplace::match_any(),
        })
        .try_select(&MdContext::default(), code_block.clone())
        .unwrap();

        assert_eq!(result, Select::Hit(vec![MdElem::CodeBlock(code_block)]));
    }

    #[test]
    fn math_block_never_matches() {
        let math_block = CodeBlock {
            variant: CodeVariant::Math {
                metadata: Some("math".to_string()),
            },
            value: "x = 1".to_string(),
        };

        let result = CodeBlockSelector::from(CodeBlockMatcher {
            language: MatchReplace::match_any(),
            contents: MatchReplace::match_any(),
        })
        .try_select(&MdContext::default(), math_block.clone())
        .unwrap();

        assert_eq!(result, Select::Miss(MdElem::CodeBlock(math_block)));
    }

    fn new_code_block(content: &str, language: Option<&str>) -> CodeBlock {
        CodeBlock {
            variant: CodeVariant::Code(language.map(|lang| CodeOpts {
                language: lang.to_string(),
                metadata: None,
            })),
            value: content.to_string(),
        }
    }
}
