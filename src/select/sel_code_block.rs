use crate::md_elem::elem::*;
use crate::md_elem::MdContext;
use crate::select::match_selector::make_select_result;
use crate::select::string_matcher::StringMatcher;
use crate::select::{CodeBlockMatcher, Select, TrySelector};

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

impl TrySelector<CodeBlock> for CodeBlockSelector {
    fn try_select(&self, _: &MdContext, item: CodeBlock) -> crate::select::Result<Select> {
        let (has_code_opts, language, metadata) = match item.variant {
            CodeVariant::Code(None) => (false, String::new(), None),
            CodeVariant::Code(Some(CodeOpts { language, metadata })) => (true, language, metadata),
            CodeVariant::Math { .. } => {
                return Ok(Select::Miss(item.into()));
            }
        };
        let lang_sel = self
            .lang_matcher
            .match_replace_string(language)
            .map_err(|e| e.to_select_error("code block"))?;
        let content_sel = self
            .contents_matcher
            .match_replace_string(item.value)
            .map_err(|e| e.to_select_error("code block"))?;

        let item_is_match = lang_sel.matched_any && content_sel.matched_any;

        let selected_code_opts = if has_code_opts {
            Some(CodeOpts {
                language: lang_sel.item,
                metadata,
            })
        } else {
            None
        };

        let selected_block = CodeBlock {
            variant: CodeVariant::Code(selected_code_opts),
            value: content_sel.item,
        };
        Ok(make_select_result(selected_block, item_is_match))
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
            Select::Hit(vec![MdElem::CodeBlock(new_code_block("def main", Some("py")))])
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
            Select::Hit(vec![MdElem::CodeBlock(new_code_block("start()", Some("rust")))])
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
            Select::Hit(vec![MdElem::CodeBlock(new_code_block("main()", Some("py")))])
        );
    }

    #[test]
    fn code_language_is_explicitly_empty() {
        let code_block = new_code_block("text", Some(""));

        let result = CodeBlockSelector::from(CodeBlockMatcher {
            language: MatchReplace::match_any(),
            contents: MatchReplace::match_any(),
        })
        .try_select(&MdContext::default(), code_block.clone())
        .unwrap();

        assert_eq!(result, Select::Hit(vec![MdElem::CodeBlock(code_block)]));
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
