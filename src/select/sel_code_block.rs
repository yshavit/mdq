use crate::matcher::{CharEnd, StringMatcher};
use crate::parsing_iter::ParsingIterator;
use crate::select::{ParseResult, Selector, SELECTOR_SEPARATOR};
use crate::tree::{CodeBlock, CodeVariant};

#[derive(Debug, PartialEq)]
pub struct CodeBlockSelector {
    lang_matcher: StringMatcher,
    contents_matcher: StringMatcher,
}

impl CodeBlockSelector {
    pub fn read(iter: &mut ParsingIterator) -> ParseResult<Self> {
        iter.require_str("``")?; // first ` is from dispatcher

        let lang_matcher = match iter.peek() {
            Some(ch) if !ch.is_whitespace() => StringMatcher::read(iter, CharEnd::AtWhitespace)?,
            _ => StringMatcher::any(),
        };
        iter.require_whitespace_or(SELECTOR_SEPARATOR, "```")?;
        let contents_matcher = StringMatcher::read(iter, SELECTOR_SEPARATOR)?;
        Ok(Self {
            lang_matcher,
            contents_matcher,
        })
    }
}

impl<'a> Selector<'a, &'a CodeBlock> for CodeBlockSelector {
    fn matches(&self, code_block: &'a CodeBlock) -> bool {
        let lang_matches = match &code_block.variant {
            CodeVariant::Code(code_opts) => {
                let actual_lang = match code_opts {
                    Some(co) => &co.language,
                    None => "",
                };
                self.lang_matcher.matches(actual_lang)
            }
            CodeVariant::Math { .. } | CodeVariant::Toml | CodeVariant::Yaml => false,
        };
        lang_matches && self.contents_matcher.matches(&code_block.value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod parsing {
        use super::*;
        use crate::matcher::StringMatcher;
        use crate::parsing_iter::ParsingIterator;

        #[test]
        fn only_backticks() {
            let input_str = "``";
            let actual = CodeBlockSelector::read(&mut ParsingIterator::new(input_str)).unwrap();
            assert_eq!(
                actual,
                CodeBlockSelector {
                    lang_matcher: StringMatcher::any(),
                    contents_matcher: StringMatcher::any(),
                },
            )
        }

        #[test]
        fn only_language() {
            let input_str = "``rust";
            let actual = CodeBlockSelector::read(&mut ParsingIterator::new(input_str)).unwrap();
            assert_eq!(
                actual,
                CodeBlockSelector {
                    lang_matcher: StringMatcher::from("(?i)rust"),
                    contents_matcher: StringMatcher::any(),
                },
            )
        }

        #[test]
        fn only_contents() {
            let input_str = "`` foo";
            let actual = CodeBlockSelector::read(&mut ParsingIterator::new(input_str)).unwrap();
            assert_eq!(
                actual,
                CodeBlockSelector {
                    lang_matcher: StringMatcher::any(),
                    contents_matcher: StringMatcher::from("(?i)foo"),
                },
            )
        }

        #[test]
        fn both() {
            let input_str = "``rust fizz";
            let actual = CodeBlockSelector::read(&mut ParsingIterator::new(input_str)).unwrap();
            assert_eq!(
                actual,
                CodeBlockSelector {
                    lang_matcher: StringMatcher::from("(?i)rust"),
                    contents_matcher: StringMatcher::from("(?i)fizz"),
                },
            )
        }
    }
}
