use crate::fmt_str::inlines_to_plain_string;
use crate::matcher::Matcher;
use crate::parsing_iter::ParsingIterator;
use crate::selectors::base::{ParseResult, SelectResult, Selector};
use crate::selectors::util::require_whitespace;
use crate::tree::Section;

#[derive(Debug, PartialEq)]
pub struct SectionSelector {
    matcher: Matcher,
}

impl SectionSelector {
    pub fn read<C: Iterator<Item = char>>(chars: &mut ParsingIterator<C>) -> ParseResult<SectionSelector> {
        require_whitespace(chars, "Section specifier")?;

        let matcher = Matcher::parse_matcher(chars)?;
        Ok(Self { matcher })
    }
}

impl<'a> Selector<'a, &'a Section> for SectionSelector {
    fn matches(&self, section: &'a Section) -> bool {
        let header_text = inlines_to_plain_string(&section.title);
        self.matcher.matches(&header_text)
    }

    fn pick(&self, item: &'a Section) -> SelectResult<'a> {
        SelectResult::Multi(&item.body)
    }
}
#[cfg(test)]
mod test {
    use super::*;
    use crate::parse_common::parse_and_check;

    #[test]
    fn section() {
        parse_and_check(
            "# foo",
            Selector::Section(SectionSelector {
                matcher: Matcher::Substring(SubstringMatcher {
                    look_for: "foo".to_string(),
                }),
            }),
            "",
        );

        parse_and_check("# ", Selector::Section(SectionSelector { matcher: Matcher::Any }), "");

        parse_and_check(
            "# | next",
            Selector::Section(SectionSelector { matcher: Matcher::Any }),
            " next",
        );
    }
}
