use pest::Parser;
use pest_derive::Parser;
use std::fmt::Debug;

#[cfg(test)]
pub use crate::query::pest::test_helpers::StringVariant;

#[derive(Parser)]
#[grammar = "query/grammar.pest"]
struct QueryPairs;

pub struct Query {
    _private: (),
}

pub type Pair<'a> = pest::iterators::Pair<'a, Rule>;
pub type Pairs<'a> = pest::iterators::Pairs<'a, Rule>;
pub type Error = pest::error::Error<Rule>;

impl Query {
    pub fn parse(query_text: &str) -> Result<Pairs, Error> {
        QueryPairs::parse(Rule::top, query_text).map_err(Self::format_err)
    }

    fn format_err(err: Error) -> Error {
        err.renamed_rules(|err| {
            match err {
                Rule::EOI => "end of input",
                Rule::WHITESPACE => "whitespace",
                Rule::top => "valid query",
                Rule::selector_chain => "one or more selectors",
                Rule::selector => "selector",
                Rule::selector_delim | Rule::explicit_space => "space",
                Rule::select_section | Rule::section_start => "_#_",
                Rule::select_list_item | Rule::list_start => "_-_ or _1._",
                Rule::list_ordered => "_-_",
                Rule::list_task_options => "_[ ]_, _[x]_, or _[?]_",
                Rule::task_checked => "_[x]_",
                Rule::task_unchecked => "_[x]_",
                Rule::task_either => "_[?]_",
                Rule::task_end => "_]_",
                Rule::select_link | Rule::link_start => "_[_ or _![_",
                Rule::image_start => "_![_",
                Rule::select_block_quote | Rule::select_block_quote_start => "_>_",
                Rule::select_code_block | Rule::code_block_start => "_```_",
                Rule::select_html | Rule::html_start => "_</>_",
                Rule::select_paragraph | Rule::select_paragraph_start => "_P:_",
                Rule::select_table | Rule::table_start => "_:-:_",
                Rule::string
                | Rule::string_for_unit_tests__do_not_use_angle
                | Rule::string_for_unit_tests__do_not_use_pipe => "string",
                Rule::unquoted_string => "unquoted string",
                Rule::regex => "regex",
                Rule::regex_char => "regex character",
                Rule::regex_escaped_slash => "_/_",
                Rule::regex_normal_char => "regex character",
                Rule::quoted_string => "quoted string",
                Rule::quoted_char => "character in quoted string",
                Rule::asterisk => "_*_",
                Rule::anchor_start => "_^_",
                Rule::anchor_end => "_$_",
                Rule::quoted_plain_chars => "character in quoted string",
                Rule::escaped_char => "\", ', `, \\, n, r, or t",
                Rule::unicode_seq => "1 - 6 hex characters",
            }
            .to_string()
            .replace('_', "\"")
        })
    }
}

/// Test-only helpers for parsing strings directly, for more direct testing of those grammar rules.
#[cfg(test)]
mod test_helpers {
    use super::*;
    use pest::Parser;

    #[derive(Clone, Copy, PartialEq, Eq)]
    pub enum StringVariant {
        Pipe,
        AngleBracket,
    }

    impl StringVariant {
        /// Tries to parse the given string. If it succeeds, returns the parsed Pairs and the remaining, unparsed query
        /// text.
        pub fn parse(self, query_text: &str) -> Result<(Pairs, &str), Error> {
            let parsed = QueryPairs::parse(self.as_rule(), query_text)?;
            let remaining = match parsed.peek() {
                None => query_text,
                Some(pair) => &query_text[pair.as_span().end()..],
            };
            Ok((parsed, remaining))
        }

        pub fn as_rule(self) -> Rule {
            match self {
                StringVariant::AngleBracket => Rule::string_for_unit_tests__do_not_use_angle,
                StringVariant::Pipe => Rule::string_for_unit_tests__do_not_use_pipe,
            }
        }
    }
}
