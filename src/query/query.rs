use crate::query::traversal::PairMatchStore;
pub(crate) use crate::query::traversal::PairMatcher;
use crate::query::traversal::{ByRule, ByTag, OnePair, Present};
use paste::paste;
use pest::error::Error;
use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest_derive::Parser;
use std::borrow::Cow;
use std::fmt::{Debug, Formatter, Write};
#[cfg(test)]
pub use tests::StringVariant;

#[derive(Parser)]
#[grammar = "query/grammar.pest"] // relative to src
struct QueryPairs; // TODO rename

pub struct Query {
    _private: (),
}

impl Query {
    pub fn parse(query_text: &str) -> Result<Pairs<Rule>, Error<Rule>> {
        QueryPairs::parse(Rule::top, query_text).map_err(Self::format_err)
    }

    fn format_err(err: Error<Rule>) -> Error<Rule> {
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
                Rule::explicit_asterisk => "explicit _*_",
                Rule::string_to_pipe
                | Rule::string_to_paren
                | Rule::string_to_bracket
                | Rule::string_to_space
                | Rule::string_to_colon => "string",
                Rule::unquoted_string_to_pipe
                | Rule::unquoted_string_to_paren
                | Rule::unquoted_string_to_bracket
                | Rule::unquoted_string_to_space
                | Rule::unquoted_string_to_colon => "unquoted string",
                Rule::regex => "_/_",
                Rule::regex_char => "regular expression character",
                Rule::regex_escaped_slash => "_/_",
                Rule::regex_normal_char => "regular expression character",
                Rule::quoted_string => "quoted string",
                Rule::quoted_char => "character",
                Rule::anchor_start => "_^_",
                Rule::anchor_end => "_$_",
                Rule::only_anchors => "_^_ _$_",
                Rule::quoted_plain_chars => "character",
                Rule::escaped_char => "\", ', `, \\, n, r, or t",
                Rule::unicode_seq => "1 - 6 hex characters",
            }
            .to_string()
            .replace('_', "\"")
        })
    }
}

// TODO: I should reorganize this file, break it up into separate files etc

macro_rules! composite_finder {
    ($name:ident { $($elem:ident $result:ty : $finder:ident),+ $(,)? }) => {
        paste! {
            composite_finder!{full: ([<$name Traverser>] / [<$name Results>] ) {$($elem $result: $finder),+} }
        }
    };

    (finder_arg: $name:ident ByRule) => {
        ByRule::new(Rule::$name)
    };
    (finder_arg: $name:ident ByTag) => {
        ByTag::new(stringify!($name))
    };

    (full: ($finder_name:ident / $result_name:ident) { $($elem:ident $result:ty : $finder:ident),+ }) => {
        #[derive(Debug)]
        pub struct $finder_name {
            $(
            $elem: $finder,
            )+
        }

        #[derive(Debug, Default)]
        pub struct $result_name<'a> {
            $(
                pub $elem: $result,
            )+
        }

        impl $finder_name {
            fn new() -> Self {
                Self {
                    $(
                    $elem: composite_finder!(finder_arg: $elem $finder),
                    )+
                }
            }

            pub fn traverse(pairs: Pairs<Rule>) -> $result_name {
                ($finder_name::new(), $result_name::default()).find_in(pairs)
            }
        }

        impl<'a> PairMatchStore<'a> for ($finder_name, $result_name<'a>) {
            type Output = $result_name<'a>;

            fn match_and_store(&mut self, pair: Pair<'a, Rule>) -> Result<(), Pair<'a, Rule>> {
                $(
                if self.0.$elem.matches(&pair) {
                    self.1.$elem.store(pair);
                    Ok(())
                }
                )else+ else {
                    Err(pair)
                }
            }

            fn get(self) -> Self::Output {
                self.1
            }
        }
    }
}

composite_finder! { Section {
    title OnePair<'a>: ByTag,
}}
composite_finder! { ListItem {
    list_ordered Present: ByRule,
    task_checked Present: ByRule,
    task_unchecked Present: ByRule,
    task_either Present: ByRule,
    contents OnePair<'a>: ByTag,
}}

composite_finder! { Link {
    display_text OnePair<'a>: ByTag,
    url_text OnePair<'a>: ByTag,
    image_start Present: ByRule,
}}

composite_finder! { BlockQuote {
    text OnePair<'a>: ByTag,
}}

composite_finder! { CodeBlock {
    language OnePair<'a>: ByTag,
    text OnePair<'a>: ByTag,
}}

composite_finder! { Html {
    text OnePair<'a>: ByTag,
}}

composite_finder! { Paragraph {
    text OnePair<'a>: ByTag,
}}

composite_finder! { Table {
    column OnePair<'a>: ByTag,
    row OnePair<'a>: ByTag,
}}

#[derive(Eq, PartialEq)]
pub struct ParsedString {
    pub text: String,
    pub anchor_start: bool,
    pub anchor_end: bool,
    pub mode: ParsedStringMode,
}

#[derive(Eq, PartialEq, Debug)]
pub enum ParsedStringMode {
    CaseSensitive,
    CaseInsensitive,
    Regex,
}

impl ParsedString {
    // Whether this instance is compatible with an `*` literal
    pub fn is_equivalent_to_asterisk(&self) -> bool {
        match self.mode {
            ParsedStringMode::Regex => false,
            ParsedStringMode::CaseSensitive | ParsedStringMode::CaseInsensitive => {
                if self.text.is_empty() {
                    !(self.anchor_start && self.anchor_end)
                } else {
                    false
                }
            }
        }
    }
}

impl Debug for ParsedString {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.is_equivalent_to_asterisk() {
            return f.write_char('*');
        }

        match self.mode {
            ParsedStringMode::Regex => {
                f.write_char('/')?;
                let escaped = if self.text.contains("/") {
                    Cow::Owned(self.text.replace("/", "//"))
                } else {
                    Cow::Borrowed(&self.text)
                };
                f.write_str(&escaped)?;
                f.write_char('/')?;
            }
            ParsedStringMode::CaseSensitive | ParsedStringMode::CaseInsensitive => {
                if self.anchor_start {
                    f.write_char('^')?;
                }
                if matches!(self.mode, ParsedStringMode::CaseInsensitive) {
                    f.write_str("(i)")?;
                }
                write!(f, "{:?}", self.text)?;
                if self.anchor_end {
                    f.write_char('$')?;
                }
            }
        }
        Ok(())
    }
}

impl TryFrom<Pairs<'_, Rule>> for ParsedString {
    type Error = String;

    fn try_from(pairs: Pairs<Rule>) -> Result<Self, Self::Error> {
        let mut s = Self {
            text: String::with_capacity(pairs.as_str().len()),
            anchor_start: false,
            anchor_end: false,
            mode: ParsedStringMode::CaseSensitive,
        };

        fn build_string(me: &mut ParsedString, pairs: Pairs<Rule>) -> Result<(), String> {
            for pair in pairs {
                match pair.as_rule() {
                    Rule::quoted_plain_chars => {
                        me.text.push_str(pair.as_str());
                    }
                    Rule::escaped_char => {
                        // we'll iterate, but we should really only have one
                        for input_ch in pair.as_str().chars() {
                            let result = match input_ch {
                                result @ ('"' | '\'' | '\\') => result,
                                '`' => '\'',
                                'n' => '\n',
                                'r' => '\r',
                                't' => '\t',
                                err => {
                                    return Err(format!("invalid escape char: {err:?}"));
                                }
                            };
                            me.text.push(result);
                        }
                    }
                    Rule::unicode_seq => {
                        let seq = pair.as_str();
                        let Ok(code_point) = u32::from_str_radix(pair.as_str(), 16) else {
                            return Err(format!("invalid unicode sequence: {seq}"));
                        };
                        let Some(ch) = char::from_u32(code_point) else {
                            return Err(format!("invalid unicode sequence: {seq}"));
                        };
                        me.text.push(ch);
                    }
                    Rule::anchor_start => {
                        me.anchor_start = true;
                    }
                    Rule::anchor_end => {
                        me.anchor_end = true;
                    }
                    Rule::unquoted_string_to_pipe
                    | Rule::unquoted_string_to_paren
                    | Rule::unquoted_string_to_bracket
                    | Rule::unquoted_string_to_space
                    | Rule::unquoted_string_to_colon => {
                        me.mode = ParsedStringMode::CaseInsensitive;
                        me.text.push_str(pair.as_str().trim_end());
                    }
                    Rule::regex => {
                        me.mode = ParsedStringMode::Regex;
                        build_string(me, pair.into_inner())?;
                    }
                    Rule::regex_normal_char => {
                        me.text.push_str(pair.as_str());
                    }
                    Rule::regex_escaped_slash => {
                        me.text.push('/');
                    }
                    _ => {
                        build_string(me, pair.into_inner())?;
                    }
                }
            }
            Ok(())
        }

        build_string(&mut s, pairs)?;
        Ok(s)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pest::Parser;
    use pretty_assertions::assert_eq;

    #[derive(Clone, Copy, PartialEq, Eq)]
    #[allow(dead_code)]
    pub enum StringVariant {
        PIPE,
        PAREN,
        BRACKET,
        SPACE,
        COLON,
    }

    impl StringVariant {
        pub fn parse(self, query_text: &str) -> Result<(Pairs<Rule>, &str), Error<Rule>> {
            let parsed = QueryPairs::parse(self.as_rule(), query_text)?;
            let remaining = match parsed.peek() {
                None => query_text,
                Some(pair) => &query_text[pair.as_span().end()..],
            };
            Ok((parsed, remaining))
        }

        pub fn as_rule(self) -> Rule {
            let rule = match self {
                StringVariant::PIPE => Rule::string_to_pipe,
                StringVariant::PAREN => Rule::string_to_paren,
                StringVariant::BRACKET => Rule::string_to_bracket,
                StringVariant::SPACE => Rule::string_to_space,
                StringVariant::COLON => Rule::string_to_colon,
            };
            rule
        }
    }

    mod strings {
        use super::*;
        use crate::query::query::tests::CaseMode::*;

        #[test]
        fn single_quoted_string() {
            check_parse(
                Rule::string_to_pipe,
                "'hello'\"X",
                parsed_text(CaseSensitive, false, "hello", false),
                "\"X",
            );
        }

        #[test]
        fn double_quoted_string() {
            check_parse(
                Rule::string_to_pipe,
                "\"hello\"'X",
                parsed_text(CaseSensitive, false, "hello", false),
                "'X",
            );
        }

        #[test]
        fn quoted_string_newline() {
            check_parse(
                Rule::string_to_pipe,
                r"'hello\nworld'",
                parsed_text(CaseSensitive, false, "hello\nworld", false),
                "",
            );
        }

        #[test]
        fn quoted_string_snowman() {
            check_parse(
                Rule::string_to_pipe,
                r"'hello\u{2603}world'",
                parsed_text(CaseSensitive, false, "hello☃world", false),
                "",
            );
        }

        #[test]
        fn unquoted_string_to_pipe() {
            check_parse(
                Rule::string_to_pipe,
                r"hello'\n | world | multiple | pipes",
                parsed_text(CaseInsensitive, false, r"hello'\n", false),
                "| world | multiple | pipes",
            );
        }

        #[test]
        fn unquoted_no_end_pipe() {
            check_parse(
                Rule::string_to_pipe,
                r"hello world   ",
                parsed_text(CaseInsensitive, false, r"hello world", false),
                "",
            );
        }

        #[test]
        fn unquoted_string_to_paren() {
            check_parse(
                Rule::string_to_paren,
                r"hello'\n ) world ) multiple ) parens",
                parsed_text(CaseInsensitive, false, r"hello'\n", false),
                ") world ) multiple ) parens",
            );
        }

        #[test]
        fn unquoted_no_end_paren() {
            check_parse(
                Rule::string_to_paren,
                r"hello world   ",
                parsed_text(CaseInsensitive, false, r"hello world", false),
                "",
            );
        }

        #[test]
        fn unquoted_string_to_bracket() {
            check_parse(
                Rule::string_to_bracket,
                r"hello'\n ] world ] multiple ] brackets",
                parsed_text(CaseInsensitive, false, r"hello'\n", false),
                "] world ] multiple ] brackets",
            );
        }

        #[test]
        fn unquoted_no_end_bracket() {
            check_parse(
                Rule::string_to_bracket,
                r"hello world   ",
                parsed_text(CaseInsensitive, false, r"hello world", false),
                "",
            );
        }

        #[test]
        fn unquoted_string_to_colon() {
            check_parse(
                Rule::string_to_colon,
                r"hello :-: there",
                parsed_text(CaseInsensitive, false, r"hello", false),
                ":-: there",
            );
        }

        #[test]
        fn unquoted_no_end_colon() {
            check_parse(
                Rule::string_to_colon,
                r"hello",
                parsed_text(CaseInsensitive, false, r"hello", false),
                "",
            );
        }

        #[test]
        fn unquoted_string_to_pipe_unicode() {
            check_parse(
                Rule::string_to_pipe,
                r"ἀλφα",
                parsed_text(CaseInsensitive, false, r"ἀλφα", false),
                "",
            );
        }

        #[test]
        fn asterisk() {
            check_parse(
                Rule::string_to_pipe,
                r"*",
                parsed_text(CaseSensitive, false, r"", false),
                "",
            );
            assert!(parsed_text(CaseSensitive, false, r"", false).is_equivalent_to_asterisk());
        }

        #[test]
        fn anchors_double_quoted_no_space() {
            check_parse(
                Rule::string_to_pipe,
                "^\"hello\"$",
                parsed_text(CaseSensitive, true, "hello", true),
                "",
            );
        }

        #[test]
        fn anchors_single_quoted_with_space() {
            check_parse(
                Rule::string_to_pipe,
                "^ 'hello' $",
                parsed_text(CaseSensitive, true, "hello", true),
                "",
            );
        }

        #[test]
        fn anchors_unquoted_to_pipe_with_space() {
            check_parse(
                Rule::string_to_pipe,
                "^ hello $ there",
                parsed_text(CaseInsensitive, true, "hello", true),
                " there",
            );
        }

        #[test]
        fn anchors_unquoted_to_pipe_no_space() {
            check_parse(
                Rule::string_to_pipe,
                "^hello$ there",
                parsed_text(CaseInsensitive, true, "hello", true),
                " there",
            );
        }
    }

    mod regexes {
        use super::*;

        #[test]
        fn normal_regex() {
            check_parse(Rule::string_to_pipe, "/hello there$/", parsed_regex("hello there$"), "");
        }

        #[test]
        fn regex_with_escaped_slash() {
            check_parse(
                Rule::string_to_pipe,
                r"/hello\/there/",
                parsed_regex(r"hello/there"),
                "",
            );
        }
    }

    // TODO move the tests in matchers.rs to here, and consolidate them

    fn check_parse(rule: Rule, input: &str, expect: ParsedString, remaining: &str) {
        let pairs = QueryPairs::parse(rule, input).unwrap();
        let consumed = pairs.as_str();
        let rule_tree: Vec<ParsedString> = pairs.into_iter().map(|p| p.into_inner().try_into().unwrap()).collect();
        assert_eq!(rule_tree, vec![expect]);
        assert_eq!(&input[consumed.len()..], remaining);
    }

    #[derive(Copy, Clone, Debug, Eq, PartialEq)]
    enum CaseMode {
        CaseSensitive,
        CaseInsensitive,
    }

    fn parsed_text(case: CaseMode, anchor_start: bool, text: &str, anchor_end: bool) -> ParsedString {
        ParsedString {
            anchor_start,
            text: text.to_string(),
            anchor_end,
            mode: match case {
                CaseMode::CaseSensitive => ParsedStringMode::CaseSensitive,
                CaseMode::CaseInsensitive => ParsedStringMode::CaseInsensitive,
            },
        }
    }

    fn parsed_regex(text: &str) -> ParsedString {
        ParsedString {
            anchor_start: false,
            text: text.to_string(),
            anchor_end: false,
            mode: ParsedStringMode::Regex,
        }
    }
}
