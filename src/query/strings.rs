use crate::query::query::Rule;
use pest::iterators::Pairs;
use std::borrow::Cow;
use std::fmt::{Debug, Formatter, Write};

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
    use crate::query::query::StringVariant;
    use pretty_assertions::assert_eq;

    mod strings {
        use super::*;
        use crate::query::strings::tests::CaseMode::*;

        #[test]
        fn single_quoted_string() {
            check_parse(
                StringVariant::Pipe,
                "'hello'\"X",
                parsed_text(CaseSensitive, false, "hello", false),
                "\"X",
            );
        }

        #[test]
        fn double_quoted_string() {
            check_parse(
                StringVariant::Pipe,
                "\"hello\"'X",
                parsed_text(CaseSensitive, false, "hello", false),
                "'X",
            );
        }

        #[test]
        fn quoted_string_newline() {
            check_parse(
                StringVariant::Pipe,
                r"'hello\nworld'",
                parsed_text(CaseSensitive, false, "hello\nworld", false),
                "",
            );
        }

        #[test]
        fn quoted_string_snowman() {
            check_parse(
                StringVariant::Pipe,
                r"'hello\u{2603}world'",
                parsed_text(CaseSensitive, false, "hello☃world", false),
                "",
            );
        }

        #[test]
        fn unquoted_string_to_pipe() {
            check_parse(
                StringVariant::Pipe,
                r"hello'\n | world | multiple | pipes",
                parsed_text(CaseInsensitive, false, r"hello'\n", false),
                "| world | multiple | pipes",
            );
        }

        #[test]
        fn unquoted_no_end_pipe() {
            check_parse(
                StringVariant::Pipe,
                r"hello world   ",
                parsed_text(CaseInsensitive, false, r"hello world", false),
                "",
            );
        }

        #[test]
        fn unquoted_string_to_paren() {
            check_parse(
                StringVariant::Paren,
                r"hello'\n ) world ) multiple ) parens",
                parsed_text(CaseInsensitive, false, r"hello'\n", false),
                ") world ) multiple ) parens",
            );
        }

        #[test]
        fn unquoted_no_end_paren() {
            check_parse(
                StringVariant::Paren,
                r"hello world   ",
                parsed_text(CaseInsensitive, false, r"hello world", false),
                "",
            );
        }

        #[test]
        fn unquoted_string_to_bracket() {
            check_parse(
                StringVariant::Bracket,
                r"hello'\n ] world ] multiple ] brackets",
                parsed_text(CaseInsensitive, false, r"hello'\n", false),
                "] world ] multiple ] brackets",
            );
        }

        #[test]
        fn unquoted_no_end_bracket() {
            check_parse(
                StringVariant::Bracket,
                r"hello world   ",
                parsed_text(CaseInsensitive, false, r"hello world", false),
                "",
            );
        }

        #[test]
        fn unquoted_string_to_colon() {
            check_parse(
                StringVariant::Colon,
                r"hello :-: there",
                parsed_text(CaseInsensitive, false, r"hello", false),
                ":-: there",
            );
        }

        #[test]
        fn unquoted_no_end_colon() {
            check_parse(
                StringVariant::Colon,
                r"hello",
                parsed_text(CaseInsensitive, false, r"hello", false),
                "",
            );
        }

        #[test]
        fn unquoted_string_to_space() {
            check_parse(
                StringVariant::Space,
                r"hello there world",
                parsed_text(CaseInsensitive, false, r"hello", false),
                "there world", // note: the trailing space gets trimmed, but that's fine
            );
        }

        #[test]
        fn unquoted_no_end_space() {
            check_parse(
                StringVariant::Space,
                r"hello",
                parsed_text(CaseInsensitive, false, r"hello", false),
                "",
            );
        }

        #[test]
        fn unquoted_string_to_pipe_unicode() {
            check_parse(
                StringVariant::Pipe,
                r"ἀλφα",
                parsed_text(CaseInsensitive, false, r"ἀλφα", false),
                "",
            );
        }

        #[test]
        fn asterisk() {
            check_parse(
                StringVariant::Pipe,
                r"*",
                parsed_text(CaseSensitive, false, r"", false),
                "",
            );
            assert!(parsed_text(CaseSensitive, false, r"", false).is_equivalent_to_asterisk());
        }

        #[test]
        fn anchors_double_quoted_no_space() {
            check_parse(
                StringVariant::Pipe,
                "^\"hello\"$",
                parsed_text(CaseSensitive, true, "hello", true),
                "",
            );
        }

        #[test]
        fn anchors_single_quoted_with_space() {
            check_parse(
                StringVariant::Pipe,
                "^ 'hello' $",
                parsed_text(CaseSensitive, true, "hello", true),
                "",
            );
        }

        #[test]
        fn anchors_unquoted_to_pipe_with_space() {
            check_parse(
                StringVariant::Pipe,
                "^ hello $ there",
                parsed_text(CaseInsensitive, true, "hello", true),
                " there",
            );
        }

        #[test]
        fn anchors_unquoted_to_pipe_no_space() {
            check_parse(
                StringVariant::Pipe,
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
            check_parse(StringVariant::Pipe, "/hello there$/", parsed_regex("hello there$"), "");
        }

        #[test]
        fn regex_with_escaped_slash() {
            check_parse(StringVariant::Pipe, r"/hello\/there/", parsed_regex(r"hello/there"), "");
        }
    }

    // TODO move the tests in matchers.rs to here, and consolidate them

    fn check_parse(variant: StringVariant, input: &str, expect: ParsedString, remaining: &str) {
        let (pairs, _) = variant.parse(input).unwrap();
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
