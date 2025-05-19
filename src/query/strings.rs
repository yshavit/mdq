use crate::query::pest::{Pairs, Rule};
use crate::query::{DetachedSpan, InnerParseError};
use std::fmt::{Debug, Formatter, Write};

#[derive(Eq, PartialEq)]
pub struct ParsedString {
    pub text: String,
    pub anchor_start: bool,
    pub anchor_end: bool,
    pub mode: ParsedStringMode,
    pub explicit_wildcard: bool,
    pub replace_string: Option<String>,
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
            return write!(f, "{} *", if self.explicit_wildcard { "explicit" } else { "implicit" });
        }

        match self.mode {
            ParsedStringMode::Regex => {
                fn write_regex_chars(t: &str, f: &mut Formatter<'_>) -> std::fmt::Result {
                    for ch in t.chars() {
                        if ch == '/' {
                            f.write_str("\\/")?;
                        } else {
                            f.write_char(ch)?;
                        }
                    }
                    Ok(())
                }
                if self.replace_string.is_some() {
                    f.write_str("!s")?;
                }
                f.write_char('/')?;
                write_regex_chars(&self.text, f)?;
                if let Some(replace_string) = &self.replace_string {
                    f.write_char('/')?;
                    write_regex_chars(replace_string, f)?;
                }
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

impl ParsedString {
    pub(crate) fn new_from_pairs(pairs: Pairs) -> Result<Self, InnerParseError> {
        let mut s = Self {
            text: String::with_capacity(pairs.as_str().len()),
            anchor_start: false,
            anchor_end: false,
            mode: ParsedStringMode::CaseSensitive,
            explicit_wildcard: false,
            replace_string: None,
        };

        fn build_string(me: &mut ParsedString, pairs: Pairs) -> Result<(), InnerParseError> {
            // If you change or move this, update the comment in grammar.pest ("If you add a variant...") as needed.
            for pair in pairs {
                let span = DetachedSpan::from(&pair);
                match pair.as_rule() {
                    Rule::quoted_plain_chars => {
                        me.text.push_str(pair.as_str());
                    }
                    Rule::asterisk => {
                        me.explicit_wildcard = true;
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
                                    return Err(InnerParseError::Other(span, format!("invalid escape char: {err:?}")));
                                }
                            };
                            me.text.push(result);
                        }
                    }
                    Rule::unicode_seq => {
                        let seq = pair.as_str();
                        let Ok(code_point) = u32::from_str_radix(pair.as_str(), 16) else {
                            return Err(InnerParseError::Other(span, format!("invalid unicode sequence: {seq}")));
                        };
                        let Some(ch) = char::from_u32(code_point) else {
                            return Err(InnerParseError::Other(span, format!("invalid unicode sequence: {seq}")));
                        };
                        me.text.push(ch);
                    }
                    Rule::anchor_start => {
                        me.anchor_start = true;
                    }
                    Rule::anchor_end => {
                        me.anchor_end = true;
                    }
                    Rule::unquoted_string => {
                        me.mode = ParsedStringMode::CaseInsensitive;
                        me.text.push_str(pair.as_str().trim_end());
                    }
                    Rule::regex => {
                        me.mode = ParsedStringMode::Regex;
                        build_string(me, pair.into_inner())?;
                    }
                    Rule::regex_normal_char => {
                        // Append to either the match pattern or replacement string based on the current parsing context
                        let to_edit = match me.replace_string {
                            None => &mut me.text,
                            Some(ref mut s) => s,
                        };
                        to_edit.push_str(pair.as_str());
                    }
                    Rule::regex_escaped_slash => {
                        // Append to either the match pattern or replacement string based on the current parsing context
                        let to_edit = match me.replace_string {
                            None => &mut me.text,
                            Some(ref mut s) => s,
                        };
                        to_edit.push('/');
                    }
                    Rule::regex_replacement_segment => {
                        me.replace_string = Some(String::with_capacity(pair.as_str().len()));
                        build_string(me, pair.into_inner())?;
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
    use crate::query::pest::StringVariant;

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
        fn unquoted_string_to_angle_bracket() {
            check_parse(
                StringVariant::AngleBracket,
                r"hello'\n > world > multiple > brackets",
                parsed_text(CaseInsensitive, false, r"hello'\n", false),
                "> world > multiple > brackets",
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
            check_parse(StringVariant::Pipe, r"*", parsed_wildcard(), "");
            assert!(parsed_wildcard().is_equivalent_to_asterisk());
        }

        #[test]
        fn empty() {
            check_parse(
                StringVariant::Pipe,
                r"",
                parsed_text(CaseSensitive, false, "", false),
                "",
            );
            assert!(parsed_text(CaseSensitive, false, "", false).is_equivalent_to_asterisk());
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
                "there",
            );
        }

        #[test]
        fn anchors_unquoted_to_pipe_no_space() {
            check_parse(
                StringVariant::Pipe,
                "^hello$ there",
                parsed_text(CaseInsensitive, true, "hello", true),
                "there",
            );
        }
    }

    mod regexes {
        use super::*;

        #[test]
        fn normal_regex() {
            check_parse(
                StringVariant::Pipe,
                "/hello there$/",
                parsed_regex("hello there$", None),
                "",
            );
        }

        #[test]
        fn regex_with_escaped_slash() {
            check_parse(
                StringVariant::Pipe,
                r"/hello\/there/",
                parsed_regex(r"hello/there", None),
                "",
            );
        }

        #[test]
        fn with_replacement() {
            check_parse(
                StringVariant::Pipe,
                "!s/hello there$/hi/",
                parsed_regex("hello there$", Some("hi")),
                "",
            );
        }

        #[test]
        fn regex_with_escaped_slash_and_replacement() {
            check_parse(
                StringVariant::Pipe,
                r"!s/hello\/there/pooh\/bear/",
                parsed_regex(r"hello/there", Some("pooh/bear")),
                "",
            );
        }
    }

    mod debug_str {
        use super::*;
        use crate::query::strings::tests::CaseMode::*;

        #[test]
        fn regex() {
            assert_eq!(format!("{:?}", parsed_regex("hello there", None)), r"/hello there/");
        }

        #[test]
        fn regex_escaped_slash() {
            assert_eq!(format!("{:?}", parsed_regex(r"hello/there", None)), r"/hello\/there/");
        }

        #[test]
        fn regex_replace() {
            assert_eq!(
                format!("{:?}", parsed_regex("hello there", Some("hi"))),
                r"!s/hello there/hi/"
            );
        }

        #[test]
        fn regex_replace_escaped_slash() {
            assert_eq!(
                format!("{:?}", parsed_regex(r"hello/there", Some(r"pooh/bear"))),
                r"!s/hello\/there/pooh\/bear/"
            );
        }

        #[test]
        fn string_with_anchor() {
            assert_eq!(
                format!("{:?}", parsed_text(CaseSensitive, true, "hello", true)),
                "^\"hello\"$"
            );
        }

        #[test]
        fn case_insensitive() {
            assert_eq!(
                format!("{:?}", parsed_text(CaseInsensitive, false, "hello", false)),
                "(i)\"hello\""
            );
        }

        #[test]
        fn implicit_wildcard() {
            let wildcard = ParsedString {
                text: String::new(),
                anchor_start: false,
                anchor_end: false,
                mode: ParsedStringMode::CaseSensitive,
                explicit_wildcard: false,
                replace_string: None,
            };
            assert_eq!(format!("{wildcard:?}"), "implicit *");
        }

        #[test]
        fn explicit_wildcard() {
            let wildcard = ParsedString {
                text: String::new(),
                anchor_start: false,
                anchor_end: false,
                mode: ParsedStringMode::CaseSensitive,
                explicit_wildcard: true,
                replace_string: None,
            };
            assert_eq!(format!("{wildcard:?}"), "explicit *");
        }
    }

    fn check_parse(variant: StringVariant, input: &str, expect: ParsedString, remaining: &str) {
        let (pairs, _) = variant.parse(input).unwrap();
        let consumed = pairs.as_str();
        let rule_tree: Vec<ParsedString> = pairs
            .into_iter()
            .map(|p| ParsedString::new_from_pairs(p.into_inner()).unwrap())
            .collect();
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
            explicit_wildcard: false,
            replace_string: None,
        }
    }

    fn parsed_regex(text: &str, replacement: Option<&str>) -> ParsedString {
        ParsedString {
            anchor_start: false,
            text: text.to_string(),
            anchor_end: false,
            mode: ParsedStringMode::Regex,
            explicit_wildcard: false,
            replace_string: replacement.map(String::from),
        }
    }

    fn parsed_wildcard() -> ParsedString {
        ParsedString {
            anchor_start: false,
            text: String::new(),
            anchor_end: false,
            mode: ParsedStringMode::CaseSensitive,
            explicit_wildcard: true,
            replace_string: None,
        }
    }
}
