use crate::query::query::{Pairs, Rule};
use crate::select::{DetachedSpan, ParseError};
use std::borrow::Cow;
use std::fmt::{Debug, Formatter, Write};

#[derive(Eq, PartialEq)]
pub struct ParsedString {
    pub text: String,
    pub anchor_start: bool,
    pub anchor_end: bool,
    pub mode: ParsedStringMode,
    pub explicit_wildcard: bool,
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

impl TryFrom<Pairs<'_>> for ParsedString {
    type Error = ParseError;

    fn try_from(pairs: Pairs) -> Result<Self, Self::Error> {
        let mut s = Self {
            text: String::with_capacity(pairs.as_str().len()),
            anchor_start: false,
            anchor_end: false,
            mode: ParsedStringMode::CaseSensitive,
            explicit_wildcard: false,
        };

        fn build_string(me: &mut ParsedString, pairs: Pairs) -> Result<(), ParseError> {
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
                                    return Err(ParseError::Other(span, format!("invalid escape char: {err:?}")));
                                }
                            };
                            me.text.push(result);
                        }
                    }
                    Rule::unicode_seq => {
                        let seq = pair.as_str();
                        let Ok(code_point) = u32::from_str_radix(pair.as_str(), 16) else {
                            return Err(ParseError::Other(span, format!("invalid unicode sequence: {seq}")));
                        };
                        let Some(ch) = char::from_u32(code_point) else {
                            return Err(ParseError::Other(span, format!("invalid unicode sequence: {seq}")));
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
                StringVariant::AngleBracket,
                "'hello'\"X",
                parsed_text(CaseSensitive, false, "hello", false),
                "\"X",
            );
        }

        #[test]
        fn double_quoted_string() {
            check_parse(
                StringVariant::AngleBracket,
                "\"hello\"'X",
                parsed_text(CaseSensitive, false, "hello", false),
                "'X",
            );
        }

        #[test]
        fn quoted_string_newline() {
            check_parse(
                StringVariant::AngleBracket,
                r"'hello\nworld'",
                parsed_text(CaseSensitive, false, "hello\nworld", false),
                "",
            );
        }

        #[test]
        fn quoted_string_snowman() {
            check_parse(
                StringVariant::AngleBracket,
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
                StringVariant::AngleBracket,
                r"hello world   ",
                parsed_text(CaseInsensitive, false, r"hello world", false),
                "",
            );
        }

        #[test]
        fn unquoted_string_to_pipe_unicode() {
            check_parse(
                StringVariant::AngleBracket,
                r"ἀλφα",
                parsed_text(CaseInsensitive, false, r"ἀλφα", false),
                "",
            );
        }

        #[test]
        fn asterisk() {
            check_parse(StringVariant::AngleBracket, r"*", parsed_wildcard(), "");
            assert!(parsed_wildcard().is_equivalent_to_asterisk());
        }

        #[test]
        fn empty() {
            check_parse(
                StringVariant::AngleBracket,
                r"",
                parsed_text(CaseSensitive, false, "", false),
                "",
            );
            assert!(parsed_text(CaseSensitive, false, "", false).is_equivalent_to_asterisk());
        }

        #[test]
        fn anchors_double_quoted_no_space() {
            check_parse(
                StringVariant::AngleBracket,
                "^\"hello\"$",
                parsed_text(CaseSensitive, true, "hello", true),
                "",
            );
        }

        #[test]
        fn anchors_single_quoted_with_space() {
            check_parse(
                StringVariant::AngleBracket,
                "^ 'hello' $",
                parsed_text(CaseSensitive, true, "hello", true),
                "",
            );
        }

        #[test]
        fn anchors_unquoted_to_pipe_with_space() {
            check_parse(
                StringVariant::AngleBracket,
                "^ hello $ there",
                parsed_text(CaseInsensitive, true, "hello", true),
                "there",
            );
        }

        #[test]
        fn anchors_unquoted_to_pipe_no_space() {
            check_parse(
                StringVariant::AngleBracket,
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
                StringVariant::AngleBracket,
                "/hello there$/",
                parsed_regex("hello there$"),
                "",
            );
        }

        #[test]
        fn regex_with_escaped_slash() {
            check_parse(
                StringVariant::AngleBracket,
                r"/hello\/there/",
                parsed_regex(r"hello/there"),
                "",
            );
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
            explicit_wildcard: false,
        }
    }

    fn parsed_regex(text: &str) -> ParsedString {
        ParsedString {
            anchor_start: false,
            text: text.to_string(),
            anchor_end: false,
            mode: ParsedStringMode::Regex,
            explicit_wildcard: false,
        }
    }

    fn parsed_wildcard() -> ParsedString {
        ParsedString {
            anchor_start: false,
            text: String::new(),
            anchor_end: false,
            mode: ParsedStringMode::CaseSensitive,
            explicit_wildcard: true,
        }
    }
}
