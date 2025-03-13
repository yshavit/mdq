use paste::paste;
use pest::iterators::{Pair, Pairs};
use pest_derive::Parser;
use std::borrow::Cow;
use std::fmt::{Debug, Formatter, Write};

#[derive(Parser)]
#[grammar = "query/grammar.pest"] // relative to src
pub struct QueryPairs; // TODO rename

pub trait PairMatcher<'a> {
    // TODO rename to PairFinder, to disambiguate from Matcher
    type Output;

    fn try_add(&mut self, pair: Pair<'a, Rule>) -> Result<(), Pair<'a, Rule>>;

    fn get(self) -> Self::Output;

    fn find_inners<I>(mut self, root: I) -> Self::Output
    where
        Self: Sized,
        I: IntoIterator<Item = Pair<'a, Rule>>,
    {
        fn build_result<'b, I2, M>(to: &mut M, from_pairs: I2)
        where
            M: PairMatcher<'b>,
            I2: IntoIterator<Item = Pair<'b, Rule>>,
        {
            for pair in from_pairs {
                if let Err(unmatched) = to.try_add(pair) {
                    build_result(to, unmatched.into_inner());
                }
            }
        }
        build_result(&mut self, root);
        self.get()
    }
}

pub struct ByRule<'a>(Rule, Vec<Pair<'a, Rule>>);

impl ByRule<'_> {
    pub fn new(rule: Rule) -> Self {
        Self(rule, Vec::new())
    }
}

impl<'a> PairMatcher<'a> for ByRule<'a> {
    type Output = Vec<Pair<'a, Rule>>;

    fn try_add(&mut self, pair: Pair<'a, Rule>) -> Result<(), Pair<'a, Rule>> {
        if self.0 == pair.as_rule() {
            self.1.push(pair);
            Ok(())
        } else {
            Err(pair)
        }
    }

    fn get(self) -> Self::Output {
        self.1
    }
}

pub struct ByTag<'a>(&'static str, OneOf<Pair<'a, Rule>>);

impl<'a> ByTag<'a> {
    pub fn new(tag: &'static str) -> Self {
        Self(tag, OneOf::empty())
    }
}

impl<'a> PairMatcher<'a> for ByTag<'a> {
    type Output = Result<Option<Pair<'a, Rule>>, String>;

    fn try_add(&mut self, pair: Pair<'a, Rule>) -> Result<(), Pair<'a, Rule>> {
        match pair.as_node_tag() {
            Some(t) if t == self.0 => {
                self.1.add(pair);
                Ok(())
            }
            _ => Err(pair),
        }
    }

    fn get(self) -> Self::Output {
        self.1.take()
    }
}

pub struct OneOf<T>(Result<Option<T>, ()>); // TODO move to a util

impl<T> OneOf<T> {
    pub fn empty() -> Self {
        Self(Ok(None))
    }

    pub fn from<I>(iter: I) -> Result<Option<T>, String>
    where
        I: IntoIterator<Item = T>,
    {
        let mut me = Self::empty();
        for item in iter {
            me.add(item);
        }
        me.take()
    }

    pub fn add(&mut self, item: T) {
        self.0 = match self.0 {
            Ok(Some(_)) | Err(_) => Err(()),
            Ok(None) => Ok(Some(item)),
        }
    }

    pub fn take(self) -> Result<Option<T>, String> {
        self.0.map_err(|_| "multiple items found".to_string())
    }
}

macro_rules! composite_finder {
    ($name:ident { $elem_name:ident:$elem_type:ident($elem_ctor:expr) $(,)? }) => {
        paste! { composite_finder !{ [<$name Tree>] { $elem_name: $elem_type($elem_ctor) } => [<$name Rule>] } }
    };

    ($name:ident { $e1_name:ident:$e1_type:ident($e1_ctor:expr), $($es_name:ident:$es_type:ident($es_ctor:expr) ),+ $(,)? }) => {
        paste! { composite_finder!{ [<$name Tree>] { $e1_name: $e1_type($e1_ctor), $($es_name: $es_type($es_ctor)),+ } => [<$name Rules>] } }
    };

    ($name:ident { $($elem_name:ident : $elem_type:ident ($elem_ctor:expr)),+ } => $result_name:ident) => {
        pub struct $name<'a> {
            $(
            $elem_name: $elem_type<'a>,
            )+
        }

        pub struct $result_name<'a> {
            $(
            pub $elem_name: <$elem_type<'a> as PairMatcher<'a>>::Output,
            )+
        }

        impl<'a> $name<'a> {
            pub fn new() -> Self {
                Self {
                    $(
                    $elem_name: $elem_type::new($elem_ctor)
                    ),+
                }
            }

            pub fn find(pairs: Pairs<'a, Rule>) -> $result_name<'a> {
                Self::new().find_inners(pairs)
            }
        }

        impl<'a> PairMatcher<'a> for $name<'a> {
            type Output = $result_name<'a>;

            fn try_add(&mut self, pair: Pair<'a, Rule>) -> Result<(), Pair<'a, Rule>> {
                $(
                let Err(pair) = self.$elem_name.try_add(pair) else {
                    return Ok(());
                };
                )*
                Err(pair)
            }

            fn get(self) -> Self::Output {
                Self::Output {
                    $(
                    $elem_name: self.$elem_name.get(),
                    )+

                }
            }
        }
    };
}

composite_finder! { Section {
    title: ByTag("title"),
}}
composite_finder! { ListItem {
    ordered: ByRule(Rule::list_ordered),
    checked: ByRule(Rule::task_checked),
    unchecked: ByRule(Rule::task_unchecked),
    either: ByRule(Rule::task_either),
    contents: ByTag("contents"),
}}

#[derive(Eq, PartialEq)]
pub struct ParsedString {
    pub text: String,
    pub anchor_start: bool,
    pub anchor_end: bool,
    pub is_regex: bool,
}

impl Debug for ParsedString {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        if self.is_regex {
            f.write_char('/')?;
            let escaped = if self.text.contains("/") {
                Cow::Owned(self.text.replace("/", "//"))
            } else {
                Cow::Borrowed(&self.text)
            };
            f.write_str(&escaped)?;
            f.write_char('/')?;
        } else {
            if self.anchor_start {
                f.write_char('^')?;
            }
            write!(f, "{:?}", self.text)?;
            if self.anchor_end {
                f.write_char('$')?;
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
            is_regex: false,
        };

        fn build_string(me: &mut ParsedString, pairs: Pairs<Rule>) -> Result<(), String> {
            for pair in pairs {
                match pair.as_rule() {
                    Rule::literal_char => {
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
                        me.text.push_str(pair.as_str().trim_end());
                    }
                    Rule::regex => {
                        me.is_regex = true;
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

    mod strings {
        use super::*;

        #[test]
        fn single_quoted_string() {
            check_parse(
                Rule::string_to_pipe,
                "'hello'\"X",
                parsed_text(false, "hello", false),
                "\"X",
            );
        }

        #[test]
        fn double_quoted_string() {
            check_parse(
                Rule::string_to_pipe,
                "\"hello\"'X",
                parsed_text(false, "hello", false),
                "'X",
            );
        }

        #[test]
        fn quoted_string_newline() {
            check_parse(
                Rule::string_to_pipe,
                r"'hello\nworld'",
                parsed_text(false, "hello\nworld", false),
                "",
            );
        }

        #[test]
        fn quoted_string_snowman() {
            check_parse(
                Rule::string_to_pipe,
                r"'hello\u{2603}world'",
                parsed_text(false, "hello☃world", false),
                "",
            );
        }

        #[test]
        fn unquoted_string_to_pipe() {
            check_parse(
                Rule::string_to_pipe,
                r"hello'\n | world | multiple | pipes",
                parsed_text(false, r"hello'\n", false),
                "| world | multiple | pipes",
            );
        }

        #[test]
        fn unquoted_no_end_pipe() {
            check_parse(
                Rule::string_to_pipe,
                r"hello world   ",
                parsed_text(false, r"hello world", false),
                "",
            );
        }

        #[test]
        fn unquoted_string_to_paren() {
            check_parse(
                Rule::string_to_paren,
                r"hello'\n ) world ) multiple ) parens",
                parsed_text(false, r"hello'\n", false),
                ") world ) multiple ) parens",
            );
        }

        #[test]
        fn unquoted_no_end_paren() {
            check_parse(
                Rule::string_to_paren,
                r"hello world   ",
                parsed_text(false, r"hello world", false),
                "",
            );
        }

        #[test]
        fn unquoted_string_to_bracket() {
            check_parse(
                Rule::string_to_bracket,
                r"hello'\n ] world ] multiple ] brackets",
                parsed_text(false, r"hello'\n", false),
                "] world ] multiple ] brackets",
            );
        }

        #[test]
        fn unquoted_no_end_bracket() {
            check_parse(
                Rule::string_to_bracket,
                r"hello world   ",
                parsed_text(false, r"hello world", false),
                "",
            );
        }

        #[test]
        fn anchors_double_quoted_no_space() {
            check_parse(
                Rule::string_to_pipe,
                "^\"hello\"$",
                parsed_text(true, "hello", true),
                "",
            );
        }

        #[test]
        fn anchors_single_quoted_with_space() {
            check_parse(
                Rule::string_to_pipe,
                "^ 'hello' $",
                parsed_text(true, "hello", true),
                "",
            );
        }

        #[test]
        fn anchors_unquoted_to_pipe_with_space() {
            check_parse(
                Rule::string_to_pipe,
                "^ hello $ there",
                parsed_text(true, "hello", true),
                " there",
            );
        }

        #[test]
        fn anchors_unquoted_to_pipe_no_space() {
            check_parse(
                Rule::string_to_pipe,
                "^hello$ there",
                parsed_text(true, "hello", true),
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

    #[test]
    fn todo() {
        todo!("check things that fail to parse");
    }

    fn check_parse(rule: Rule, input: &str, expect: ParsedString, remaining: &str) {
        let pairs = QueryPairs::parse(rule, input).unwrap();
        let consumed = pairs.as_str();
        let rule_tree: Vec<ParsedString> = pairs.into_iter().map(|p| p.into_inner().try_into().unwrap()).collect();
        assert_eq!(rule_tree, vec![expect]);
        assert_eq!(&input[consumed.len()..], remaining);
    }

    fn parsed_text(anchor_start: bool, text: &str, anchor_end: bool) -> ParsedString {
        ParsedString {
            anchor_start,
            text: text.to_string(),
            anchor_end,
            is_regex: false,
        }
    }

    fn parsed_regex(text: &str) -> ParsedString {
        ParsedString {
            anchor_start: false,
            text: text.to_string(),
            anchor_end: false,
            is_regex: true,
        }
    }
}
