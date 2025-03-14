use paste::paste;
use pest::iterators::{Pair, Pairs};
use pest_derive::Parser;
use std::borrow::Cow;
use std::fmt::{Debug, Formatter, Write};

#[derive(Parser)]
#[grammar = "query/grammar.pest"] // relative to src
pub struct QueryPairs; // TODO rename

// TODO: I should reorganize this file, break it up into separate files etc

pub trait PairStorage<'a> {
    type Output;

    fn store(&mut self, pair: Pair<'a, Rule>);
    fn get(self) -> Self::Output;
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Default)]
pub struct Present(bool);

impl Present {
    pub fn is_present(&self) -> bool {
        self.0
    }
}

impl PairStorage<'_> for Present {
    type Output = bool;

    fn store(&mut self, _pair: Pair<'_, Rule>) {
        self.0 = true
    }

    fn get(self) -> Self::Output {
        self.0
    }
}

#[derive(Debug)]
pub struct OneOf<T>(Result<Option<T>, ()>); // TODO move to a util

impl<T> Default for OneOf<T> {
    fn default() -> Self {
        Self(Ok(None))
    }
}

impl<T> OneOf<T> {
    pub fn take(self) -> Result<Option<T>, String> {
        self.0.map_err(|_| "multiple items found".to_string())
    }
}

impl<'a> PairStorage<'a> for OneOf<Pair<'a, Rule>> {
    type Output = Result<Option<Pair<'a, Rule>>, String>;

    fn store(&mut self, pair: Pair<'a, Rule>) {
        self.0 = match self.0 {
            Ok(Some(_)) | Err(_) => Err(()),
            Ok(None) => Ok(Some(pair)),
        }
    }

    fn get(self) -> Self::Output {
        self.take()
    }
}

pub trait PairMatcher {
    fn matches(&self, pair: &Pair<Rule>) -> bool;

    fn find_all_in(self, pairs: Pairs<Rule>) -> Vec<Pair<Rule>>
    where
        Self: Sized,
    {
        FindAll::new(self).find_in(pairs)
    }
}

#[derive(Debug)]
pub struct FindAll<'a, M>(M, Vec<Pair<'a, Rule>>);

impl<'a, M> FindAll<'a, M> {
    pub fn new(matcher: M) -> Self {
        Self(matcher, Vec::new())
    }
}

impl<'a, M> PairMatchStore<'a> for FindAll<'a, M>
where
    M: PairMatcher,
{
    type Output = Vec<Pair<'a, Rule>>;

    fn match_and_store(&mut self, pair: Pair<'a, Rule>) -> Result<(), Pair<'a, Rule>> {
        if self.0.matches(&pair) {
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

pub trait PairMatchStore<'a> {
    type Output;

    fn match_and_store(&mut self, pair: Pair<'a, Rule>) -> Result<(), Pair<'a, Rule>>;

    fn get(self) -> Self::Output;

    fn find_in(mut self, pairs: Pairs<'a, Rule>) -> Self::Output
    where
        Self: Sized,
    {
        fn build<'b>(me: &mut impl PairMatchStore<'b>, pairs: Pairs<'b, Rule>) {
            for pair in pairs {
                if let Err(unmatched) = me.match_and_store(pair) {
                    build(me, unmatched.into_inner())
                }
            }
        }
        build(&mut self, pairs);
        self.get()
    }
}

#[derive(Debug)]
pub struct ByRule(Rule);

impl ByRule {
    pub fn new(rule: Rule) -> Self {
        Self(rule)
    }
}

impl<'a> PairMatcher for ByRule {
    fn matches(&self, pair: &Pair<Rule>) -> bool {
        self.0 == pair.as_rule()
    }
}

#[derive(Debug)]
pub struct ByTag(&'static str);

impl ByTag {
    pub fn new(tag: &'static str) -> Self {
        Self(tag)
    }
}

impl PairMatcher for ByTag {
    fn matches(&self, pair: &Pair<Rule>) -> bool {
        match pair.as_node_tag() {
            Some(t) => t == self.0,
            None => false,
        }
    }
}

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
    title OneOf<Pair<'a, Rule>>: ByTag,
}}
composite_finder! { ListItem {
    list_ordered Present: ByRule,
    task_checked Present: ByRule,
    task_unchecked Present: ByRule,
    task_either Present: ByRule,
    contents OneOf<Pair<'a, Rule>>: ByTag,
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
