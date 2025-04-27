use crate::query::pest::{Pair, Pairs, Rule};
use crate::query::traversal::MatchStoreResult;
use crate::query::traversal::PairMatchStore;
use crate::query::traversal::PairMatcher;
use crate::query::traversal::{ByRule, ByTag, OnePair, Present};
use paste::paste;

/// A macro for creating:
/// 1. a `${name}Traverser`, which looks for several elements as it goes; and
/// 2. a `${name}Results`, which stores every Pair it finds in a [OneOf]
macro_rules! composite_finder {
    ($name:ident { $($elem:ident $result:ty : $finder:ident),+ $(,)? }) => {
        paste! {
            composite_finder!{full: ([<$name Traverser>] / [<$name Results>] / [< $name MatchStore >] ) {$($elem $result: $finder),+} }
        }
    };

    (finder_arg: $name:ident ByRule) => {
        ByRule::new(Rule::$name)
    };
    (finder_arg: $name:ident ByTag) => {
        ByTag::new(stringify!($name))
    };

    (full: ($finder_name:ident / $result_name:ident / $match_store_name:ident) { $($elem:ident $result:ty : $finder:ident),+ }) => {
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

        struct $match_store_name<'a>($finder_name, $result_name<'a>);

        impl $finder_name {
            fn new() -> Self {
                Self {
                    $(
                    $elem: composite_finder!(finder_arg: $elem $finder),
                    )+
                }
            }

            pub fn traverse(pairs: Pairs) -> $result_name {
                $match_store_name($finder_name::new(), $result_name::default()).find_in(pairs)
            }
        }

        impl<'a> PairMatchStore<'a> for $match_store_name<'a> {
            type Output = $result_name<'a>;

            fn match_and_store(&mut self, pair: Pair<'a, >) -> MatchStoreResult<'a> {
                $(
                if self.0.$elem.matches(&pair) {
                    self.1.$elem.store(pair);
                    MatchStoreResult::Stored
                }
                )else+ else {
                    MatchStoreResult::NotStored(pair)
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
