use crate::query::query::Rule;
use pest::iterators::{Pair, Pairs};

pub type OnePair<'a> = OneOf<Pair<'a, Rule>>;

/// A trait for determining whether a [Pair] matches some condition.
pub trait PairMatcher {
    fn matches(&self, pair: &Pair<Rule>) -> bool;

    fn find_all_in(self, pairs: Pairs<Rule>) -> Vec<Pair<Rule>>
    where
        Self: Sized,
    {
        FindAll::new(self).find_in(pairs)
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

#[derive(Copy, Clone, Eq, PartialEq, Debug, Default)]
pub struct Present(bool);

impl Present {
    pub fn is_present(&self) -> bool {
        self.0
    }
}

impl Present {
    pub fn store(&mut self, _pair: Pair<'_, Rule>) {
        self.0 = true
    }
}

#[derive(Debug)]
pub struct OneOf<T>(Result<Option<T>, ()>);

impl<T> Default for OneOf<T> {
    fn default() -> Self {
        Self(Ok(None))
    }
}

impl<T> OneOf<T> {
    // TODO the Err should be an Error<Rule>
    pub fn take(self) -> Result<Option<T>, String> {
        self.0.map_err(|_| "multiple items found".to_string())
    }

    pub fn store(&mut self, item: T) {
        self.0 = match self.0 {
            Ok(Some(_)) | Err(_) => Err(()),
            Ok(None) => Ok(Some(item)),
        }
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
