use crate::query::pest::{Pair, Pairs, Rule};

pub type OnePair<'a> = OneOf<Pair<'a>>;

/// A trait for determining whether a [Pair] matches some condition.
pub trait PairMatcher {
    fn matches(&self, pair: &Pair) -> bool;

    fn find_all_in(self, pairs: Pairs) -> Vec<Pair>
    where
        Self: Sized,
    {
        FindAll::new(self).find_in(pairs)
    }
}

/// A trait for matching pairs, and then either storing them or traversing further.
///
/// This lets us separate out the matching (which is typically [ByRule] or [ByTag], or some combination of them) from
/// the storing (which may be to add to a vec, to add to [OneOf], or anything else).
pub trait PairMatchStore<'a> {
    type Output;

    fn match_and_store(&mut self, pair: Pair<'a>) -> MatchStoreResult<'a>;

    fn get(self) -> Self::Output;

    fn find_in(mut self, pairs: Pairs<'a>) -> Self::Output
    where
        Self: Sized,
    {
        fn build<'b>(me: &mut impl PairMatchStore<'b>, pairs: Pairs<'b>) {
            for pair in pairs {
                if let MatchStoreResult::NotStored(unmatched) = me.match_and_store(pair) {
                    build(me, unmatched.into_inner())
                }
            }
        }
        build(&mut self, pairs);
        self.get()
    }
}

pub enum MatchStoreResult<'a> {
    Stored,
    NotStored(Pair<'a>),
}

#[derive(Copy, Clone, Eq, PartialEq, Debug, Default)]
pub struct Present(bool);

impl Present {
    pub fn is_present(&self) -> bool {
        self.0
    }
}

impl Present {
    pub fn store(&mut self, _pair: Pair) {
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
pub struct FindAll<'a, M>(M, Vec<Pair<'a>>);

impl<M> FindAll<'_, M> {
    pub fn new(matcher: M) -> Self {
        Self(matcher, Vec::new())
    }
}

impl<'a, M> PairMatchStore<'a> for FindAll<'a, M>
where
    M: PairMatcher,
{
    type Output = Vec<Pair<'a>>;

    fn match_and_store(&mut self, pair: Pair<'a>) -> MatchStoreResult<'a> {
        if self.0.matches(&pair) {
            self.1.push(pair);
            MatchStoreResult::Stored
        } else {
            MatchStoreResult::NotStored(pair)
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

impl PairMatcher for ByRule {
    fn matches(&self, pair: &Pair) -> bool {
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
    fn matches(&self, pair: &Pair) -> bool {
        match pair.as_node_tag() {
            Some(t) => t == self.0,
            None => false,
        }
    }
}
