use crate::query::query::Rule;
use pest::iterators::Pair;

pub type OnePair<'a> = OneOf<Pair<'a, Rule>>;

pub trait PairStorage<'a> {
    type Output;

    fn store(&mut self, pair: Pair<'a, Rule>);
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

impl<'a> PairStorage<'a> for OneOf<Pair<'a, Rule>> {
    type Output = Result<Option<Pair<'a, Rule>>, String>;

    fn store(&mut self, pair: Pair<'a, Rule>) {
        OneOf::store(self, pair)
    }
}
