mod query;
mod selectors_parse;

mod strings;
mod traversal;
mod traversal_composites;

#[cfg(test)]
pub use query::StringVariant;
#[allow(unused_imports)]
pub use query::{Error, Pair, Pairs, Query};
pub use selectors_parse::*;
