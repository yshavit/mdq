mod query;

mod error;
mod matcher_try_from;
mod selector_try_from;
mod strings;
mod traversal;
mod traversal_composites;

pub use error::*;

#[cfg(test)]
pub use query::StringVariant;
#[allow(unused_imports)]
pub use query::{Error, Pair, Pairs, Query};
