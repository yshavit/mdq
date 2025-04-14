//! Parsed Markdown nodes (and how to parse them).
//!
//! This module provides the AST for a parsed Markdown document. Its main entry point is [parse].
mod tree;
mod tree_ref;

pub use tree::*;

#[cfg(test)]
pub(crate) mod tree_test_utils;
#[cfg(test)]
pub(crate) use tree_test_utils::*;
