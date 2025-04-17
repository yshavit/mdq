//! End-to-end runs.
//!
//! This module combines the [`crate::md_elem`], [`crate::select`], and [`crate::output`] mods into a single workflow.
//! It's useful for building functionality like the CLI's, but running it within-process.
mod cli;
mod run;

pub use cli::*;
pub use run::*;
