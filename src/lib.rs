//! This crate is the library behind the [mdq] CLI tool.
//!
//! The general flow is to:
//!
//! 1. Parse Markdown into [`md_elem::MdElem`]s via [`md_elem::parse`]
//! 2. Parse a query via [`select::Selector`'s `TryFrom::<&str>`][selector-parse]
//! 3. Use [`select::Selector::find_nodes`] to filter the `MdElem`s down
//! 4. Use [`output`] to write the results
//!
//! The [`run`] module implements this workflow using options similar to the CLI's flags and a facade for I/O. You can
//! also do it yourself.
//!
//! [mdq]: https://github.com/yshavit/mdq
//! [selector-parse]: select::Selector#impl-TryFrom<%26str>-for-Selector

pub mod md_elem;
pub mod output;
mod query;
pub mod run;
pub mod select;
mod util;
