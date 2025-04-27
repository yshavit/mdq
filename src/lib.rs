//! This crate is the library behind the [mdq] CLI tool.
//!
//! <div class="warning">
//!
//! **This is a preview API**. While I'll try to keep it as stable as possible, some breaking changes may occur.
//!
//! I will note any such changes in the [release notes on GitHub]. You can also find them searching the
//! [`breaking change` label] in the project's issue tracker.
//!
//! [release notes on GitHub]: https://github.com/yshavit/mdq/releases
//! [`breaking change` label]: https://github.com/yshavit/mdq/issues?q=label%3A%22breaking%20change%22
//!
//! </div>
//!
//! The general flow to use this crate is:
//!
//! 1. Parse Markdown into [`md_elem::MdElem`]s via [`md_elem::MdDoc::parse`]
//! 2. Parse a query via [`select::Selector`'s `TryFrom::<&str>`][selector-parse]
//! 3. Use [`select::Selector::find_nodes`] to filter the `MdElem`s down
//! 4. Use [`output`] to write the results
//!
//! The [`run`] module implements this workflow using options similar to the CLI's flags and a facade for I/O. You can
//! also do it yourself. See that module's documentation for an example.
//!
//! ## Example: End-to-end parsing and selection
//!
//! To parse some Markdown and a query string and output the result as Markdown to stdout:
//!
//! ```
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! use indoc::indoc;
//!
//! // Define some markdown
//! let markdown_text = indoc! {r##"
//! ## First section
//!
//! - hello
//! - world
//!
//! ## Second section
//!
//! - foo
//! - bar
//! "##};
//! let parsed_md = mdq::md_elem::MdDoc::parse(markdown_text, &mdq::md_elem::ParseOptions::default())?;
//!
//! // Parse a selector that looks for a section with title containing "second", and
//! // then looks for list items within it
//! let query_text = "# second | - *";
//! let selector: mdq::select::Selector = query_text.try_into()?;
//!
//! // Run the selector against the parsed Markdown
//! let (found_nodes, found_nodes_ctx) = selector.find_nodes(parsed_md);
//!
//! // Output. Note our use of
//! let mut output_string = String::new();
//! let writer = mdq::output::MdWriter::default();
//! writer.write(&found_nodes_ctx, &found_nodes, &mut output_string);
//!
//! assert_eq!(
//!     output_string,
//!     indoc! {r"
//!     - foo
//!
//!     - bar
//! "});
//! #
//! #     Ok(())
//! # }
//! ```
//!
//! [mdq]: https://github.com/yshavit/mdq
//! [selector-parse]: select::Selector#impl-TryFrom<%26str>-for-Selector

pub mod md_elem;
pub mod output;
mod query;
pub mod run;
pub mod select;
mod util;
