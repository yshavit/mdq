//! End-to-end runs.
//!
//! This module combines the [`crate::md_elem`], [`crate::select`], and [`crate::output`] mods into a single workflow.
//! It's useful for building functionality like the CLI's, but running it within-process.
//!
//! ## Example
//!
//! ```
//! # use mdq::run;
//!
//! // First, let's define a mocked I/O. Replace this with whatever you need.
//! #[derive(Default)]
//! struct MockIo {
//!     stdout: Vec<u8>,
//! }
//!
//! impl run::OsFacade for MockIo {
//!     fn read_stdin(&self) -> std::io::Result<String> {
//!         Ok("- hello\n- world".to_string())
//!     }
//!
//!     fn read_file(&self, path: &str) -> std::io::Result<String> {
//!         Err(std::io::Error::new(std::io::ErrorKind::NotFound, path))
//!     }
//!
//!     fn stdout(&mut self) -> impl std::io::Write {
//!         &mut self.stdout
//!     }
//!
//!     fn write_error(&mut self, err: run::Error) {
//!         eprintln!("{err}")
//!     }
//! }
//!
//! // Now, use it:
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//!
//! // Define our "CLI" options. Use the defaults, but add a positional arg for an "- h" selector.
//! let mut cli_options = run::RunOptions::default();
//! cli_options.selectors = "- h".to_string(); // list items containing an 'h'
//!
//! let mut os_facade = MockIo::default();
//! let found_any = run::run(&cli_options, &mut os_facade);
//! let stdout_text = String::from_utf8(os_facade.stdout)?;
//!
//! assert_eq!(found_any, true);
//! assert_eq!(stdout_text, "- hello\n");
//! #
//! #     Ok(())
//! # }
//! ```
mod cli;
mod run_main;

pub use cli::*;
pub use run_main::*;
