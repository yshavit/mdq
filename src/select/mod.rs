//! The query and filtering ability of mdq.
//!
//! The main item here is [`Selector`]:
//!
//! - Parse text into `Selector` using `try_into`.
//! - Run it against an [`MdDoc`](crate::md_elem::MdDoc) using [`Selector::find_nodes`].
mod api;
mod match_selector;
mod matcher;
mod sel_chain;
mod sel_code_block;
mod sel_link_like;
mod sel_list_item;
mod sel_section;
mod sel_single_matcher;
mod sel_table;
mod selector;
mod string_matcher;

pub(crate) use api::*;

pub use crate::query::ParseError;
pub use matcher::*;
pub use selector::*;
