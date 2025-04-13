mod fmt_md;
mod fmt_md_inlines;
mod fmt_plain_inline;
mod fmt_plain_str;
mod fmt_plain_writer;
mod footnote_transform;
mod link_transform;
mod output_adapter;
mod tree_ref_serde;

use crate::md_elem::{MdContext, MdElem};
use crate::output::tree_ref_serde::MdSerde;
use serde::Serialize;

pub use crate::output::fmt_md::*;
pub use crate::output::fmt_md_inlines::*;
pub use crate::output::link_transform::*;
pub use crate::output::output_adapter::*;

pub use crate::output::fmt_plain_inline::*;
pub use crate::output::fmt_plain_str::*;

pub fn serializable<'a>(
    elems: &'a [MdElem],
    ctx: &'a MdContext,
    inline_options: InlineElemOptions,
) -> impl Serialize + 'a {
    MdSerde::new(&elems, &ctx, inline_options)
}
