mod fmt_md;
mod fmt_md_inlines;
pub mod fmt_plain_inline;
mod fmt_plain_str;
mod fmt_plain_writer;
mod footnote_transform;
mod link_transform;
mod tree_ref_serde;

pub mod md {
    pub use crate::output::fmt_md::*;
    pub use crate::output::fmt_md_inlines::*;
    pub use crate::output::link_transform::*;
}
pub mod plain {
    pub use crate::output::fmt_plain_inline::*;
    pub use crate::output::fmt_plain_str::*;
}
pub mod serde {
    pub use crate::output::tree_ref_serde::*;
}
