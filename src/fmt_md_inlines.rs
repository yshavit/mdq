use crate::fmt_md::MdOptions;
use crate::link_transform::{LinkLabel, LinkTransformer};
use crate::output::{Output, SimpleWrite};
use crate::tree::{
    Footnote, Formatting, FormattingVariant, Inline, LinkDefinition, LinkReference, MdElem, Text, TextVariant,
};
use crate::tree_ref::MdElemRef;
use std::borrow::{Borrow, Cow};
use std::collections::{HashMap, HashSet};

pub struct MdInlinesWriter<'a> {
    seen_links: HashSet<LinkLabel<'a>>,
    seen_footnotes: HashSet<&'a String>,
    pending_references: PendingReferences<'a>,
    link_transformer: LinkTransformer,
}

pub struct PendingReferences<'a> {
    pub links: HashMap<LinkLabel<'a>, UrlAndTitle<'a>>,
    pub footnotes: HashMap<&'a String, &'a Vec<MdElem>>,
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub struct UrlAndTitle<'a> {
    pub url: &'a String,
    pub title: &'a Option<String>,
}

impl<'a> MdInlinesWriter<'a> {
    pub fn new(options: &MdOptions) -> Self {
        let pending_refs_capacity = 8; // arbitrary guess
        Self {
            seen_links: HashSet::with_capacity(pending_refs_capacity),
            seen_footnotes: HashSet::with_capacity(pending_refs_capacity),
            pending_references: PendingReferences {
                links: HashMap::with_capacity(pending_refs_capacity),
                footnotes: HashMap::with_capacity(pending_refs_capacity),
            },
            link_transformer: LinkTransformer::from(options.link_canonicalization),
        }
    }

    pub fn pending_references(&mut self) -> &mut PendingReferences<'a> {
        &mut self.pending_references
    }

    pub fn write_line<E, W>(&mut self, out: &mut Output<W>, elems: &'a [E])
    where
        E: Borrow<Inline>,
        W: SimpleWrite,
    {
        for elem in elems {
            self.write_inline_element(out, elem.borrow());
        }
    }

    pub fn write_inline_element<W>(&mut self, out: &mut Output<W>, elem: &'a Inline)
    where
        W: SimpleWrite,
    {
        match elem {
            Inline::Formatting(Formatting { variant, children }) => {
                let surround = match variant {
                    FormattingVariant::Delete => "~~",
                    FormattingVariant::Emphasis => "_",
                    FormattingVariant::Strong => "**",
                };
                out.write_str(surround);
                self.write_line(out, children);
                out.write_str(surround);
            }
            Inline::Text(Text { variant, value }) => {
                let surround = match variant {
                    TextVariant::Plain => "",
                    TextVariant::Code => "`",
                    TextVariant::Math => "$",
                    TextVariant::Html => "",
                };
                out.write_str(surround);
                out.write_str(value);
                out.write_str(surround);
            }
            Inline::Link(link) => self.write_one_md(out, MdElemRef::Link(link)),
            Inline::Image(image) => self.write_one_md(out, MdElemRef::Image(image)),
            Inline::Footnote(Footnote { label, text }) => {
                out.write_str("[^");
                out.write_str(label);
                out.write_char(']');
                if self.seen_footnotes.insert(label) {
                    self.pending_references.footnotes.insert(label, text);
                }
            }
        }
    }

    /// Writes the inline portion of the link, which may be the full link if it was originally inlined.
    ///
    /// Examples:
    ///
    /// ```
    /// [an inline link](https://example.com)
    /// [a referenced link][1]
    /// ```
    ///
    /// The `contents` function is what writes e.g. `an inline link` above. It's a function because it may be a recursive
    /// call into [write_line] (for links) or just simple text (for image alts).
    pub fn write_link_inline_portion<W>(&mut self, out: &mut Output<W>, label: LinkLabel<'a>, link: &'a LinkDefinition)
    where
        W: SimpleWrite,
    {
        out.write_char('[');
        match &label {
            LinkLabel::Text(text) => out.write_str(text),
            LinkLabel::Inline(text) => self.write_line(out, text),
        }
        out.write_char(']');

        let transformed = self.link_transformer.transform(&link.reference, &label);
        let link_ref_owned = transformed.into_owned();

        let reference_to_add = match link_ref_owned {
            LinkReference::Inline => {
                out.write_char('(');
                out.write_str(&link.url);
                self.write_url_title(out, &link.title);
                out.write_char(')');
                None
            }
            LinkReference::Full(identifier) => {
                out.write_char('[');
                out.write_str(&identifier);
                out.write_char(']');
                Some(LinkLabel::Text(Cow::from(identifier)))
            }
            LinkReference::Collapsed => {
                out.write_str("[]");
                Some(label)
            }
            LinkReference::Shortcut => Some(label),
        };

        if let Some(reference_label) = reference_to_add {
            if self.seen_links.insert(reference_label.clone()) {
                self.pending_references.links.insert(
                    reference_label,
                    UrlAndTitle {
                        url: &link.url,
                        title: &link.title,
                    },
                );
                // else warn?
            }
        }
    }
}
