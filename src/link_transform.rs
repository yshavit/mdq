use crate::fmt_md::LinkLabel;
use crate::fmt_str::inlines_to_plain_string;
use crate::tree::{Inline, LinkDefinition, LinkReference, MdElem};
use crate::tree_visitor::{traverse_all, MutTreeVisitor};
use clap::ValueEnum;
use std::collections::hash_map::Entry;
use std::collections::HashMap;

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, ValueEnum)]
pub enum LinkTransform {
    /// Keep links as they were in the original
    Keep,

    /// Turn all links into inlined form: `[link text](https://example.com)`
    Inline,

    /// Turn all links into reference form: `[link text][1]`
    ///
    /// Links that weren't already in reference form will be auto-assigned a reference id. Links that were in reference
    /// form will have the link number be reordered.
    Reference,
}

impl LinkTransform {
    pub fn transform(&self, elems: &mut Vec<MdElem>) {
        match self {
            LinkTransform::Keep => {}
            LinkTransform::Inline => {
                for_each_link(elems, |link, _| link.reference = LinkReference::Inline);
            }
            LinkTransform::Reference => {
                let mut assigner = ReferenceAssigner::new();
                for_each_link(elems, |link, label| assigner.assign(link, label));
            }
        }
    }
}

struct ReferenceAssigner {
    /// Let's not worry about overflow. The minimum size for each link is 5 bytes (`[][1]`), so u64 of those is about
    ///  80 exabytes of markdown -- and it would be even bigger than that, since the digits get bigger. It's just not a
    /// case I'm too worried about right now.
    next_index: u64,
    /// Mappings from old reference id to new. We store these as an Strings (not ints) so that we don't need to worry
    /// about overflow.
    reorderings: HashMap<String, u64>,
}

impl ReferenceAssigner {
    fn new() -> Self {
        Self {
            next_index: 1,
            reorderings: HashMap::with_capacity(16), // arbitrary
        }
    }

    fn assign(&mut self, link: &mut LinkDefinition, label: LinkLabel) {
        let replacement = match &link.reference {
            LinkReference::Inline => Some(self.assign_new()),
            LinkReference::Full(prev) => self.maybe_assign(prev),
            LinkReference::Collapsed | LinkReference::Shortcut => match label {
                LinkLabel::Identifier(text) => self.maybe_assign(text),
                LinkLabel::Inline(text) => self.maybe_assign(&inlines_to_plain_string(text)),
            },
        };
        if let Some(replacement) = replacement {
            link.reference = replacement;
        }
    }

    fn maybe_assign(&mut self, prev: &String) -> Option<LinkReference> {
        if prev.chars().all(|ch| ch.is_numeric()) {
            match self.reorderings.entry(prev.clone()) {
                Entry::Occupied(map_to) => Some(LinkReference::Full(map_to.get().to_string())),
                Entry::Vacant(e) => {
                    e.insert(self.next_index);
                    Some(self.assign_new())
                }
            }
        } else {
            None
        }
    }

    fn assign_new(&mut self) -> LinkReference {
        let idx_str = self.next_index.to_string();
        self.next_index += 1;
        LinkReference::Full(idx_str)
    }
}

struct LinkFinder<F>(F)
where
    F: FnMut(&mut LinkDefinition, LinkLabel);

fn for_each_link<F>(elems: &mut Vec<MdElem>, action: F)
where
    F: FnMut(&mut LinkDefinition, LinkLabel),
{
    traverse_all(elems, &mut LinkFinder(action))
}

impl<F> MutTreeVisitor for LinkFinder<F>
where
    F: FnMut(&mut LinkDefinition, LinkLabel),
{
    fn visit(&mut self, elem: &mut MdElem) {
        match elem {
            MdElem::Inline(Inline::Link(link)) => {
                self.0(&mut link.link_definition, LinkLabel::Inline(&link.text));
            }
            MdElem::Inline(Inline::Image(image)) => {
                self.0(&mut image.link, LinkLabel::Identifier(&image.alt));
            }
            _ => {}
        }
    }
}
