use crate::md_elem::{MdContext, MdElem};
use crate::select::{Result, Select, Selector, SelectorAdapter, TrySelector};

#[derive(Debug)]
pub struct ChainSelector {
    chain: Vec<SelectorAdapter>,
}

impl From<Vec<Selector>> for ChainSelector {
    fn from(chain: Vec<Selector>) -> Self {
        Self {
            chain: chain.into_iter().map(|s| s.into()).collect(),
        }
    }
}

impl TrySelector<Vec<MdElem>> for ChainSelector {
    fn try_select(&self, ctx: &MdContext, mut items: Vec<MdElem>) -> Result<Select> {
        if self.chain.is_empty() {
            // This is a bit of a hack: an empty chain is really a noop, and in this case we assume that the items
            // aren't actually a stream, but are actually an MdDoc that has been deconstructed into the Vec<MdElem>.
            // So, just reconstruct it back.
            return Ok(Select::Hit(vec![MdElem::Doc(items)]));
        }
        for adapter in &self.chain {
            items = adapter.find_nodes(ctx, items)?;
        }
        Ok(Select::Hit(items))
    }
}
