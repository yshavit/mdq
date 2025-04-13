use crate::md_elem::{MdContext, MdElem};
use crate::select::{Selector, SelectorAdapter, TrySelector};

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
    fn try_select(&self, ctx: &MdContext, mut items: Vec<MdElem>) -> Result<MdElem, MdElem> {
        for adapter in &self.chain {
            items = adapter.find_nodes(ctx, items);
        }
        Ok(MdElem::Doc(items))
    }
}
