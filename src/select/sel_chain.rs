use crate::md_elem::MdElem;
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
    fn try_select(&self, item: Vec<MdElem>) -> Result<MdElem, MdElem> {
        todo!("there isn't really any good implementation here")
    }
}
