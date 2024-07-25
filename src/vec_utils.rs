pub struct IndexRemover {
    /// must be ordered!
    to_remove_order_asc: Vec<usize>,
}

// TODO actually, I should just rm this struct altogether. The one place I need it, I can just construct the Vec<usize>
// directly, and then invoke retrain_if_with_index
impl IndexRemover {
    pub fn for_items<I, F>(items: &[I], mut allow_filter: F) -> Self
    where
        F: FnMut(usize, &I) -> bool,
    {
        let mut indices = Vec::with_capacity(items.len());
        for (idx, item) in items.iter().enumerate() {
            if !allow_filter(idx, item) {
                indices.push(idx);
            }
        }
        Self {
            to_remove_order_asc: indices,
        }
    }

    pub fn apply<I>(&self, items: &mut Vec<I>) {
        // TODO: this is O(n^2): it loops N=rows times, and each row.remove is O(M=columns).
        // We could do this more efficiently by swapping the to-be-saved items in, and then truncating the rest.
        for to_rm in self.to_remove_order_asc.iter().rev() {
            items.remove(*to_rm);
        }
    }

    pub fn count_removals(&self) -> usize {
        self.to_remove_order_asc.len()
    }
}

pub trait ItemRetainer<I> {
    /// Iterates over the items in order, invoking `f` on each and retaining only those elements for which it returns
    /// `true`.
    ///
    /// This is guaranteed to iterate over items sequentially, and filters can take advantage of that fact.
    fn retain_with_index<F>(&mut self, f: F)
    where
        F: FnMut(usize, &I) -> bool;
}

impl<I> ItemRetainer<I> for Vec<I> {
    fn retain_with_index<F>(&mut self, f: F)
    where
        F: FnMut(usize, &I) -> bool,
    {
        // TODO we don't need the IndexRemover here! We can just do the swapping logic directly here.
        // In fact, IndexRemover::apply should invoke this method.

        // We want F to be mut because we can then have an efficient way of dealing with an ordered list of indexes:
        // we can peek at the next one (O(1)), and then if it matches then discard it via next() (also O(1)).

        let remover = IndexRemover::for_items(self, f);
        remover.apply(self)
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn todo() {
        todo!("add some tests!")
        // - remover is empty
        // - remover is lal
        // - remover is some items (but not all)
        // - remover has more indices than target vec (should silently ignore)
    }
}
