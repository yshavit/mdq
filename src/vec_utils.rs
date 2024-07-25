pub struct IndexRemover {
    /// must be ordered!
    to_remove_order_asc: Vec<usize>,
}

impl IndexRemover {
    pub fn for_items<I, F>(items: &[I], allow_filter: F) -> Self
    where
        F: Fn(usize, &I) -> bool,
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
