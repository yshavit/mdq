pub struct IndexRemover {
    /// invariant: must be ordered ascending
    indices_to_keep_ordered_asc: Vec<usize>,
}

impl IndexRemover {
    pub fn for_items<I, F>(items: &[I], mut allow_filter: F) -> Self
    where
        F: FnMut(usize, &I) -> bool,
    {
        let mut indices_to_keep_ordered_asc = Vec::with_capacity(items.len());
        for (idx, item) in items.iter().enumerate() {
            if allow_filter(idx, item) {
                indices_to_keep_ordered_asc.push(idx);
            }
        }
        Self {
            indices_to_keep_ordered_asc,
        }
    }

    /// Returns an `FnMut` suitable for use in [ItemRetainer::retain_with_index].
    pub fn retain_fn<I>(&self) -> impl FnMut(usize, &I) -> bool + '_ {
        let mut next_to_keep = self.indices_to_keep_ordered_asc.iter().peekable();
        move |target, _| {
            while let Some(&&value) = next_to_keep.peek() {
                if value == target {
                    let _ = next_to_keep.next();
                    return true;
                }
                if value > target {
                    return false;
                }
            }
            false
        }
    }

    pub fn apply<I>(&self, items: &mut Vec<I>) {
        items.retain_with_index(self.retain_fn());
    }

    pub fn count_keeps(&self) -> usize {
        self.indices_to_keep_ordered_asc.len()
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
    fn retain_with_index<F>(&mut self, mut f: F)
    where
        F: FnMut(usize, &I) -> bool,
    {
        // A simple algorithm, which is O(n) in both space and time.
        // I feel like there's an algorithm out there that's O(n) in time and O(1) in space, but this is good enough,
        // and it's nice and simple.
        let mut scratch = Vec::with_capacity(self.len());
        for (idx, item) in self.drain(..).enumerate() {
            if f(idx, &item) {
                scratch.push(item);
            }
        }
        self.append(&mut scratch);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_remover() {
        let mut items = vec!['a', 'b', 'c', 'd'];
        IndexRemover {
            indices_to_keep_ordered_asc: [].into(),
        }
        .apply(&mut items);
        assert_eq!(items, vec![]);
    }

    #[test]
    fn remover_has_bigger_indexes_than_items() {
        let mut items = vec!['a', 'b', 'c', 'd'];
        IndexRemover {
            indices_to_keep_ordered_asc: [0, 1, 2, 3, 4, 5, 6].into(),
        }
        .apply(&mut items);
        assert_eq!(items, vec!['a', 'b', 'c', 'd']);
    }

    #[test]
    fn keep_head() {
        let mut items = vec!['a', 'b', 'c', 'd'];
        IndexRemover {
            indices_to_keep_ordered_asc: [0].into(),
        }
        .apply(&mut items);
        assert_eq!(items, vec!['a']);
    }

    #[test]
    fn keep_middle() {
        let mut items = vec!['a', 'b', 'c', 'd'];
        IndexRemover {
            indices_to_keep_ordered_asc: [2].into(),
        }
        .apply(&mut items);
        assert_eq!(items, vec!['c']);
    }

    #[test]
    fn keep_tail() {
        let mut items = vec!['a', 'b', 'c', 'd'];
        IndexRemover {
            indices_to_keep_ordered_asc: [items.len() - 1].into(),
        }
        .apply(&mut items);
        assert_eq!(items, vec!['d']);
    }
}
