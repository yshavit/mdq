use std::collections::BTreeSet;

pub(crate) struct IndexKeeper {
    indices_to_keep: BTreeSet<usize>,
}

impl IndexKeeper {
    pub(crate) fn new() -> Self {
        Self {
            indices_to_keep: BTreeSet::new(),
        }
    }

    pub(crate) fn retain_when<I, F, E>(&mut self, items: &[I], mut allow_filter: F) -> Result<(), E>
    where
        F: FnMut(usize, &I) -> Result<bool, E>,
    {
        for (idx, item) in items.iter().enumerate() {
            if allow_filter(idx, item)? {
                self.indices_to_keep.insert(idx);
            }
        }
        Ok(())
    }

    /// Returns an `FnMut` suitable for use in [ItemRetainer::retain_with_index].
    pub(crate) fn retain_fn<I, E>(&self) -> impl FnMut(usize, &I) -> Result<bool, E> + '_ {
        let mut next_to_keep = self.indices_to_keep.iter().peekable();
        move |target, _| {
            while let Some(&&value) = next_to_keep.peek() {
                if value == target {
                    let _ = next_to_keep.next();
                    return Ok(true);
                }
                if value > target {
                    return Ok(false);
                }
            }
            Ok(false)
        }
    }

    pub(crate) fn count_keeps(&self) -> usize {
        self.indices_to_keep.len()
    }
}

pub(crate) trait ItemRetainer<I> {
    /// Iterates over the items in order, invoking `f` on each and retaining only those elements for which it returns
    /// `true`.
    ///
    /// This is guaranteed to iterate over items sequentially, and filters can take advantage of that fact.
    fn retain_with_index<F, E>(&mut self, f: F) -> Result<(), E>
    where
        F: FnMut(usize, &I) -> Result<bool, E>;
}

impl<I> ItemRetainer<I> for Vec<I> {
    fn retain_with_index<F, E>(&mut self, mut f: F) -> Result<(), E>
    where
        F: FnMut(usize, &I) -> Result<bool, E>,
    {
        // A simple algorithm, which is O(n) in both space and time.
        // I feel like there's an algorithm out there that's O(n) in time and O(1) in space, but this is good enough,
        // and it's nice and simple.
        let mut scratch = Vec::with_capacity(self.len());
        for (idx, item) in self.drain(..).enumerate() {
            if f(idx, &item)? {
                scratch.push(item);
            }
        }
        self.append(&mut scratch);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_remover() {
        let mut items = vec!['a', 'b', 'c', 'd'];
        let remover: IndexKeeper = [].into();
        items.retain_with_index(remover.retain_fn::<_, ()>()).unwrap();
        assert_eq!(items, vec![]);
    }

    #[test]
    fn remover_has_bigger_indexes_than_items() {
        let mut items = vec!['a', 'b', 'c', 'd'];
        let remover: IndexKeeper = [0, 1, 2, 3, 4, 5, 6].into();
        items.retain_with_index(remover.retain_fn::<_, ()>()).unwrap();
        assert_eq!(items, vec!['a', 'b', 'c', 'd']);
    }

    #[test]
    fn keep_head() {
        let mut items = vec!['a', 'b', 'c', 'd'];
        let remover: IndexKeeper = [0].into();
        items.retain_with_index(remover.retain_fn::<_, ()>()).unwrap();
        assert_eq!(items, vec!['a']);
    }

    #[test]
    fn keep_middle() {
        let mut items = vec!['a', 'b', 'c', 'd'];
        let remover: IndexKeeper = [2].into();
        items.retain_with_index(remover.retain_fn::<_, ()>()).unwrap();
        assert_eq!(items, vec!['c']);
    }

    #[test]
    fn keep_tail() {
        let mut items = vec!['a', 'b', 'c', 'd'];
        let remover: IndexKeeper = [items.len() - 1].into();
        items.retain_with_index(remover.retain_fn::<_, ()>()).unwrap();
        assert_eq!(items, vec!['d']);
    }

    impl<const N: usize> From<[usize; N]> for IndexKeeper {
        fn from(indices: [usize; N]) -> Self {
            let mut result = Self::new();
            for idx in indices {
                result.indices_to_keep.insert(idx);
            }
            result
        }
    }
}
