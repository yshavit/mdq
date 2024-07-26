use std::iter::Peekable;
use std::slice::Iter;

pub struct IndexRemover {
    /// invariant: must be ordered ascending
    to_keep_asc: Vec<usize>,
}

impl IndexRemover {
    pub fn for_items<I, F>(items: &[I], mut allow_filter: F) -> Self
    where
        F: FnMut(usize, &I) -> bool,
    {
        let mut to_keep_asc = Vec::with_capacity(items.len());
        for (idx, item) in items.iter().enumerate() {
            if allow_filter(idx, item) {
                to_keep_asc.push(idx);
            }
        }
        Self { to_keep_asc }
    }

    pub fn apply<I>(&self, items: &mut Vec<I>) {
        /// Returns whether the given `target` value is in the stream. The stream must be ordered, and the target
        /// must increase on each subsequent call.
        fn find_value(iter: &mut Peekable<Iter<usize>>, target: usize) -> bool {
            while let Some(&&value) = iter.peek() {
                if value == target {
                    let _ = iter.next();
                    return true;
                }
                if value > target {
                    return false;
                }
            }
            false
        }

        // A simple algorithm, which is O(n) in both space and time.
        // I feel like there's an algorithm out there that's O(n) in time and O(1) in space, but this is good enough,
        // and it's nice and simple.
        let mut scratch = Vec::with_capacity(items.len());
        let mut next_to_keep = self.to_keep_asc.iter().peekable();
        for (idx, item) in items.drain(..).enumerate() {
            if find_value(&mut next_to_keep, idx) {
                scratch.push(item);
            }
        }
        items.append(&mut scratch);
    }

    pub fn count_keeps(&self) -> usize {
        self.to_keep_asc.len()
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
        IndexRemover::for_items(self, f).apply(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_remover() {
        let mut items = vec!['a', 'b', 'c', 'd'];
        IndexRemover { to_keep_asc: [].into() }.apply(&mut items);
        assert_eq!(items, vec![]);
    }

    #[test]
    fn remover_has_bigger_indexes_than_items() {
        let mut items = vec!['a', 'b', 'c', 'd'];
        IndexRemover {
            to_keep_asc: [0, 1, 2, 3, 4, 5, 6].into(),
        }
        .apply(&mut items);
        assert_eq!(items, vec!['a', 'b', 'c', 'd']);
    }

    #[test]
    fn keep_head() {
        let mut items = vec!['a', 'b', 'c', 'd'];
        IndexRemover {
            to_keep_asc: [0].into(),
        }
        .apply(&mut items);
        assert_eq!(items, vec!['a']);
    }

    #[test]
    fn keep_middle() {
        let mut items = vec!['a', 'b', 'c', 'd'];
        IndexRemover {
            to_keep_asc: [2].into(),
        }
        .apply(&mut items);
        assert_eq!(items, vec!['c']);
    }

    #[test]
    fn keep_tail() {
        let mut items = vec!['a', 'b', 'c', 'd'];
        IndexRemover {
            to_keep_asc: [items.len() - 1].into(),
        }
        .apply(&mut items);
        assert_eq!(items, vec!['d']);
    }
}
