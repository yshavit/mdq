use std::collections::BTreeSet;

pub(crate) struct NumberAssigner {
    next_int: u64,
    reserved_nums: BTreeSet<u64>,
}

impl NumberAssigner {
    pub(crate) fn new(reserved_nums: BTreeSet<u64>) -> Self {
        NumberAssigner {
            next_int: 1,
            reserved_nums,
        }
    }

    pub(crate) fn next_num(&mut self) -> u64 {
        let mut proposed_result = self.next_int;
        // This isn't the most efficient algorithm possible, but it's good enough, and very simple.
        while self.reserved_nums.contains(&proposed_result) {
            proposed_result += 1;
        }
        self.next_int = proposed_result + 1;
        proposed_result
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nothing_reserved() {
        check([], [1, 2, 3, 4]);
    }

    #[test]
    fn first_is_reserved() {
        check([1], [2, 3]);
    }

    #[test]
    fn just_one_reserved() {
        check([3], [1, 2, 4]);
    }

    #[test]
    fn multiple_in_range_reserved() {
        check([3, 4, 5], [1, 2, 6]);
    }

    /// Reserves some numbers, and then checks that the next `E` are what we expect.
    fn check<const R: usize, const E: usize>(reserved: [u64; R], expect: [u64; E]) {
        let mut assigner = NumberAssigner::new(reserved.into());

        let actual = [(); E].map(|_| Some(assigner.next_num()));
        assert_eq!(actual, expect.map(Some));
    }
}
