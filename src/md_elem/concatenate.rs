pub trait Concatenate: Sized {
    fn try_concatenate(&mut self, other: Self) -> Result<(), Self>;

    fn concatenate_similar(items: Vec<Self>) -> Vec<Self> {
        let mut results = Vec::with_capacity(items.len());
        let mut iter = items.into_iter();
        let Some(first) = iter.next() else {
            return results;
        };
        results.push(first);
        let mut prev = results.last_mut().expect("can't be empty immediately after push");

        for curr in iter {
            match prev.try_concatenate(curr) {
                Ok(()) => {}
                Err(new_segment) => {
                    results.push(new_segment);
                    prev = results.last_mut().expect("can't be empty immediately after push");
                }
            }
        }
        results
    }
}
