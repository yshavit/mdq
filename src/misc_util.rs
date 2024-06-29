// TODO rm if unneeded
pub struct Holder<T> {
    elems: Vec<T>,
}

impl<T> Holder<T> {
    pub fn with_capacity(size: usize) -> Self {
        Self {
            elems: Vec::with_capacity(size),
        }
    }

    pub fn store(&mut self, elem: T) -> &T {
        self.elems.push(elem);
        &self.elems[self.elems.len() - 1]
    }
}
