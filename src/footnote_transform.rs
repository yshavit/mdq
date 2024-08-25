use crate::output::{Output, SimpleWrite};
use std::collections::HashMap;

pub struct FootnoteTransformer<'md> {
    mappings: Option<HashMap<&'md str, usize>>,
}

impl<'md> FootnoteTransformer<'md> {
    pub fn new(active: bool) -> Self {
        Self {
            mappings: if active { Some(HashMap::default()) } else { None },
        }
    }

    pub fn write<W>(&mut self, out: &mut Output<W>, label: &'md str)
    where
        W: SimpleWrite,
    {
        match &mut self.mappings {
            None => out.write_str(label),
            Some(mapping) => {
                let current_mapping_len = mapping.len();
                let num = mapping.entry(label).or_insert(current_mapping_len + 1);
                out.write_str(&num.to_string());
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::footnote_transform::FootnoteTransformer;
    use crate::output::Output;

    #[test]
    fn inactive() {
        let mut transformer = FootnoteTransformer::new(false);
        check("a", &mut transformer, "a");
        check("1", &mut transformer, "1");
        check("3", &mut transformer, "3");

        // remember the old value
        check("1", &mut transformer, "1");
    }

    #[test]
    fn active() {
        let mut transformer = FootnoteTransformer::new(true);
        check("a", &mut transformer, "1");
        check("1", &mut transformer, "2");
        check("3", &mut transformer, "3");

        // remember the old value
        check("1", &mut transformer, "2");
    }

    fn check<'a>(input: &'a str, transformer: &mut FootnoteTransformer<'a>, expect: &str) {
        let mut output = Output::new(String::with_capacity(expect.len()));
        transformer.write(&mut output, input);
        let actual = output.take_underlying().unwrap();
        assert_eq!(&actual, expect);
    }
}
