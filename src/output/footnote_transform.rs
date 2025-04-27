use crate::util::output::{Output, SimpleWrite};
use std::collections::HashMap;

pub struct FootnoteTransformer<'md> {
    mappings: Option<HashMap<&'md str, usize>>,
}

pub struct FootnoteTransformerToString<'a, 'md> {
    transformer: &'a mut FootnoteTransformer<'md>,
    scratch: Output<String>,
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

    pub fn new_to_stringer<'a>(&'a mut self) -> FootnoteTransformerToString<'a, 'md> {
        FootnoteTransformerToString::new(self)
    }
}

impl<'a, 'md> FootnoteTransformerToString<'a, 'md> {
    pub fn transform(&mut self, label: &'md str) -> String {
        let len = self.transformed_label_len(label);
        _ = self.scratch.replace_underlying(String::with_capacity(len)).unwrap();
        self.transformer.write(&mut self.scratch, label);
        self.scratch.take_underlying().unwrap()
    }

    fn new(transformer: &'a mut FootnoteTransformer<'md>) -> Self {
        Self {
            transformer,
            scratch: Output::without_text_wrapping(String::new()),
        }
    }

    fn transformed_label_len(&mut self, label: &str) -> usize {
        match &mut self.transformer.mappings {
            None => label.len(),
            Some(mapping) => {
                let renumbered_to = mapping.get(label).copied().unwrap_or(mapping.len() + 1);
                let renumbered_log10 = renumbered_to.checked_ilog10().unwrap_or(0);
                // Try to convert the u32 to usize; if we can't, just guess a length of 3.
                // That should be plenty!
                usize::try_from(renumbered_log10 + 1).unwrap_or(3)
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::output::footnote_transform::FootnoteTransformer;
    use crate::util::output::Output;

    #[test]
    fn inactive() {
        let mut transformer = FootnoteTransformer::new(false);
        check("abc", &mut transformer, "abc", 3);
        check("1", &mut transformer, "1", 1);
        check("3", &mut transformer, "3", 1);

        // remember the old value
        check("1", &mut transformer, "1", 1);
    }

    #[test]
    fn active() {
        let mut transformer = FootnoteTransformer::new(true);
        check("abc", &mut transformer, "1", 1);
        check("1", &mut transformer, "2", 1);
        check("3", &mut transformer, "3", 1);

        // remember the old value
        check("1", &mut transformer, "2", 1);
    }

    #[test]
    fn active_with_ten_footnotes() {
        let mut transformer = FootnoteTransformer::new(true);

        // write nine labels; we don't care about the results
        let nine_labels: Vec<_> = (1..10).map(|i| format!("footnote-{i}")).collect();
        for label in &nine_labels {
            transformer.write(&mut Output::without_text_wrapping(String::new()), label);
        }

        // the tenth label should remap to "10" with an expected len of 2
        check("z", &mut transformer, "10", 2);
    }

    fn check<'a>(
        input: &'a str,
        transformer: &mut FootnoteTransformer<'a>,
        expect: &str,
        expect_transformed_len: usize,
    ) {
        // len-calculation should work before and after we first officially see the label. So, try
        // this once before transformer.write, and then later we'll try it again.
        assert_eq!(
            transformer.new_to_stringer().transformed_label_len(input),
            expect_transformed_len
        );

        let mut output = Output::without_text_wrapping(String::with_capacity(expect.len()));
        transformer.write(&mut output, input);
        let actual = output.take_underlying().unwrap();
        assert_eq!(&actual, expect);

        assert_eq!(
            transformer.new_to_stringer().transformed_label_len(input),
            expect_transformed_len
        );
    }
}
