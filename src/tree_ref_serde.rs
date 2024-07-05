use crate::tree_ref::MdElemRef;
use serde::ser::SerializeSeq;
use serde::{Serialize, Serializer};

pub struct MdElemsStream<'md, 's> {
    elems: &'s Vec<MdElemRef<'md>>,
}

impl<'md, 's> From<&'s Vec<MdElemRef<'md>>> for MdElemsStream<'md, 's> {
    fn from(elems: &'s Vec<MdElemRef<'md>>) -> Self {
        Self { elems }
    }
}

impl<'md, 's> Serialize for MdElemsStream<'md, 's> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        todo
        // TODO: create Inlines holder, use it to serialize something of form:
        // {
        //     items: <vec>,
        //     references: <vec>
        // }
        // // or maybe:
        // {
        //     items: []items
        // }
        // where:
        //     item: {
        //         item: <contents>
        //         references: []refs
        //     }
    }
}
