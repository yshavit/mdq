use crate::tree_ref::MdElemRef;
use serde::{Serialize, Serializer};

impl<'a> Serialize for MdElemRef<'a> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str("testing")
    }
}
