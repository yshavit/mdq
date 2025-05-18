use crate::query::{InnerParseError, Pair};
use crate::select::{MatchReplace, Matcher};

impl MatchReplace {
    pub(crate) fn try_from(pair: Option<Pair>) -> Result<Self, InnerParseError> {
        let matcher = Matcher::try_from(pair)?;
        Ok(MatchReplace {
            matcher,
            replacement: None,
        })
    }
}
