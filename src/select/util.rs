use crate::parsing_iter::ParsingIterator;
use crate::select::{ParseErrorReason, ParseResult};

pub fn require_whitespace(chars: &mut ParsingIterator, description: &str) -> ParseResult<()> {
    if chars.drop_while(|ch| ch.is_whitespace()).is_empty() && chars.peek().is_some() {
        return Err(ParseErrorReason::InvalidSyntax(format!(
            "{} must be followed by whitespace",
            description
        )));
    } else {
        Ok(())
    }
}
