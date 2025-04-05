use pest::error::ErrorVariant;
use pest::Span;

#[derive(Debug, Clone, PartialEq)]
pub enum ParseError {
    Pest(crate::query::Error),
    Other(DetachedSpan, String),
}

impl ParseError {
    pub fn to_string(&self, query_text: &str) -> String {
        match self {
            ParseError::Pest(e) => format!("{e}"),
            ParseError::Other(span, message) => match Span::new(query_text, span.start, span.end) {
                None => format!("{message}"),
                Some(span) => {
                    let pest_err = crate::query::Error::new_from_span(
                        ErrorVariant::CustomError {
                            message: message.to_string(),
                        },
                        span,
                    );
                    pest_err.to_string()
                }
            },
        }
    }
}

impl From<crate::query::Error> for ParseError {
    fn from(err: crate::query::Error) -> Self {
        Self::Pest(err)
    }
}

/// Like a [pest::Span], but without a reference to the underlying `&str`, and thus cheaply Copyable.
#[derive(Copy, Clone, Eq, PartialEq, Debug, Default)]
pub struct DetachedSpan {
    pub start: usize,
    pub end: usize,
}

impl From<pest::Span<'_>> for DetachedSpan {
    fn from(value: pest::Span) -> Self {
        Self {
            start: value.start(),
            end: value.end(),
        }
    }
}

impl From<&crate::query::Pair<'_>> for DetachedSpan {
    fn from(value: &crate::query::Pair<'_>) -> Self {
        value.as_span().into()
    }
}
