use pest::Span;
use std::fmt::{Display, Formatter};

/// An error representing an invalid selector query.
///
/// <div class="warning">
/// This struct's <code>source()</code> is not part of the public contract, and may change at any time without that change being
/// marked as a breaking change.
/// </div>
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct ParseError {
    pub(crate) inner: InnerParseError,
}

impl ParseError {
    /// Creates a new ParseError from an [InnerParseError].
    ///
    /// This is intentionally not a [From] impl, because we want to keep it `pub(crate)`.
    pub(crate) fn new(inner: InnerParseError) -> Self {
        Self { inner }
    }
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.inner, f)
    }
}

impl std::error::Error for ParseError {
    /// This method gets the error's source, if available. **Not part of the public API contract.**
    ///
    /// Please see the warning on [this struct's main documentation](ParseError).
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.inner.source()
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) enum InnerParseError {
    Pest(crate::query::Error),
    Other(DetachedSpan, String),
}

impl Display for InnerParseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match &self {
            InnerParseError::Pest(error) => Display::fmt(error, f),
            InnerParseError::Other(_, message) => Display::fmt(message, f),
        }
    }
}

impl std::error::Error for InnerParseError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        match self {
            InnerParseError::Pest(err) => Some(err),
            InnerParseError::Other(_, _) => None,
        }
    }
}

impl ParseError {
    /// Gets a string suitable for displaying to a user, given the original query string.
    ///
    /// ```
    /// use mdq::select::Selector;
    /// let query_text = "$ ! invalid query string ! $";
    /// let parse_error = Selector::try_from(query_text).expect_err("expected an error");
    /// let expected_error = r" --> 1:1
    ///   |
    /// 1 | $ ! invalid query string ! $
    ///   | ^---
    ///   |
    ///   = expected valid query";
    /// assert_eq!(parse_error.to_string(query_text), expected_error);
    /// ```
    pub fn to_string(&self, query_text: &str) -> String {
        match &self.inner {
            InnerParseError::Pest(e) => format!("{e}"),
            InnerParseError::Other(span, message) => match Span::new(query_text, span.start, span.end) {
                None => message.to_string(),
                Some(span) => {
                    let pest_err = crate::query::Error::new_from_span(span, message.to_string());
                    pest_err.to_string()
                }
            },
        }
    }
}

impl From<crate::query::Error> for InnerParseError {
    fn from(err: crate::query::Error) -> Self {
        Self::Pest(err)
    }
}

/// Like a [pest::Span], but without a reference to the underlying `&str`, and thus cheaply Copyable.
#[derive(Copy, Clone, Default, Debug, PartialEq, Eq, Hash)]
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
