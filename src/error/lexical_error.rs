use std::cell::RefCell;

use crate::source::{SourceFile, Span};

#[derive(Debug)]
pub struct LexicalError<'a> {
    pub error: LexicalErrorKind,
    pub span: Span,
    pub source: &'a RefCell<SourceFile<'a>>,
}

impl<'a> LexicalError<'a> {
    pub fn unrecognized_token(span: Span, source: &'a RefCell<SourceFile<'a>>) -> Self {
        Self {
            error: LexicalErrorKind::UnrecognizedToken,
            span,
            source,
        }
    }

    pub fn unbalanced_quote(span: Span, source: &'a RefCell<SourceFile<'a>>) -> Self {
        Self {
            error: LexicalErrorKind::UnbalancedQuote,
            span,
            source,
        }
    }
}

impl<'a> std::error::Error for LexicalError<'a> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(&self.error)
    }
}

impl<'a> std::fmt::Display for LexicalError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let sf = self.source.borrow();
        let line = sf.line_str(self.span.start.line).trim_end();
        let span_length = self.span.end.column - self.span.start.column;
        let hint = "^".repeat(span_length.max(1));
        let hint_padding = " ".repeat(self.span.start.column);

        write!(
            f,
            "{} at line {}, column {}:\n{}\n{}{}",
            self.error, self.span.start.line, self.span.start.column, line, hint_padding, hint,
        )
    }
}

#[derive(Debug)]
pub enum LexicalErrorKind {
    UnrecognizedToken,
    UnbalancedQuote,
}

impl std::error::Error for LexicalErrorKind {}

impl std::fmt::Display for LexicalErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::UnrecognizedToken => f.write_str("Unrecognized token"),
            Self::UnbalancedQuote => f.write_str("Unbalanced quote"),
        }
    }
}
