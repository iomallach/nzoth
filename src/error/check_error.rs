use std::cell::RefCell;

use crate::source::{SourceFile, Span};

#[derive(Debug)]
pub struct CheckError<'a> {
    pub error: CheckErrorKind,
    pub span: Span,
    pub source: &'a RefCell<SourceFile<'a>>,
}

impl<'a> CheckError<'a> {
    pub fn mismatched_types(
        expected: String,
        found: String,
        span: Span,
        source: &'a RefCell<SourceFile<'a>>,
    ) -> Self {
        Self {
            error: CheckErrorKind::MismatchedTypes { expected, found },
            span,
            source,
        }
    }

    pub fn unknown_type(name: String, span: Span, source: &'a RefCell<SourceFile<'a>>) -> Self {
        Self {
            error: CheckErrorKind::UnknownType { name },
            span,
            source,
        }
    }

    pub fn unbound_variable(name: String, span: Span, source: &'a RefCell<SourceFile<'a>>) -> Self {
        Self {
            error: CheckErrorKind::UnboundVariable { name },
            span,
            source,
        }
    }

    pub fn cannot_apply_bin_op(
        op: String,
        left: String,
        right: String,
        span: Span,
        source: &'a RefCell<SourceFile<'a>>,
    ) -> Self {
        Self {
            error: CheckErrorKind::CannotApplyBinOp { op, left, right },
            span,
            source,
        }
    }

    pub fn cannot_apply_un_op(
        op: String,
        right: String,
        span: Span,
        source: &'a RefCell<SourceFile<'a>>,
    ) -> Self {
        Self {
            error: CheckErrorKind::CannotApplyUnOp { op, right },
            span,
            source,
        }
    }
}

impl<'a> std::fmt::Display for CheckError<'a> {
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

impl<'a> std::error::Error for CheckError<'a> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(&self.error)
    }
}

#[derive(Debug)]
pub enum CheckErrorKind {
    MismatchedTypes {
        expected: String,
        found: String,
    },
    UnknownType {
        name: String,
    },
    UnboundVariable {
        name: String,
    },
    CannotApplyBinOp {
        op: String,
        left: String,
        right: String,
    },
    CannotApplyUnOp {
        op: String,
        right: String,
    },
}

impl std::fmt::Display for CheckErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::MismatchedTypes { expected, found } => {
                write!(f, "Mismatched types, expected {expected}, found {found}")
            }
            Self::UnknownType { name } => write!(f, "Can't find type {name} in current scope"),
            Self::UnboundVariable { name } => write!(f, "Can't find variable `{name}`"),
            Self::CannotApplyBinOp { op, left, right } => {
                write!(f, "Can't apply `{op}` to {left} and {right}")
            }
            Self::CannotApplyUnOp { op, right } => write!(f, "Can't apply `{op}` to {right}"),
        }
    }
}

impl std::error::Error for CheckErrorKind {}
