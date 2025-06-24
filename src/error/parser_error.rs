use std::cell::RefCell;

use crate::{
    source::{SourceFile, Span},
    token::TokenKind,
};

#[derive(Debug)]
pub struct ParserError<'a> {
    pub error: ParserErrorKind,
    pub span: Span,
    pub source: &'a RefCell<SourceFile<'a>>,
}

impl<'a> ParserError<'a> {
    pub fn expected_token(
        expected: TokenKind,
        found: TokenKind,
        span: Span,
        source: &'a RefCell<SourceFile<'a>>,
    ) -> Self {
        Self {
            error: ParserErrorKind::ExpectedToken { expected, found },
            span,
            source,
        }
    }

    pub fn unexpected_end_of_file(source: &'a RefCell<SourceFile<'a>>) -> Self {
        Self {
            error: ParserErrorKind::UnexpectedEndOfFile,
            span: source.borrow().end_of_file(),
            source,
        }
    }

    pub fn unknow_operator_in_expression(
        operator: TokenKind,
        span: Span,
        source: &'a RefCell<SourceFile<'a>>,
    ) -> Self {
        Self {
            error: ParserErrorKind::UnknownOperatorInExpression(operator),
            span,
            source,
        }
    }

    pub fn unknown_symbol_in_let_declaration(
        span: Span,
        source: &'a RefCell<SourceFile<'a>>,
    ) -> Self {
        Self {
            error: ParserErrorKind::UnknownSymbolInLetDeclaration(
                source.borrow().span_text(&span).to_string(),
            ),
            span,
            source,
        }
    }

    pub fn expected_top_level_item(
        found: TokenKind,
        span: Span,
        source: &'a RefCell<SourceFile<'a>>,
    ) -> Self {
        Self {
            error: ParserErrorKind::ExpectedTopLevelItem { found },
            span,
            source,
        }
    }

    pub fn expected_semicolon(span: Span, source: &'a RefCell<SourceFile<'a>>) -> Self {
        Self {
            error: ParserErrorKind::ExpectedSemicolon,
            span,
            source,
        }
    }

    pub fn expected_type(found: String, span: Span, source: &'a RefCell<SourceFile<'a>>) -> Self {
        Self {
            error: ParserErrorKind::ExpectedType { found },
            span,
            source,
        }
    }
}

impl<'a> std::error::Error for ParserError<'a> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        Some(&self.error)
    }
}

impl<'a> std::fmt::Display for ParserError<'a> {
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

#[derive(Debug, PartialEq, Eq)]
pub enum ParserErrorKind {
    ExpectedToken {
        expected: TokenKind,
        found: TokenKind,
    },
    UnknownOperatorInExpression(TokenKind),
    UnexpectedEndOfFile,
    UnknownSymbolInLetDeclaration(String),
    ExpectedTopLevelItem {
        found: TokenKind,
    },
    ExpectedSemicolon,
    ExpectedType {
        found: String,
    },
}

impl std::error::Error for ParserErrorKind {}

impl std::fmt::Display for ParserErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ExpectedToken { expected, found } => {
                write!(f, "Expected {expected}, found {found}")
            }
            Self::UnexpectedEndOfFile => write!(f, "Unexpected end of file"),
            Self::UnknownOperatorInExpression(tk) => {
                write!(f, "Unexpected operator `{tk} in expression`")
            }
            Self::UnknownSymbolInLetDeclaration(s) => {
                write!(f, "Unknown symbol `{s}` in let declaration")
            }
            Self::ExpectedTopLevelItem { found } => {
                write!(f, "Expected a top level item, found {found} ")
            }
            Self::ExpectedSemicolon => {
                write!(f, "Expected semicolon")
            }
            Self::ExpectedType { found } => {
                write!(f, "Expected type, found {found}")
            }
        }
    }
}
