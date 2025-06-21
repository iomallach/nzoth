use std::cell::RefCell;

use nzoth::{error::CompilationError, source::SourceFile, token::Token};

pub struct SnapshotErrors<'a>(pub &'a Vec<CompilationError<'a>>);

impl<'a> std::fmt::Display for SnapshotErrors<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for error in self.0 {
            write!(f, "{error}\n")?;
        }
        Ok(())
    }
}

pub struct SnapshotToken<'a> {
    token: Token,
    source: &'a RefCell<SourceFile<'a>>,
}

impl<'a> SnapshotToken<'a> {
    pub fn new(token: Token, source: &'a RefCell<SourceFile<'a>>) -> Self {
        Self { token, source }
    }
}

impl<'a> std::fmt::Display for SnapshotToken<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Token(kind: \"{}\", literal: \"{}\")",
            self.token.kind,
            self.source.borrow().span_text(&self.token.span)
        )
    }
}

pub struct SnapshotTokens<'a>(pub Vec<SnapshotToken<'a>>);

impl<'a> std::fmt::Display for SnapshotTokens<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.0.iter();
        f.write_str("[\n")?;
        while let Some(token) = iter.next() {
            write!(f, "  {},\n", token)?;
        }
        f.write_str("]")
    }
}
