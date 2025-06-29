mod helpers;
use std::cell::RefCell;

use helpers::{SnapshotToken, SnapshotTokens};
use insta;
use nzoth::{lexer::Lexer, source::SourceFile};

#[test]
fn test_individual_token_kind() {
    insta::glob!("resources/lexer_cases/all_valid*.nz", |path| {
        let contents = std::fs::read_to_string(path).expect("Expect file to exist");
        let source = RefCell::new(SourceFile::new(
            0,
            path.file_name()
                .expect("Not a directory")
                .to_str()
                .expect("Converts to str")
                .to_string(),
            contents.as_str(),
        ));
        let mut lexer = Lexer::new(&source);
        let mut tokens = vec![];

        while let Some(token) = lexer.next() {
            tokens.push(token);
        }

        insta::assert_snapshot!(SnapshotTokens(
            tokens
                .into_iter()
                .map(|t| SnapshotToken::new(t, &source))
                .collect::<Vec<_>>()
        ));
    })
}

#[test]
fn test_parentheses() {
    insta::glob!("resources/lexer_cases/parentheses.nz", |path| {
        let contents = std::fs::read_to_string(path).expect("Expect file to exist");
        let source = RefCell::new(SourceFile::new(
            0,
            path.file_name()
                .expect("Not a directory")
                .to_str()
                .expect("Converts to str")
                .to_string(),
            contents.as_str(),
        ));
        let mut lexer = Lexer::new(&source);
        let mut tokens = vec![];

        while let Some(token) = lexer.next() {
            tokens.push(token);
        }

        insta::assert_snapshot!(SnapshotTokens(
            tokens
                .into_iter()
                .map(|t| SnapshotToken::new(t, &source))
                .collect::<Vec<_>>()
        ));
    })
}
