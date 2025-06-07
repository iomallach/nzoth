mod helpers;
use std::cell::RefCell;

use helpers::SnapshotErrors;
use insta;
use nzoth::{parser::Parser, source::SourceFile};

#[test]
fn test_valid_let_declarations() {
    insta::glob!("resources/parser_cases/valid_let_declarations.nz", |path| {
        let contents = std::fs::read_to_string(path).expect("Expect path exists");
        let source = RefCell::new(SourceFile::new(
            0,
            path.file_name()
                .expect("Expect a file")
                .to_str()
                .expect("No fail")
                .to_string(),
            contents.as_str(),
        ));

        let mut parser = Parser::new(&source);
        let program = parser.parse();
        assert_eq!(2, program.nodes.len());

        let output = format!("{:#?}", program);

        insta::assert_snapshot!(output);
    })
}

#[test]
fn test_valid_identifier() {
    insta::glob!("resources/parser_cases/valid_identifiers.nz", |path| {
        let contents = std::fs::read_to_string(path).expect("Expect path exists");
        let source = RefCell::new(SourceFile::new(
            0,
            path.file_name()
                .expect("Expect a file")
                .to_str()
                .expect("No fail")
                .to_string(),
            contents.as_str(),
        ));

        let mut parser = Parser::new(&source);
        let program = parser.parse();

        let output = format!("{:#?}", program);
        insta::assert_snapshot!(output);
    })
}

#[test]
fn test_valid_assignments() {
    insta::glob!("resources/parser_cases/valid_assignments.nz", |path| {
        let contents = std::fs::read_to_string(path).expect("Expect path exists");
        let source = RefCell::new(SourceFile::new(
            0,
            path.file_name()
                .expect("Expect a file")
                .to_str()
                .expect("No fail")
                .to_string(),
            contents.as_str(),
        ));

        let mut parser = Parser::new(&source);
        let program = parser.parse();

        let output = format!("{:#?}", program);
        insta::assert_snapshot!(output);
    })
}

#[test]
fn test_invalid_assignments() {
    insta::glob!("resources/parser_cases/invalid_assignments.nz", |path| {
        let contents = std::fs::read_to_string(path).expect("Expect path exists");
        let source = RefCell::new(SourceFile::new(
            0,
            path.file_name()
                .expect("Expect a file")
                .to_str()
                .expect("No fail")
                .to_string(),
            contents.as_str(),
        ));

        let mut parser = Parser::new(&source);
        _ = parser.parse();

        insta::assert_snapshot!(SnapshotErrors(parser.errors()));
    })
}
