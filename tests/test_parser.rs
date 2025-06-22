mod helpers;
use std::{cell::RefCell, path::Path};

use helpers::SnapshotErrors;
use insta;
use nzoth::{parser::Parser, source::SourceFile};

fn create_source_file<'a>(path: &'a Path, contents: &'a str) -> RefCell<SourceFile<'a>> {
    RefCell::new(SourceFile::new(
        0,
        path.file_name()
            .expect("Expect a file")
            .to_str()
            .expect("No fail")
            .to_string(),
        contents,
    ))
}

fn test_valid_cases(glob_pattern: &str) {
    insta::glob!(glob_pattern, |path| {
        let contents = std::fs::read_to_string(path).expect("Expect path exists");
        let source = create_source_file(path, contents.as_str());

        let mut parser = Parser::new(&source);
        let program = parser.parse();
        assert_eq!(parser.errors().len(), 0);

        let output = format!("{:#?}", program);
        insta::with_settings!({
            omit_expression => true,
            input_file => path,
            snapshot_suffix => path.file_name().unwrap().to_str().unwrap(),
        }, {
            insta::assert_snapshot!(output);
        });
    })
}

fn test_invalid_cases(glob_pattern: &str) {
    insta::glob!(glob_pattern, |path| {
        let contents = std::fs::read_to_string(path).expect("Expect path exists");
        let source = create_source_file(path, contents.as_str());

        let mut parser = Parser::new(&source);
        _ = parser.parse();

        insta::with_settings!({
            omit_expression => true,
            input_file => path,
            snapshot_suffix => path.file_name().unwrap().to_str().unwrap(),
        }, {
            insta::assert_snapshot!(SnapshotErrors(parser.errors()));
        });
    })
}

#[test]
fn test_valid_let_declarations() {
    test_valid_cases("resources/parser_cases/valid_let_declarations.nz");
}

#[test]
fn test_valid_identifier() {
    test_valid_cases("resources/parser_cases/valid_identifiers.nz");
}

#[test]
fn test_valid_assignments() {
    test_valid_cases("resources/parser_cases/valid_assignments.nz");
}

#[test]
fn test_invalid_assignments() {
    test_invalid_cases("resources/parser_cases/invalid_assignments.nz");
}

#[test]
fn test_valid_func_declarations() {
    test_valid_cases("resources/parser_cases/valid_func_declarations.nz");
}

#[test]
fn test_invalid_func_declarations() {
    test_invalid_cases("resources/parser_cases/invalid_func_declarations.nz");
}

#[test]
fn test_valid_prefix_expressions() {
    test_valid_cases("resources/parser_cases/valid_prefix_expressions.nz");
}

#[test]
fn test_valid_boolean_expressions() {
    test_valid_cases("resources/parser_cases/valid_boolean_expressions.nz");
}

#[test]
fn test_valid_infix_expressions() {
    test_valid_cases("resources/parser_cases/valid_infix_expressions.nz");
}
