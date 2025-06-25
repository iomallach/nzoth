mod helpers;
use helpers::{SnapshotErrors, create_source_file};
use nzoth::{checker::Checker, parser::Parser};

fn test_valid_cases(glob_pattern: &str) {
    insta::glob!(glob_pattern, |path| {
        let contents = std::fs::read_to_string(path).expect("Expect path exists");
        let source = create_source_file(path, contents.as_str());

        let mut parser = Parser::new(&source);
        let program = parser.parse();
        assert_eq!(parser.errors().len(), 0);

        let mut checker = Checker::new(&source);
        checker.typecheck_program(program);
        let checked_program = checker.comp_unit;

        let output = format!("{:#?}", checked_program);
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
        let program = parser.parse();

        let mut checker = Checker::new(&source);
        checker.typecheck_program(program);

        insta::with_settings!({
            omit_expression => true,
            input_file => path,
            snapshot_suffix => path.file_name().unwrap().to_str().unwrap(),
        }, {
            insta::assert_snapshot!(SnapshotErrors(&checker.errors));
        });
    })
}

#[test]
fn test_valid_let_declarations() {
    test_valid_cases("resources/checker_cases/valid_let_declarations.nz");
}

#[test]
fn test_invalid_let_declarations() {
    test_invalid_cases("resources/checker_cases/invalid_let_declarations.nz");
}

#[test]
fn test_invalid_expressions() {
    test_invalid_cases("resources/checker_cases/invalid_expressions.nz");
}
