use std::cell::RefCell;

use insta;
use nzoth::{lexer::Lexer, source::SourceFile, token::Token};

struct PrintableToken<'a> {
    token: Token,
    source: &'a RefCell<SourceFile<'a>>,
}

impl<'a> PrintableToken<'a> {
    fn new(token: Token, source: &'a RefCell<SourceFile<'a>>) -> Self {
        Self { token, source }
    }
}

impl<'a> std::fmt::Display for PrintableToken<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Token(kind: \"{}\", literal: \"{}\")",
            self.token.kind,
            self.source.borrow().span_text(&self.token.span)
        )
    }
}

struct PrintableTokens<'a>(Vec<PrintableToken<'a>>);

impl<'a> std::fmt::Display for PrintableTokens<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.0.iter();
        f.write_str("[\n")?;
        while let Some(token) = iter.next() {
            write!(f, "  {},\n", token)?;
        }
        f.write_str("]")
    }
}

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

        insta::assert_snapshot!(PrintableTokens(
            tokens
                .into_iter()
                .map(|t| PrintableToken::new(t, &source))
                .collect::<Vec<_>>()
        ));
    })
}
