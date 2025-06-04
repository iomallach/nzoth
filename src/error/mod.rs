use lexical_error::LexicalError;
use parser_error::ParserError;

pub mod lexical_error;
pub mod parser_error;

//TODO: since this is only used in parse and lex stages, would AnalysisError be slightly better?
#[derive(Debug)]
pub enum CompilationError<'a> {
    LexicalError(LexicalError<'a>),
    ParserError(ParserError<'a>),
}

impl<'a> std::error::Error for CompilationError<'a> {}

impl<'a> std::fmt::Display for CompilationError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::LexicalError(le) => write!(f, "{le}"),
            Self::ParserError(pe) => write!(f, "{pe}"),
        }
    }
}
