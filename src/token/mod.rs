use crate::source::Span;

pub enum TokenKind {
    Eof,
    Comma,
    SingleLineComment,
    Plus,
    Minus,
    Slash,
    Asterisk,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Equal,
    EqualEqual,
    Bang,
    BangEqual,
    Semicolon,
    Colon,
    LParen,
    RParen,
    LBrace,
    RBrace,
}

pub struct Token {
    kind: TokenKind,
    span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}
