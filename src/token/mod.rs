pub enum TokenKind {
    Eof,
    Comma,
    SingleLineComment,
    Plus,
    Minus,
    Slash,
    Asterisk,
    Greater,
    Less,
    Equal,
    Bang,
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

pub struct Span {
    start: usize,
    end: usize,
}

impl Span {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }
}
