use crate::source::Span;

#[derive(Debug, PartialEq, Eq)]
pub enum TokenKind {
    Comma,
    SingleLineComment,
    Plus,
    PlusPlus,
    Minus,
    MinusMinus,
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
    StringLit,
    UnbalancedQuote,
    Integer,
    Illegal,
    Identifier,
    KWLet,
    KWTrue,
    KWFalse,
}

pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}
