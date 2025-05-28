use std::fmt::Display;

use crate::source::Span;

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
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
    ColonColon,
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

impl Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Comma => write!(f, ","),
            TokenKind::SingleLineComment => write!(f, "//"),
            TokenKind::Plus => write!(f, "+"),
            TokenKind::PlusPlus => write!(f, "++"),
            TokenKind::Minus => write!(f, "-"),
            TokenKind::MinusMinus => write!(f, "--"),
            TokenKind::Slash => write!(f, "/"),
            TokenKind::Asterisk => write!(f, "*"),
            TokenKind::Greater => write!(f, ">"),
            TokenKind::GreaterEqual => write!(f, ">="),
            TokenKind::Less => write!(f, "<"),
            TokenKind::LessEqual => write!(f, "<="),
            TokenKind::Equal => write!(f, "="),
            TokenKind::EqualEqual => write!(f, "=="),
            TokenKind::Bang => write!(f, "!"),
            TokenKind::BangEqual => write!(f, "!="),
            TokenKind::Semicolon => write!(f, ";"),
            TokenKind::ColonColon => write!(f, "::"),
            TokenKind::LParen => write!(f, "("),
            TokenKind::RParen => write!(f, ")"),
            TokenKind::LBrace => write!(f, "{{"),
            TokenKind::RBrace => write!(f, "}}"),
            TokenKind::StringLit => write!(f, "string literal"),
            TokenKind::UnbalancedQuote => write!(f, "unbalanced quote"),
            TokenKind::Integer => write!(f, "integer"),
            TokenKind::Illegal => write!(f, "illegal token"),
            TokenKind::Identifier => write!(f, "identifier"),
            TokenKind::KWLet => write!(f, "let"),
            TokenKind::KWTrue => write!(f, "true"),
            TokenKind::KWFalse => write!(f, "false"),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }
}
