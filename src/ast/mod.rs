use std::{
    fmt::Display,
    ops::{Deref, DerefMut},
};

use crate::{source::Span, token::TokenKind};

#[derive(Debug)]
pub struct Program {
    pub nodes: Vec<Node>,
}

impl Deref for Program {
    type Target = Vec<Node>;

    fn deref(&self) -> &Self::Target {
        &self.nodes
    }
}

impl DerefMut for Program {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.nodes
    }
}

#[derive(Debug)]
pub enum Node {
    ImportDeclaration, //not implemented
    FunctionDeclaration(FuncDeclaration),
    ConstDeclaration,  //not implemented
    StaticDeclaration, //not implemented
}

#[derive(Debug)]
pub struct FuncDeclaration {
    pub identifier: String,
    pub paramemetrs: Vec<FuncParameter>,
    pub body: Block,
    pub return_type: Option<Type>,
    pub span: Span,
}

#[derive(Debug)]
pub struct Block {
    pub nodes: Vec<Statement>,
    pub last_expression: Option<Box<ExpressionStatement>>,
    pub span: Span,
}

#[derive(Debug)]
pub enum Statement {
    Node(Node),
    LetDeclaration(LetDeclaration),
    Expression(ExpressionStatement),
    Return(Return),
    //if statement
    //while loop statement
    //for loop statement
    //assignment??? now is an expression
}

#[derive(Debug)]
pub struct LetDeclaration {
    pub identifier: String,
    pub expression: Expression,
    pub span: Span,
    pub ty: Option<Type>,
}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub span: Span,
    pub expression: Expression,
}

#[derive(Debug)]
pub struct FuncParameter {
    pub identifier: String,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug)]
pub struct Return {
    pub expression: Expression,
    pub span: Span,
}

#[derive(Debug)]
pub enum Expression {
    NumericLiteral(NumericLiteral, Span),
    Bool(bool, Span),
    Identifier(String, Span),
    Prefix(PrefixOp, Box<Expression>, Span),
    Infix(Box<Expression>, InfixOp, Box<Expression>, Span),
    Grouped(Box<Expression>, Span),
    FunctionCall(Box<Expression>, Vec<Expression>, Span),
}

impl Expression {
    pub fn span(&self) -> Span {
        match self {
            Expression::NumericLiteral(_, span) => span.clone(),
            Expression::Bool(_, span) => span.clone(),
            Expression::Prefix(_, _, span) => span.clone(),
            Expression::Infix(_, _, _, span) => span.clone(),
            Expression::Identifier(_, span) => span.clone(),
            Expression::Grouped(_, span) => span.clone(),
            Expression::FunctionCall(_, _, span) => span.clone(),
        }
    }
}

#[derive(Debug)]
pub enum NumericLiteral {
    Integer(i64),
    Float(f64),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Name(String),
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Name(s) => write!(f, "{s}"),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum PrefixOp {
    Negation,
    BoolNegation,
}

impl Display for PrefixOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Negation => f.write_str("-"),
            Self::BoolNegation => f.write_str("!"),
        }
    }
}

impl From<&str> for PrefixOp {
    fn from(value: &str) -> Self {
        match value {
            "!" => Self::BoolNegation,
            "-" => Self::Negation,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum InfixOp {
    Add,
    Assignment,
    Subtract,
    Multiply,
    Divide,
    Equals,
    NotEquals,
    LessThan,
    LessThanEquals,
    GreaterThan,
    GreaterThanEquals,
}

impl Display for InfixOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            InfixOp::Add => f.write_str("+"),
            InfixOp::Assignment => f.write_str("="),
            InfixOp::Subtract => f.write_str("-"),
            InfixOp::Multiply => f.write_str("*"),
            InfixOp::Divide => f.write_str("/"),
            InfixOp::Equals => f.write_str("=="),
            InfixOp::NotEquals => f.write_str("!="),
            InfixOp::LessThan => f.write_str("<"),
            InfixOp::LessThanEquals => f.write_str("<="),
            InfixOp::GreaterThan => f.write_str(">"),
            InfixOp::GreaterThanEquals => f.write_str(">="),
        }
    }
}

impl From<&str> for InfixOp {
    fn from(value: &str) -> Self {
        match value {
            "+" => Self::Add,
            "-" => Self::Subtract,
            "*" => Self::Multiply,
            "/" => Self::Divide,
            "=" => Self::Assignment,
            "==" => Self::Equals,
            "!=" => Self::NotEquals,
            "<" => Self::LessThan,
            "<=" => Self::LessThanEquals,
            ">" => Self::GreaterThan,
            ">=" => Self::GreaterThanEquals,
            _ => unreachable!(),
        }
    }
}

#[derive(PartialOrd, PartialEq)]
#[repr(u8)]
pub enum Precedence {
    Lowest,
    Equals,
    Less,
    Add,
    Product,
    Prefix,
    Call,
    Index,
}

impl From<TokenKind> for Precedence {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::Plus | TokenKind::Minus => Self::Add,
            TokenKind::Asterisk | TokenKind::Slash => Self::Product,
            TokenKind::Equal | TokenKind::BangEqual | TokenKind::EqualEqual => Self::Equals,
            TokenKind::GreaterEqual
            | TokenKind::Greater
            | TokenKind::Less
            | TokenKind::LessEqual => Self::Less,
            TokenKind::LParen => Self::Call,
            _ => Self::Lowest,
        }
    }
}
// .EQUAL_EQUAL => Precedence.EQUALS,
// .BANG_EQUAL => Precedence.EQUALS,
// .LESS => Precedence.LESSGREATER,
// .LESS_EQUAL => Precedence.LESSGREATER,
// .GREATER => Precedence.LESSGREATER,
// .GREATER_EQUAL => Precedence.LESSGREATER,
// .PLUS => Precedence.SUM,
// .MINUS => Precedence.SUM,
// .SLASH => Precedence.PRODUCT,
// .ASTERISK => Precedence.PRODUCT,
// .LPAREN => Precedence.CALL,
// .LBRACKET => Precedence.INDEX,
// else => Precedence.LOWEST,
