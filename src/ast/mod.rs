pub mod visitor;

use std::{
    fmt::Display,
    ops::{Deref, DerefMut},
};

use crate::{source::Span, token::TokenKind};

pub struct Program {
    pub nodes: Vec<AstNode>,
}

impl Deref for Program {
    type Target = Vec<AstNode>;

    fn deref(&self) -> &Self::Target {
        &self.nodes
    }
}

impl DerefMut for Program {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.nodes
    }
}

impl Program {
    pub fn push(&mut self, node: AstNode) {
        self.nodes.push(node);
    }
}

#[derive(Debug)]
pub enum AstNode {
    Statement(Statement),
    Expression(Expression),
}

#[derive(Debug)]
pub enum Statement {
    LetDeclaration(LetDeclaration),
    Expression(ExpressionStatement),
    Block(Block),
    //function declaration
    //if statement
    //while loop statement
    //for loop statement
    //assignment
    //import statement here or top level node
}

#[derive(Debug)]
pub struct LetDeclaration {
    pub identifier: Expression,
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
pub struct Block {
    pub nodes: Vec<AstNode>,
    pub last_expression: Option<Box<AstNode>>,
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
        }
    }
}

#[derive(Debug)]
pub enum NumericLiteral {
    Integer(i64),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Integer,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Integer => write!(f, "int"),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug, PartialEq, Eq)]
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
