use std::ops::{Deref, DerefMut};

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

pub enum AstNode {
    Statement(Statement),
    Expression(Expression),
}

pub enum Statement {
    LetDeclaration(LetDeclaration),
    //function declaration
    //if statement
    //while loop statement
    //for loop statement
    //assignment
    //import statement here or top level node
}

pub struct LetDeclaration {
    pub identifier: Expression,
    // span
    pub expression: Expression,
    pub span: Span,
}

pub enum Expression {
    NumericLiteral(NumericLiteral, Span),
    Bool(bool, Span),
    Identifier(Span),
    Prefix(PrefixOp, Box<Expression>, Span),
    Infix(Box<Expression>, InfixOp, Box<Expression>, Span),
}

impl Expression {
    pub fn span(&self) -> Span {
        match self {
            Expression::NumericLiteral(_, span) => span.clone(),
            Expression::Bool(_, span) => span.clone(),
            Expression::Prefix(_, _, span) => span.clone(),
            Expression::Infix(_, _, _, span) => span.clone(),
            Expression::Identifier(span) => span.clone(),
        }
    }
}

pub enum NumericLiteral {
    Integer(i64),
}

pub enum PrefixOp {
    Negation,
    BoolNegation,
}

pub enum InfixOp {
    Add,
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

impl From<&str> for InfixOp {
    fn from(value: &str) -> Self {
        match value {
            "+" => Self::Add,
            "-" => Self::Subtract,
            "*" => Self::Multiply,
            "/" => Self::Divide,
            "=" => Self::Equals,
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
            TokenKind::Plus => Self::Add,
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
