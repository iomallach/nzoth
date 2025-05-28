use std::ops::{Deref, DerefMut};

use crate::source::Span;

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
    pub name: String,
    // span
    pub expression: Expression,
    pub span: Span,
}

pub enum Expression {
    NumericLiteral(NumericLiteral, Span),
    Bool(bool, Span),
    Prefix(PrefixOp, Box<Expression>, Span),
    Infix(Box<Expression>, InfixOp, Box<Expression>, Span),
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

#[repr(u8)]
pub enum Precedence {
    Lowest,
    Equals,
    Less,
    Add,
}
