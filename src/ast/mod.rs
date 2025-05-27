use crate::source::Span;

pub struct Program {
    nodes: Vec<AstNode>,
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
