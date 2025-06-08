use crate::source::Span;

pub type TypeId = usize;

pub const INT_TYPE_ID: TypeId = 0;
pub const BOOL_TYPE_ID: TypeId = 1;

pub enum CheckedStatement {
    LetVarDeclaration(CheckedLetVarDeclaration),
    Expression(CheckedExpression),
    Block(CheckedBlock),
    LetFuncDeclaration(CheckedLetFuncDeclaration),
    Return(CheckedReturn),
    EndOfProgram,
}

#[derive(Clone)]
pub struct CheckedLetVarDeclaration {
    pub name: String,
    pub initializer: Box<CheckedExpression>,
    pub ty: TypeId,
    pub span: Span,
}

pub struct CheckedBlock {
    pub statements: Vec<CheckedStatement>,
    pub trailing_expression: Option<CheckedExpression>,
    pub span: Span,
}

pub struct CheckedFunctionBody {
    pub statements: Vec<CheckedStatement>,
    pub span: Span,
}

pub struct CheckedLetFuncDeclaration {
    pub name: String,
    pub parameters: Vec<CheckedLetVarDeclaration>,
    pub body: CheckedFunctionBody,
    pub return_type: TypeId,
    pub span: Span,
}

pub struct CheckedReturn {
    pub expression: CheckedExpression,
    pub span: Span,
}

#[derive(Clone)]
pub enum CheckedExpression {
    NumericLiteral(CheckedNumericLiteral, TypeId, Span),
    Bool(bool, Span),
    Variable(CheckedLetVarDeclaration, TypeId, Span),
    Prefix(CheckedPrefixOp, Box<CheckedExpression>, TypeId, Span),
    Infix(
        Box<CheckedExpression>,
        CheckedInfixOp,
        Box<CheckedExpression>,
        TypeId,
        Span,
    ),
}

impl CheckedExpression {
    pub fn type_id(&self) -> TypeId {
        match self {
            Self::NumericLiteral(_, tid, _) => *tid,
            Self::Bool(_, _) => BOOL_TYPE_ID,
            Self::Variable(_, tid, _) => *tid,
            Self::Prefix(_, _, tid, _) => *tid,
            Self::Infix(_, _, _, tid, _) => *tid,
        }
    }
}

pub enum CheckedType {
    BuiltIn(BuiltInType),
}

pub enum BuiltInType {
    Int,
    Bool,
}

#[derive(Clone)]
pub enum CheckedNumericLiteral {
    Integer(i64),
}

#[derive(Clone)]
pub enum CheckedPrefixOp {
    Negation,
    Not,
}

#[derive(Clone)]
pub enum CheckedInfixOp {
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
