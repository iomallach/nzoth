use crate::{
    ast::{InfixOp, PrefixOp},
    source::Span,
};

pub type TypeId = usize;

pub const INT_TYPE_ID: TypeId = 0;
pub const BOOL_TYPE_ID: TypeId = 1;
pub const FLOAT_TYPE_ID: TypeId = 2;
pub const UNIT_TYPE_ID: TypeId = 3;

#[derive(Clone, Debug)]
pub enum CheckedNode {
    FunctionDeclaration(CheckedFuncDeclaration),
}

#[derive(Clone, Debug)]
pub struct CheckedFuncDeclaration {
    pub name: String,
    pub parameters: Vec<CheckedLetVarDeclaration>,
    pub body: CheckedFunctionBody,
    pub return_type: TypeId,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct CheckedFunctionBody {
    pub statements: Vec<CheckedStatement>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub enum CheckedStatement {
    Node(CheckedNode),
    CheckedLetVarDeclaration(CheckedLetVarDeclaration, CheckedExpression),
    Expression(CheckedExpression),
    Return(CheckedReturn),
}

#[derive(Clone, Debug)]
pub struct CheckedLetVarDeclaration {
    pub name: String,
    pub ty: TypeId,
    pub span: Span,
}

#[derive(Clone)]
pub struct CheckedBlock {
    pub statements: Vec<CheckedStatement>,
    pub trailing_expression: Option<CheckedExpression>,
    pub span: Span,
}

#[derive(Clone, Debug)]
pub struct CheckedReturn {
    pub expression: CheckedExpression,
    pub span: Span,
}

#[derive(Clone, Debug)]
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
    Unit(Span),
}

impl CheckedExpression {
    pub fn span(&self) -> Span {
        match self {
            CheckedExpression::NumericLiteral(_, _, span) => *span,
            CheckedExpression::Bool(_, span) => *span,
            CheckedExpression::Variable(_, _, span) => *span,
            CheckedExpression::Prefix(_, _, _, span) => *span,
            CheckedExpression::Infix(_, _, _, _, span) => *span,
            CheckedExpression::Unit(span) => *span,
        }
    }
}

impl CheckedExpression {
    pub fn type_id(&self) -> TypeId {
        match self {
            Self::NumericLiteral(_, tid, _) => *tid,
            Self::Bool(_, _) => BOOL_TYPE_ID,
            Self::Variable(_, tid, _) => *tid,
            Self::Prefix(_, _, tid, _) => *tid,
            Self::Infix(_, _, _, tid, _) => *tid,
            Self::Unit(_) => UNIT_TYPE_ID,
        }
    }
}

#[derive(Debug)]
pub enum CheckedType {
    BuiltIn(BuiltInType),
}

impl CheckedType {
    pub fn is_numeric(&self) -> bool {
        match self {
            Self::BuiltIn(bi) => bi.is_numeric(),
        }
    }
    pub fn is_bool(&self) -> bool {
        match self {
            Self::BuiltIn(bi) => bi.is_bool(),
        }
    }

    pub fn is_builtin(&self) -> bool {
        if let Self::BuiltIn(_) = self {
            true
        } else {
            false
        }
    }
}

impl std::fmt::Display for CheckedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::BuiltIn(t) => write!(f, "{t}"),
        }
    }
}

#[derive(Debug)]
pub enum BuiltInType {
    Int,
    Float,
    Bool,
    Unit,
}

impl BuiltInType {
    pub fn is_numeric(&self) -> bool {
        match self {
            Self::Int => true,
            Self::Float => true,
            Self::Bool => false,
            Self::Unit => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            Self::Int => false,
            Self::Float => false,
            Self::Bool => true,
            Self::Unit => false,
        }
    }
}

impl std::fmt::Display for BuiltInType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BuiltInType::Int => write!(f, "`int`"),
            BuiltInType::Float => write!(f, "`float`"),
            BuiltInType::Bool => write!(f, "`bool`"),
            BuiltInType::Unit => write!(f, "`unit`"),
        }
    }
}

#[derive(Clone, Debug)]
pub enum CheckedNumericLiteral {
    Integer(i64),
    Float(f64),
}

#[derive(Clone, Debug)]
pub enum CheckedPrefixOp {
    Negation,
    Not,
}

impl From<PrefixOp> for CheckedPrefixOp {
    fn from(value: PrefixOp) -> Self {
        match value {
            PrefixOp::BoolNegation => Self::Not,
            PrefixOp::Negation => Self::Negation,
        }
    }
}

#[derive(Clone, Debug)]
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

impl From<InfixOp> for CheckedInfixOp {
    fn from(value: InfixOp) -> Self {
        match value {
            InfixOp::Add => Self::Add,
            InfixOp::Assignment => Self::Assignment,
            InfixOp::Subtract => Self::Subtract,
            InfixOp::Multiply => Self::Multiply,
            InfixOp::Divide => Self::Divide,
            InfixOp::Equals => Self::Equals,
            InfixOp::NotEquals => Self::NotEquals,
            InfixOp::LessThan => Self::LessThan,
            InfixOp::LessThanEquals => Self::LessThanEquals,
            InfixOp::GreaterThan => Self::GreaterThan,
            InfixOp::GreaterThanEquals => Self::GreaterThanEquals,
        }
    }
}
