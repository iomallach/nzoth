use checked_ir::{
    BOOL_TYPE_ID, BuiltInType, CheckedExpression, CheckedLetFuncDeclaration,
    CheckedLetVarDeclaration, CheckedNumericLiteral, CheckedStatement, CheckedType, FLOAT_TYPE_ID,
    INT_TYPE_ID, TypeId,
};

use crate::ast::{AstNode, Expression, LetDeclaration, NumericLiteral, Program, Statement, Type};

pub mod checked_ir;

type ScopeId = usize;

pub struct CompilationUnit {
    scopes: Vec<Scope>,
    functions: Vec<CheckedLetFuncDeclaration>,
    current_scope: ScopeId,
    types: Vec<CheckedType>,
}

impl CompilationUnit {
    pub fn new() -> Self {
        let scopes = vec![Scope::new(0, None)];
        let mut types = vec![];
        types.push(CheckedType::BuiltIn(BuiltInType::Int));
        types.push(CheckedType::BuiltIn(BuiltInType::Bool));

        Self {
            scopes,
            functions: vec![],
            current_scope: 0,
            types,
        }
    }

    pub fn add_function(&mut self, function: CheckedLetFuncDeclaration) {
        self.functions.push(function);
    }

    pub fn add_type(&mut self, ty: CheckedType) -> TypeId {
        self.types.push(ty);

        self.types.len() - 1
    }

    pub fn find_type_by_id(&self, type_id: TypeId) -> &CheckedType {
        &self.types[type_id]
    }

    pub fn add_let_var_decl_to_scope(&mut self, scope_id: ScopeId, decl: CheckedLetVarDeclaration) {
        self.scopes.get_mut(scope_id).map(|s| s.add_var_decl(decl));
    }

    pub fn find_let_var_decl_in_scope(
        &self,
        scope_id: ScopeId,
        name: &str,
    ) -> Option<CheckedLetVarDeclaration> {
        let mut scope_id = Some(scope_id);

        while let Some(yas) = scope_id {
            let scope = &self.scopes[yas];
            let maybe_pos = scope.var_decls.iter().position(|vd| vd.name == name);
            if let Some(pos) = maybe_pos {
                return Some(scope.var_decls[pos].clone());
            }
            scope_id = scope.parent;
        }

        None
    }
}

pub struct Scope {
    id: ScopeId,
    parent: Option<ScopeId>,
    functions: Vec<CheckedLetFuncDeclaration>,
    var_decls: Vec<CheckedLetVarDeclaration>,
    //imports,
    //types,
}

impl Scope {
    pub fn new(id: ScopeId, parent: Option<ScopeId>) -> Self {
        Self {
            id,
            parent,
            functions: vec![],
            var_decls: vec![],
        }
    }

    pub fn add_var_decl(&mut self, decl: CheckedLetVarDeclaration) {
        self.var_decls.push(decl);
    }
}

pub struct Checker {
    pub comp_unit: CompilationUnit,
}

impl Checker {
    pub fn new() -> Self {
        let comp_unit = CompilationUnit::new();

        Self { comp_unit }
    }

    pub fn typecheck_program(&mut self, program: Program) {
        for node in program.nodes {
            match self.typecheck_node(&node) {
                CheckedStatement::LetFuncDeclaration(fd) => {
                    self.comp_unit.add_function(fd);
                }
                _ => unreachable!(
                    "Top level statements other than function declarations are not allowed"
                ),
            }
        }
    }

    pub fn typecheck_node(&mut self, ast_node: &AstNode) -> CheckedStatement {
        match ast_node {
            AstNode::Statement(stmt) => todo!(),
            AstNode::Expression(expr) => todo!(),
            AstNode::EndOfProgram => CheckedStatement::EndOfProgram,
        }
    }

    pub fn typecheck_statement(&mut self, stmt: &Statement) -> CheckedStatement {
        match stmt {
            Statement::LetDeclaration(ld) => {
                CheckedStatement::LetVarDeclaration(self.typecheck_let_declaration(ld))
            }
            Statement::Expression(_) => todo!(),
            Statement::Block(_) => todo!(),
            Statement::FuncDeclaration(_) => todo!(),
            Statement::Return(_) => todo!(),
        }
    }

    pub fn typecheck_let_declaration(&mut self, decl: &LetDeclaration) -> CheckedLetVarDeclaration {
        let checked_expr = self.typecheck_expression(&decl.expression);

        if decl.ty.is_some()
            && self.typecheck_typename(decl.ty.as_ref().unwrap()) != checked_expr.type_id()
        {
            todo!("handle type mismatch");
        }

        let checked_decl = CheckedLetVarDeclaration {
            name: decl.identifier.clone(),
            ty: checked_expr.type_id(),
            initializer: Box::new(checked_expr),
            span: decl.span,
        };
        self.comp_unit
            .add_let_var_decl_to_scope(self.comp_unit.current_scope, checked_decl.clone());

        checked_decl
    }

    pub fn typecheck_typename(&mut self, ty: &Type) -> TypeId {
        match ty {
            Type::Name(name) => match name.as_str() {
                "int" => INT_TYPE_ID,
                "float" => FLOAT_TYPE_ID,
                "bool" => BOOL_TYPE_ID,
                _ => todo!("handle unknown types"),
            },
        }
    }

    pub fn typecheck_expression(&mut self, expr: &Expression) -> CheckedExpression {
        match expr {
            Expression::NumericLiteral(nl, span) => match nl {
                NumericLiteral::Integer(val) => CheckedExpression::NumericLiteral(
                    CheckedNumericLiteral::Integer(*val),
                    INT_TYPE_ID,
                    *span,
                ),
                NumericLiteral::Float(val) => CheckedExpression::NumericLiteral(
                    CheckedNumericLiteral::Float(*val),
                    FLOAT_TYPE_ID,
                    *span,
                ),
            },
            Expression::Bool(val, span) => CheckedExpression::Bool(*val, span.clone()),
            Expression::Identifier(name, span) => {
                let checked_let_decl = self
                    .comp_unit
                    .find_let_var_decl_in_scope(self.comp_unit.current_scope, name)
                    .expect("Might actually panic, needs handling");
                let checked_type_id = checked_let_decl.ty;
                CheckedExpression::Variable(checked_let_decl, checked_type_id, *span)
            }
            Expression::Prefix(_, _, _) => todo!(),
            Expression::Infix(_, _, _, _) => todo!(),
            Expression::Grouped(_, _) => todo!(),
            Expression::FunctionCall(_, _, _) => todo!(),
        }
    }
}
