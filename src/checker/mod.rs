use std::collections::HashMap;

use checked_ir::{
    BOOL_TYPE_ID, BuiltInType, CheckedBlock, CheckedExpression, CheckedFuncDeclaration,
    CheckedFunctionBody, CheckedInfixOp, CheckedLetVarDeclaration, CheckedNode,
    CheckedNumericLiteral, CheckedPrefixOp, CheckedReturn, CheckedStatement, CheckedType,
    FLOAT_TYPE_ID, INT_TYPE_ID, TypeId,
};

use crate::ast::{
    Block, Expression, FuncDeclaration, FuncParameter, LetDeclaration, Node, NumericLiteral,
    Program, Statement, Type,
};

pub mod checked_ir;

pub struct CompilationUnit {
    scope: Scope,
    functions: Vec<CheckedFuncDeclaration>,
    types: Vec<CheckedType>,
}

impl CompilationUnit {
    pub fn new() -> Self {
        let mut types = vec![];
        types.push(CheckedType::BuiltIn(BuiltInType::Int));
        types.push(CheckedType::BuiltIn(BuiltInType::Bool));
        types.push(CheckedType::BuiltIn(BuiltInType::Float));

        Self {
            scope: Scope::new(None),
            functions: vec![],
            types,
        }
    }

    pub fn enter_scope(&mut self) {
        let old_scope = std::mem::take(&mut self.scope);
        self.scope = Scope::new(Some(old_scope));
    }

    pub fn exit_scope(&mut self) {
        let current_scope = std::mem::take(&mut self.scope);
        let parent_scope = current_scope.parent.expect("Never exit the root scope");
        self.scope = *parent_scope;
    }

    pub fn add_function(&mut self, function: CheckedFuncDeclaration) {
        self.functions.push(function);
    }

    pub fn add_type(&mut self, ty: CheckedType) -> TypeId {
        self.types.push(ty);

        self.types.len() - 1
    }

    pub fn find_type_by_id(&self, type_id: TypeId) -> &CheckedType {
        &self.types[type_id]
    }

    pub fn add_let_func_decl_to_scope(&mut self, decl: CheckedFuncDeclaration) {
        self.scope.add_func_decl(decl);
    }

    pub fn add_let_var_decl_to_scope(&mut self, decl: CheckedLetVarDeclaration) {
        self.scope.add_var_decl(decl);
    }

    pub fn find_let_var_decl_in_scope(&self, name: &str) -> Option<CheckedLetVarDeclaration> {
        self.scope.lookup_var_decl(name)
    }

    pub fn find_func_decl_in_scope(&self, name: &str) -> Option<CheckedFuncDeclaration> {
        self.scope.lookup_func_decl(name)
    }

    //TODO: this is incorrect, it would count being inside a block scope as being inside a function
    pub fn is_inside_function(&self) -> bool {
        self.scope.parent.is_some()
    }
}

pub struct Scope {
    parent: Option<Box<Scope>>,
    functions: HashMap<String, CheckedFuncDeclaration>,
    var_decls: HashMap<String, CheckedLetVarDeclaration>,
    //imports,
    //types,
}

impl Default for Scope {
    fn default() -> Self {
        Self::new(None)
    }
}

impl Scope {
    pub fn new(parent: Option<Scope>) -> Self {
        Self {
            parent: parent.map(|p| Box::new(p)),
            functions: HashMap::new(),
            var_decls: HashMap::new(),
        }
    }

    pub fn lookup_var_decl(&self, name: &str) -> Option<CheckedLetVarDeclaration> {
        self.var_decls
            .get(name)
            .cloned()
            .or_else(|| self.parent.as_ref()?.lookup_var_decl(name))
    }

    pub fn lookup_func_decl(&self, name: &str) -> Option<CheckedFuncDeclaration> {
        self.functions
            .get(name)
            .cloned()
            .or_else(|| self.parent.as_ref()?.lookup_func_decl(name))
    }

    pub fn add_var_decl(&mut self, decl: CheckedLetVarDeclaration) {
        //TODO: handle redefinition
        self.var_decls.insert(decl.name.clone(), decl);
    }

    pub fn add_func_decl(&mut self, decl: CheckedFuncDeclaration) {
        //TODO: handle redefinition
        self.functions.insert(decl.name.clone(), decl);
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
                CheckedNode::FunctionDeclaration(fd) => {
                    self.comp_unit.add_function(fd);
                }
            }
        }
    }

    pub fn typecheck_node(&mut self, node: &Node) -> CheckedNode {
        match node {
            Node::FunctionDeclaration(fd) => {
                CheckedNode::FunctionDeclaration(self.typecheck_func_declaration(fd))
            }
            Node::ImportDeclaration | Node::StaticDeclaration | Node::ConstDeclaration => {
                todo!("not implemented")
            }
        }
    }

    pub fn typecheck_statement(&mut self, stmt: &Statement) -> CheckedStatement {
        match stmt {
            Statement::Node(node) => CheckedStatement::Node(self.typecheck_node(node)),
            Statement::Expression(expr_stmt) => {
                CheckedStatement::Expression(self.typecheck_expression(&expr_stmt.expression))
            }
            Statement::LetDeclaration(ld) => {
                let (decl, expr) = self.typecheck_let_declaration(ld);
                CheckedStatement::CheckedLetVarDeclaration(decl, expr)
            }
            Statement::Return(_) => todo!(),
        }
    }

    pub fn typecheck_let_declaration(
        &mut self,
        decl: &LetDeclaration,
    ) -> (CheckedLetVarDeclaration, CheckedExpression) {
        let checked_expr = self.typecheck_expression(&decl.expression);

        if decl.ty.is_some()
            && self.typecheck_typename(decl.ty.as_ref().unwrap()) != checked_expr.type_id()
        {
            todo!("handle type mismatch");
        }

        let checked_decl = CheckedLetVarDeclaration {
            name: decl.identifier.clone(),
            ty: checked_expr.type_id(),
            span: decl.span,
        };
        self.comp_unit
            .add_let_var_decl_to_scope(checked_decl.clone());

        (checked_decl, checked_expr)
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
                    .find_let_var_decl_in_scope(name)
                    .expect("Might actually panic, needs handling");
                let checked_type_id = checked_let_decl.ty;
                CheckedExpression::Variable(checked_let_decl, checked_type_id, *span)
            }
            Expression::Prefix(op, expr, span) => {
                let checked_expr = self.typecheck_expression(expr);
                let checked_expr_type = self.comp_unit.find_type_by_id(checked_expr.type_id());
                let checked_prefix_op = CheckedPrefixOp::from(*op);

                match (&checked_prefix_op, checked_expr_type) {
                    (CheckedPrefixOp::Not, CheckedType::BuiltIn(BuiltInType::Bool)) => {
                        CheckedExpression::Prefix(
                            checked_prefix_op,
                            Box::new(checked_expr),
                            BOOL_TYPE_ID,
                            *span,
                        )
                    }
                    (CheckedPrefixOp::Negation, CheckedType::BuiltIn(BuiltInType::Int)) => {
                        CheckedExpression::Prefix(
                            checked_prefix_op,
                            Box::new(checked_expr),
                            INT_TYPE_ID,
                            *span,
                        )
                    }
                    (CheckedPrefixOp::Negation, CheckedType::BuiltIn(BuiltInType::Float)) => {
                        CheckedExpression::Prefix(
                            checked_prefix_op,
                            Box::new(checked_expr),
                            FLOAT_TYPE_ID,
                            *span,
                        )
                    }
                    _ => todo!("Handle these cases"),
                }
            }
            Expression::Infix(left, op, right, span) => {
                let checked_left_expr = self.typecheck_expression(left);
                let checked_right_expr = self.typecheck_expression(right);
                let checked_infix_op = CheckedInfixOp::from(*op);

                let left_type_id = checked_left_expr.type_id();
                let right_type_id = checked_right_expr.type_id();
                let left_type = self.comp_unit.find_type_by_id(left_type_id);
                let right_type = self.comp_unit.find_type_by_id(right_type_id);

                match checked_infix_op {
                    CheckedInfixOp::Add
                    | CheckedInfixOp::Subtract
                    | CheckedInfixOp::Multiply
                    | CheckedInfixOp::Divide => {
                        if left_type.is_numeric()
                            && right_type.is_numeric()
                            && left_type_id == right_type_id
                        {
                            CheckedExpression::Infix(
                                Box::new(checked_left_expr),
                                checked_infix_op,
                                Box::new(checked_right_expr),
                                left_type_id,
                                *span,
                            )
                        } else {
                            todo!("Handle these cases")
                        }
                    }
                    CheckedInfixOp::LessThan
                    | CheckedInfixOp::LessThanEquals
                    | CheckedInfixOp::GreaterThan
                    | CheckedInfixOp::GreaterThanEquals => {
                        if left_type.is_numeric()
                            && right_type.is_numeric()
                            && left_type_id == right_type_id
                        {
                            CheckedExpression::Infix(
                                Box::new(checked_left_expr),
                                checked_infix_op,
                                Box::new(checked_right_expr),
                                BOOL_TYPE_ID,
                                *span,
                            )
                        } else {
                            todo!("Handle these cases")
                        }
                    }
                    CheckedInfixOp::Assignment => {
                        if left_type_id == right_type_id {
                            CheckedExpression::Infix(
                                Box::new(checked_left_expr),
                                checked_infix_op,
                                Box::new(checked_right_expr),
                                left_type_id,
                                *span,
                            )
                        } else {
                            todo!("Handle these cases")
                        }
                    }
                    CheckedInfixOp::Equals | CheckedInfixOp::NotEquals => {
                        if left_type.is_builtin()
                            && right_type.is_builtin()
                            && left_type_id == right_type_id
                        {
                            CheckedExpression::Infix(
                                Box::new(checked_left_expr),
                                checked_infix_op,
                                Box::new(checked_right_expr),
                                BOOL_TYPE_ID,
                                *span,
                            )
                        } else {
                            todo!("Handle these cases")
                        }
                    }
                }
            }
            Expression::Grouped(expr, _) => self.typecheck_expression(expr),
            Expression::FunctionCall(_, _, _) => todo!(),
        }
    }

    fn typecheck_block(&mut self, block: &Block) -> CheckedBlock {
        let mut checked_nodes = vec![];
        let mut checked_last_expr = None;
        for node in &block.nodes {
            checked_nodes.push(self.typecheck_statement(node));
        }

        if let Some(expr_stmt) = &block.last_expression {
            checked_last_expr = Some(self.typecheck_expression(&expr_stmt.expression));
        }

        CheckedBlock {
            statements: checked_nodes,
            trailing_expression: checked_last_expr,
            span: block.span,
        }
    }

    fn typecheck_func_declaration(&mut self, decl: &FuncDeclaration) -> CheckedFuncDeclaration {
        // save decl
        self.comp_unit.enter_scope();

        let checked_params = self.typecheck_function_parameters(&decl.paramemetrs);
        let checked_body_block = self.typecheck_block(&decl.body);
        let checked_func_body = self.typecheck_func_body_block(checked_body_block);
        let checked_return_type = decl
            .return_type
            .as_ref()
            .map(|t| self.typecheck_typename(t));
        let return_type = match checked_func_body.statements.last().expect("Never empty") {
            CheckedStatement::Return(ret) => {
                self.typecheck_func_return_type(ret, checked_return_type)
            }
            _ => unreachable!("Function body always ends with a return even if it is empty"),
        };

        self.comp_unit.exit_scope();

        let checked_decl = CheckedFuncDeclaration {
            name: decl.identifier.clone(),
            parameters: checked_params,
            body: checked_func_body,
            return_type,
            span: decl.span,
        };

        self.comp_unit
            .add_let_func_decl_to_scope(checked_decl.clone());

        checked_decl
    }

    fn typecheck_function_parameters(
        &mut self,
        params: &Vec<FuncParameter>,
    ) -> Vec<CheckedLetVarDeclaration> {
        let mut checked_params = vec![];
        for param in params {
            let checked_parameter = CheckedLetVarDeclaration {
                name: param.identifier.clone(),
                ty: self.typecheck_typename(&param.ty),
                span: param.span,
            };
            self.comp_unit
                .add_let_var_decl_to_scope(checked_parameter.clone());
            checked_params.push(checked_parameter);
        }

        checked_params
    }

    fn typecheck_func_body_block(&mut self, block: CheckedBlock) -> CheckedFunctionBody {
        let mut body_statements = vec![];

        for stmt in block.statements {
            body_statements.push(stmt);
        }

        let is_last_statement_return =
            if let Some(CheckedStatement::Return(_)) = body_statements.last() {
                true
            } else {
                false
            };

        if is_last_statement_return {
            return CheckedFunctionBody {
                statements: body_statements,
                span: block.span,
            };
        } else if let Some(expr) = block.trailing_expression {
            body_statements.push(CheckedStatement::Return(CheckedReturn {
                span: expr.span(),
                expression: expr,
            }));
        } else {
            todo!("Implicit unit return not implemented")
        }

        CheckedFunctionBody {
            statements: body_statements,
            span: block.span,
        }
    }

    fn typecheck_func_return_type(
        &mut self,
        ret: &CheckedReturn,
        ret_type: Option<TypeId>,
    ) -> TypeId {
        //TODO: for the sake of simplicity assume the type is always given for now
        let ret_ty = ret_type.unwrap();
        let ret_exp_ty = ret.expression.type_id();

        if ret_ty != ret_exp_ty {
            todo!("handle error")
        }
        ret_ty
    }
}
