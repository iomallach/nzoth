use super::{
    AstNode, Block, Expression, ExpressionStatement, InfixOp, LetDeclaration, NumericLiteral,
    PrefixOp, Program, Statement,
};

trait Visitor {
    type Output;

    fn visit_program(&mut self, program: &Program) -> Self::Output;
    fn visit_ast_node(&mut self, node: &AstNode) -> Self::Output;
    fn visit_statement_node(&mut self, stmt: &Statement) -> Self::Output;
    fn visit_let_declaration(&mut self, let_decl: &LetDeclaration) -> Self::Output;
    fn visit_expression_statement(&mut self, expr_stmt: &ExpressionStatement) -> Self::Output;
    fn visit_block(&mut self, block: &Block) -> Self::Output;
    fn visit_expression_node(&mut self, expr: &Expression) -> Self::Output;
    fn visit_numeric_literal(&mut self, lit: &NumericLiteral) -> Self::Output;
    fn visit_bool(&mut self, b: bool) -> Self::Output;
    fn visit_identifier(&mut self, name: &str) -> Self::Output;
    fn visit_prefix(&mut self, op: &PrefixOp, expr: &Expression) -> Self::Output;
    fn visit_infix(&mut self, left: &Expression, op: &InfixOp, right: &Expression) -> Self::Output;
    fn visit_grouped_expression(&mut self, expr: &Expression) -> Self::Output;
}

pub struct ProgramPrinter;

impl ProgramPrinter {
    pub fn print(program: &Program) -> String {
        Self.visit_program(program)
    }
}

impl Visitor for ProgramPrinter {
    type Output = String;

    fn visit_program(&mut self, program: &Program) -> Self::Output {
        let mut out = vec![];
        for p in &program.nodes {
            out.push(self.visit_ast_node(p));
        }

        out.join("\n")
    }

    fn visit_ast_node(&mut self, node: &AstNode) -> Self::Output {
        match node {
            AstNode::Statement(stmt) => self.visit_statement_node(stmt),
            AstNode::Expression(expr) => self.visit_expression_node(expr),
        }
    }

    fn visit_statement_node(&mut self, stmt: &Statement) -> Self::Output {
        match stmt {
            Statement::LetDeclaration(let_decl) => self.visit_let_declaration(let_decl),
            Statement::Block(block) => self.visit_block(block),
            Statement::Expression(expr_stmt) => self.visit_expression_statement(expr_stmt),
        }
    }

    fn visit_let_declaration(&mut self, let_decl: &LetDeclaration) -> Self::Output {
        let mut out = String::new();
        out.push_str(format!("let {}", self.visit_expression_node(&let_decl.identifier)).as_str());

        if let Some(ty) = &let_decl.ty {
            out.push_str(format!(" :: {ty}").as_str());
        }
        out.push_str(format!(" = {};", self.visit_expression_node(&let_decl.expression)).as_str());

        out
    }

    fn visit_expression_statement(&mut self, expr_stmt: &ExpressionStatement) -> Self::Output {
        format!("{};", self.visit_expression_node(&expr_stmt.expression))
    }

    fn visit_block(&mut self, block: &Block) -> Self::Output {
        let mut out = String::new();
        out.push_str("{\n");

        for node in &block.nodes {
            out.push_str(format!("{}\n", self.visit_ast_node(node)).as_str());
        }
        if let Some(le) = &block.last_expression {
            //TODO: not sure this has to be a node, check it
            out.push_str(format!("{}\n", self.visit_ast_node(le)).as_str());
        }
        out.push_str("}");

        out
    }

    fn visit_expression_node(&mut self, expr: &Expression) -> Self::Output {
        match &expr {
            Expression::NumericLiteral(num_lit, _) => self.visit_numeric_literal(num_lit),
            Expression::Bool(b, _) => self.visit_bool(*b),
            Expression::Identifier(name, _) => self.visit_identifier(name),
            Expression::Prefix(op, expr, _) => self.visit_prefix(op, expr),
            Expression::Infix(left, op, right, _) => self.visit_infix(left, op, right),
            Expression::Grouped(expr, _) => self.visit_grouped_expression(expr),
        }
    }

    fn visit_numeric_literal(&mut self, lit: &NumericLiteral) -> Self::Output {
        match lit {
            NumericLiteral::Integer(i) => format!("{i}"),
        }
    }

    fn visit_bool(&mut self, b: bool) -> Self::Output {
        format!("{b}")
    }

    fn visit_identifier(&mut self, name: &str) -> Self::Output {
        name.to_string()
    }

    fn visit_prefix(&mut self, op: &PrefixOp, expr: &Expression) -> Self::Output {
        format!("({op}{})", self.visit_expression_node(expr))
    }

    fn visit_infix(&mut self, left: &Expression, op: &InfixOp, right: &Expression) -> Self::Output {
        format!(
            "({} {op} {})",
            self.visit_expression_node(left),
            self.visit_expression_node(right)
        )
    }

    fn visit_grouped_expression(&mut self, expr: &Expression) -> Self::Output {
        format!("{}", self.visit_expression_node(expr))
    }
}
