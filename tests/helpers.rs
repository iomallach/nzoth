use std::cell::RefCell;

use nzoth::{
    ast::{Program, visitor::Visitor},
    error::CompilationError,
    source::SourceFile,
    token::Token,
};

pub struct SnapshotErrors<'a>(pub &'a Vec<CompilationError<'a>>);

impl<'a> std::fmt::Display for SnapshotErrors<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for error in self.0 {
            write!(f, "{error}\n")?;
        }
        Ok(())
    }
}

pub struct SnapshotToken<'a> {
    token: Token,
    source: &'a RefCell<SourceFile<'a>>,
}

impl<'a> SnapshotToken<'a> {
    pub fn new(token: Token, source: &'a RefCell<SourceFile<'a>>) -> Self {
        Self { token, source }
    }
}

impl<'a> std::fmt::Display for SnapshotToken<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Token(kind: \"{}\", literal: \"{}\")",
            self.token.kind,
            self.source.borrow().span_text(&self.token.span)
        )
    }
}

pub struct SnapshotTokens<'a>(pub Vec<SnapshotToken<'a>>);

impl<'a> std::fmt::Display for SnapshotTokens<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut iter = self.0.iter();
        f.write_str("[\n")?;
        while let Some(token) = iter.next() {
            write!(f, "  {},\n", token)?;
        }
        f.write_str("]")
    }
}

pub struct SnapshotVisitor<'a> {
    pub source: &'a SourceFile<'a>,
}

impl<'a> Visitor for SnapshotVisitor<'a> {
    type Output = String;

    fn visit_program(&mut self, program: &Program) -> Self::Output {
        todo!()
    }

    fn visit_ast_node(&mut self, node: &nzoth::ast::AstNode) -> Self::Output {
        todo!()
    }

    fn visit_statement_node(&mut self, stmt: &nzoth::ast::Statement) -> Self::Output {
        todo!()
    }

    fn visit_let_declaration(&mut self, let_decl: &nzoth::ast::LetDeclaration) -> Self::Output {
        todo!()
    }

    fn visit_expression_statement(
        &mut self,
        expr_stmt: &nzoth::ast::ExpressionStatement,
    ) -> Self::Output {
        todo!()
    }

    fn visit_block(&mut self, block: &nzoth::ast::Block) -> Self::Output {
        todo!()
    }

    fn visit_expression_node(&mut self, expr: &nzoth::ast::Expression) -> Self::Output {
        todo!()
    }

    fn visit_numeric_literal(&mut self, lit: &nzoth::ast::NumericLiteral) -> Self::Output {
        todo!()
    }

    fn visit_bool(&mut self, b: bool) -> Self::Output {
        todo!()
    }

    fn visit_identifier(&mut self, name: &str) -> Self::Output {
        todo!()
    }

    fn visit_prefix(
        &mut self,
        op: &nzoth::ast::PrefixOp,
        expr: &nzoth::ast::Expression,
    ) -> Self::Output {
        todo!()
    }

    fn visit_infix(
        &mut self,
        left: &nzoth::ast::Expression,
        op: &nzoth::ast::InfixOp,
        right: &nzoth::ast::Expression,
    ) -> Self::Output {
        todo!()
    }

    fn visit_grouped_expression(&mut self, expr: &nzoth::ast::Expression) -> Self::Output {
        todo!()
    }

    fn visit_func_declaration(&mut self, func_decl: &nzoth::ast::FuncDeclaration) -> Self::Output {
        todo!()
    }

    fn visit_func_parameter(&mut self, param: &nzoth::ast::FuncParameter) -> Self::Output {
        todo!()
    }

    fn visit_return(&mut self, ret: &nzoth::ast::Return) -> Self::Output {
        todo!()
    }

    fn visit_function_call(
        &mut self,
        ident_expr: &nzoth::ast::Expression,
        expr_list: &Vec<nzoth::ast::Expression>,
    ) -> Self::Output {
        todo!()
    }
}
