use std::cell::RefCell;
use std::iter::Peekable;

use crate::ast::{
    Block, Expression, ExpressionStatement, FuncDeclaration, FuncParameter, InfixOp,
    LetDeclaration, Node, NumericLiteral, Precedence, PrefixOp, Program, Return, Statement, Type,
};
use crate::error::CompilationError;
use crate::error::lexical_error::LexicalError;
use crate::error::parser_error::ParserError;
use crate::lexer::lexer::Lexer;
use crate::source::{SourceFile, Span};
use crate::token::{Token, TokenKind};

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    source_file: &'a RefCell<SourceFile<'a>>,
    errors: Vec<CompilationError<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a RefCell<SourceFile<'a>>) -> Self {
        let lexer = Lexer::new(source).peekable();
        Self {
            lexer,
            source_file: source,
            errors: vec![],
        }
    }

    pub fn errors(&self) -> &Vec<CompilationError<'a>> {
        &self.errors
    }

    pub fn parse(&mut self) -> Program {
        let mut program = Program { nodes: vec![] };

        while self.lexer.peek().is_some_and(|t| t.kind != TokenKind::Eof) {
            match self.parse_node() {
                Ok(node) => program.push(node),
                Err(comp_error) => {
                    self.errors.push(comp_error);
                    self.synchronize_parser();
                }
            }
        }
        program
    }

    fn synchronize_parser(&mut self) {
        loop {
            match self.peek_token_kind() {
                Ok(kind) if matches!(kind, TokenKind::Eof | TokenKind::Semicolon) => {
                    self.lexer.next();
                    return;
                }
                Ok(kind) if matches!(kind, TokenKind::RBrace) => {
                    return;
                }
                _ => {
                    if let Err(e) = self.next_token() {
                        self.errors.push(e);
                    }
                }
            }
        }
    }

    pub fn parse_node(&mut self) -> Result<Node, CompilationError<'a>> {
        let peek_token = self.peek_token()?;
        match peek_token.kind {
            TokenKind::KWLet => {
                let let_token = self.next_token()?;
                self.parse_let_func_declaration(let_token)
                    .map(|d| Node::FunctionDeclaration(d))
            }
            _ => Err(CompilationError::ParserError(
                ParserError::expected_top_level_item(
                    peek_token.kind,
                    peek_token.span,
                    &self.source_file,
                ),
            )),
        }
    }

    pub fn parse_statement(&mut self) -> Result<Statement, CompilationError<'a>> {
        match self.peek_token_kind()? {
            TokenKind::KWLet => {
                let let_token = self.next_token()?;
                let next_token = self.peek_token()?;
                match next_token.kind {
                    TokenKind::Identifier => self
                        .parse_let_var_declaration(let_token)
                        .map(|d| Statement::LetDeclaration(d)),
                    TokenKind::KWFn => self
                        .parse_let_func_declaration(let_token)
                        .map(|d| Statement::Node(Node::FunctionDeclaration(d))),
                    _ => Err(CompilationError::ParserError(
                        ParserError::unknown_symbol_in_let_declaration(
                            next_token.span,
                            &self.source_file,
                        ),
                    )),
                }
            }
            TokenKind::KWReturn => self.parse_return().map(|r| Statement::Return(r)),
            _ => self
                .parse_expression_statement()
                .map(|es| Statement::Expression(es)),
        }
    }

    pub fn parse_expression_statement(
        &mut self,
    ) -> Result<ExpressionStatement, CompilationError<'a>> {
        let start_token = self.peek_token()?;
        let expression = self.parse_expression(Precedence::Lowest)?;

        Ok(ExpressionStatement {
            span: Span::from_spans(start_token.span, expression.span()),
            expression,
        })
    }

    fn parse_let_func_declaration(
        &mut self,
        let_token: Token,
    ) -> Result<FuncDeclaration, CompilationError<'a>> {
        self.expect_and_next(TokenKind::KWFn)?;
        let identifier_token = self.expect_and_next(TokenKind::Identifier)?;
        let func_parameters = self.parse_func_parameter_list()?;
        let return_ty = if self.match_peek_token_kind(TokenKind::ColonColon)? {
            self.lexer.next();
            Some(self.parse_type_annotation()?)
        } else {
            None
        };
        let block = self.parse_block()?;

        Ok(FuncDeclaration {
            identifier: self
                .source_file
                .borrow()
                .span_text(&identifier_token.span)
                .to_string(),
            paramemetrs: func_parameters,
            body: block,
            return_type: return_ty,
            span: let_token.span,
        })
    }

    fn parse_func_parameter_list(&mut self) -> Result<Vec<FuncParameter>, CompilationError<'a>> {
        let mut func_params = vec![];
        self.expect_and_next(TokenKind::LParen)?;

        if self.match_peek_token_kind(TokenKind::RParen)? {
            self.lexer.next();
            return Ok(func_params);
        }

        loop {
            //TODO: might want to record the error right here and sync the parser until ")"
            let param_identifier_token = self.expect_and_next(TokenKind::Identifier)?;
            //TODO: sync the parser until ","
            self.expect_and_next(TokenKind::ColonColon)?;

            let param_type_token = self.expect_and_next(TokenKind::Identifier)?;
            func_params.push(FuncParameter {
                identifier: self
                    .source_file
                    .borrow()
                    .span_text(&param_identifier_token.span)
                    .to_string(),
                ty: Type::Name(
                    self.source_file
                        .borrow()
                        .span_text(&param_type_token.span)
                        .to_string(),
                ),
                span: param_identifier_token.span,
            });

            if !self.match_peek_token_kind(TokenKind::Comma)? {
                break;
            }
            self.lexer.next();
        }

        self.expect_and_next(TokenKind::RParen)?;

        Ok(func_params)
    }

    fn parse_let_var_declaration(
        &mut self,
        let_token: Token,
    ) -> Result<LetDeclaration, CompilationError<'a>> {
        let identifier_token = self.expect_and_next(TokenKind::Identifier)?;
        //TODO: check :: and parse type

        let ty = if self.match_peek_token_kind(TokenKind::ColonColon)? {
            self.lexer.next();
            Some(self.parse_type_annotation()?)
        } else {
            None
        };

        self.expect_and_next(TokenKind::Equal)?;

        let expression = self.parse_expression(Precedence::Lowest)?;
        let expression_span = expression.span();

        self.expect_and_next(TokenKind::Semicolon)?;

        let sf = self.source_file.borrow();

        //TODO: figure out the span here (now line vs the span of the name)
        Ok(LetDeclaration {
            identifier: sf.span_text(&identifier_token.span).to_string(),
            expression,
            span: Span {
                start: let_token.span.start,
                end: expression_span.end,
                file: sf.id,
            },
            ty,
        })
    }

    fn parse_return(&mut self) -> Result<Return, CompilationError<'a>> {
        let return_token = self.next_token()?;
        let return_expression = self.parse_expression(Precedence::Lowest)?;
        self.expect_and_next(TokenKind::Semicolon)?;

        Ok(Return {
            span: Span::from_spans(return_token.span, return_expression.span()),
            expression: return_expression,
        })
    }

    fn parse_block(&mut self) -> Result<Block, CompilationError<'a>> {
        let mut statements = vec![];
        let opening_curly_brace = self.expect_and_next(TokenKind::LBrace)?;
        let mut last_expression: Option<ExpressionStatement> = None;

        // TODO: edge cases: empty statements, no statements
        // TODO: these two probably should not short-circuit the execution and be handled
        // gracefully
        while !self.match_peek_token_kind(TokenKind::RBrace)?
            && !self.match_peek_token_kind(TokenKind::Eof)?
        {
            if let Some(le) = &last_expression {
                //FIXME: wrong error location, should point at the end of the last expression
                self.errors.push(CompilationError::ParserError(
                    ParserError::expected_semicolon(
                        Span::from_span_end(le.expression.span()),
                        &self.source_file,
                    ),
                ));
            }
            match self.parse_statement() {
                Ok(Statement::Expression(es))
                    if !self.match_peek_token_kind(TokenKind::Semicolon)? =>
                {
                    last_expression = Some(es);
                }
                Ok(Statement::Expression(es))
                    if self.match_peek_token_kind(TokenKind::Semicolon)? =>
                {
                    statements.push(Statement::Expression(es));
                    self.expect_and_next(TokenKind::Semicolon)?;
                }
                Ok(otherwise) => {
                    statements.push(otherwise);
                }
                Err(err) => {
                    self.errors.push(err);
                    self.synchronize_parser();
                }
            }
        }

        let closing_curly = self.expect_and_next(TokenKind::RBrace)?;

        Ok(Block {
            nodes: statements,
            last_expression: last_expression.map(|le| Box::new(le)),
            span: Span::from_spans(opening_curly_brace.span, closing_curly.span),
        })
    }

    fn parse_expression(
        &mut self,
        precedence: Precedence,
    ) -> Result<Expression, CompilationError<'a>> {
        let mut left_expression = self.apply_prefix_parse_fn()?;

        while !self.match_peek_token_kind(TokenKind::Semicolon)?
            && precedence < self.peek_precedence()?
        {
            left_expression = self.apply_infix_parse_fn(left_expression)?;
        }

        Ok(left_expression)
    }

    fn parse_integer(&mut self) -> Result<Expression, CompilationError<'a>> {
        self.next_token().map(|t| {
            let sf = self.source_file.borrow();
            let int_literal = sf.span_text(&t.span);
            Expression::NumericLiteral(
                NumericLiteral::Integer(int_literal.parse().expect("Lexer failed")),
                t.span,
            )
        })
    }

    fn parse_float(&mut self) -> Result<Expression, CompilationError<'a>> {
        self.next_token().map(|t| {
            let sf = self.source_file.borrow();
            let float_literal = sf.span_text(&t.span);
            Expression::NumericLiteral(
                NumericLiteral::Float(float_literal.parse().expect("Lexer failed")),
                t.span,
            )
        })
    }

    fn parse_identifier(&mut self) -> Result<Expression, CompilationError<'a>> {
        self.next_token().map(|t| {
            Expression::Identifier(
                self.source_file.borrow().span_text(&t.span).to_string(),
                t.span,
            )
        })
    }

    fn parse_boolean(&mut self) -> Result<Expression, CompilationError<'a>> {
        let boolean_token = self.next_token()?;

        if let TokenKind::KWTrue = boolean_token.kind {
            Ok(Expression::Bool(true, boolean_token.span))
        } else {
            Ok(Expression::Bool(false, boolean_token.span))
        }
    }

    //TODO: test coverage 0%
    fn parse_grouped_expression(&mut self) -> Result<Expression, CompilationError<'a>> {
        let l_paren_token = self.next_token()?;
        let grouped_expression = self.parse_expression(Precedence::Lowest)?;
        let grouped_expression_span = grouped_expression.span();

        self.expect_and_next(TokenKind::RParen)?;

        Ok(Expression::Grouped(
            Box::new(grouped_expression),
            Span::from_spans(l_paren_token.span, grouped_expression_span),
        ))
    }

    fn parse_function_call(
        &mut self,
        func_identifier: Expression,
    ) -> Result<Expression, CompilationError<'a>> {
        let func_ident_span = func_identifier.span();
        let params = self.parse_expression_list()?;
        Ok(Expression::FunctionCall(
            Box::new(func_identifier),
            params,
            func_ident_span,
        ))
    }

    fn parse_expression_list(&mut self) -> Result<Vec<Expression>, CompilationError<'a>> {
        let mut expr_list = vec![];
        self.expect_and_next(TokenKind::LParen)?;
        if self.match_peek_token_kind(TokenKind::RParen)? {
            self.lexer.next();
            return Ok(expr_list);
        }

        loop {
            let expr = self.parse_expression(Precedence::Lowest)?;
            expr_list.push(expr);

            if !self.match_peek_token_kind(TokenKind::Comma)? {
                break;
            }
            //skip comma
            self.lexer.next();
        }
        self.expect_and_next(TokenKind::RParen)?;

        Ok(expr_list)
    }

    fn parse_infix_expression(
        &mut self,
        left_expression: Expression,
    ) -> Result<Expression, CompilationError<'a>> {
        let current_token = self.next_token()?;
        let right_expression = self.parse_expression(Precedence::from(current_token.kind))?;
        let left_span = left_expression.span();
        let sf = self.source_file.borrow();
        let infix_op = InfixOp::from(sf.span_text(&current_token.span));

        Ok(Expression::Infix(
            Box::new(left_expression),
            infix_op,
            Box::new(right_expression),
            left_span,
        ))
    }

    fn parse_prefix_expression(&mut self) -> Result<Expression, CompilationError<'a>> {
        let prefix_operator_token = self.next_token()?;
        let prefix_op = PrefixOp::from(
            self.source_file
                .borrow()
                .span_text(&prefix_operator_token.span),
        );
        let expression = self.parse_expression(Precedence::Prefix)?;
        let expression_span = expression.span();

        Ok(Expression::Prefix(
            prefix_op,
            Box::new(expression),
            Span::from_spans(prefix_operator_token.span, expression_span),
        ))
    }

    fn apply_prefix_parse_fn(&mut self) -> Result<Expression, CompilationError<'a>> {
        let next_token = self.peek_token()?;
        match next_token.kind {
            TokenKind::Integer => self.parse_integer(),
            TokenKind::Float => self.parse_float(),
            TokenKind::Identifier => self.parse_identifier(),
            TokenKind::LParen => self.parse_grouped_expression(),
            TokenKind::KWTrue | TokenKind::KWFalse => self.parse_boolean(),
            TokenKind::Minus | TokenKind::Bang => self.parse_prefix_expression(),
            otherwise => Err(CompilationError::ParserError(
                ParserError::unknow_operator_in_expression(
                    otherwise,
                    next_token.span,
                    self.source_file,
                ),
            )),
        }
    }

    fn apply_infix_parse_fn(
        &mut self,
        left: Expression,
    ) -> Result<Expression, CompilationError<'a>> {
        let next_token = self.peek_token()?;
        match next_token.kind {
            TokenKind::Asterisk
            | TokenKind::Slash
            | TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Equal
            | TokenKind::EqualEqual
            | TokenKind::BangEqual
            | TokenKind::Less
            | TokenKind::LessEqual
            | TokenKind::Greater
            | TokenKind::GreaterEqual => self.parse_infix_expression(left),
            TokenKind::LParen => self.parse_function_call(left),
            otherwise => Err(CompilationError::ParserError(
                ParserError::unknow_operator_in_expression(
                    otherwise,
                    next_token.span,
                    self.source_file,
                ),
            )),
        }
    }

    fn parse_type_annotation(&mut self) -> Result<Type, CompilationError<'a>> {
        match self.peek_token_kind()? {
            TokenKind::Identifier => {
                let token = self.lexer.next().expect("Never fails");
                Ok(Type::Name(
                    self.source_file.borrow().span_text(&token.span).to_string(),
                ))
            }
            _ => todo!(),
        }
    }

    fn peek_token_kind(&mut self) -> Result<TokenKind, CompilationError<'a>> {
        self.peek_token().map(|t| t.kind)
    }

    fn peek_token(&mut self) -> Result<Token, CompilationError<'a>> {
        let next_token = self.lexer.peek();

        //TODO: maybe something like TokenStream should handle these?
        match next_token {
            None => Ok(Token {
                kind: TokenKind::Eof,
                span: self.source_file.borrow().end_of_file(),
            }),
            Some(token) => match token.kind {
                TokenKind::Illegal => Err(CompilationError::LexicalError(
                    LexicalError::unrecognized_token(token.span, self.source_file),
                )),
                TokenKind::UnbalancedQuote => Err(CompilationError::LexicalError(
                    LexicalError::unbalanced_quote(token.span, self.source_file),
                )),
                _ => Ok(next_token.expect("Should never fail").clone()),
            },
        }
    }

    fn match_peek_token_kind(&mut self, kind: TokenKind) -> Result<bool, CompilationError<'a>> {
        Ok(self.peek_token_kind()? == kind)
    }

    fn peek_precedence(&mut self) -> Result<Precedence, CompilationError<'a>> {
        Ok(Precedence::from(self.peek_token_kind()?))
    }

    fn next_token(&mut self) -> Result<Token, CompilationError<'a>> {
        //TODO: not ideal, consider implementing a self.check_token to skip peeking
        self.peek_token()?;
        Ok(self.lexer.next().expect("Should never fail"))
    }

    fn expect_and_next(&mut self, typ: TokenKind) -> Result<Token, CompilationError<'a>> {
        let next_token = self.peek_token()?;

        match next_token.kind {
            TokenKind::Eof => Err(CompilationError::ParserError(
                ParserError::unexpected_end_of_file(self.source_file),
            )),
            tk if tk == typ => Ok(self.lexer.next().expect("Should never fail")),
            tk => Err(CompilationError::ParserError(ParserError::expected_token(
                typ,
                tk,
                next_token.span,
                self.source_file,
            ))),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;

    use crate::source::SourceFile;

    use super::Parser;

    #[test]
    fn test_expression_operator_precedence() {
        let tests = vec![
            ("a + b", "(a + b)"),
            ("-a + b", "((-a) + b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4);\n((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            // (
            //     "a * [1, 2, 3, 4][b * c] * d",
            //     "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            // ),
            // (
            //     "add(a + b + c * d / f + g)",
            //     "add((((a + b) + ((c * d) / f)) + g))",
            // ),
            // (
            //     "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
            //     "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            // ),
            // ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
        ];

        for (code, expected) in tests {
            let source = RefCell::new(SourceFile::new(0, "test".to_string(), code));

            let mut parser = Parser::new(&source);
            let program = parser.parse();
        }
    }
}
