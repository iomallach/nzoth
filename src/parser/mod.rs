use std::cell::RefCell;
use std::iter::Peekable;

use crate::ast::{
    AstNode, Block, Expression, ExpressionStatement, InfixOp, LetDeclaration, NumericLiteral,
    Precedence, PrefixOp, Program, Statement, Type,
};
use crate::lexer::lexer::Lexer;
use crate::source::{SourceFile, Span};
use crate::token::{Token, TokenKind};

type PrefixParseFn = Box<dyn FnMut(&mut Parser) -> Result<Expression, Error>>;
type InfixParseFn = Box<dyn FnMut(&mut Parser, Expression) -> Result<Expression, Error>>;

pub enum Error {
    Eof,
    Unexpected { expected: String, got: Span },
}

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    source_file: &'a RefCell<SourceFile<'a>>,
    diagnostics: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a RefCell<SourceFile<'a>>) -> Self {
        let lexer = Lexer::new(source).peekable();
        Self {
            lexer,
            source_file: source,
            diagnostics: vec![],
        }
    }

    pub fn parse(&mut self) -> Program {
        let mut program = Program { nodes: vec![] };

        while self.lexer.peek().is_some() {
            match self.parse_node() {
                Ok(node) => program.push(node),
                Err(_) => todo!(),
            }
        }
        program
    }

    pub fn parse_node(&mut self) -> Result<AstNode, Error> {
        match self.peek_token_kind() {
            TokenKind::KWLet => self.parse_let_declaration(),
            TokenKind::Eof => Err(Error::Eof),
            _ => self.parse_expression_node(),
        }
    }

    pub fn parse_expression_node(&mut self) -> Result<AstNode, Error> {
        let start_token = self
            .lexer
            .peek()
            .copied()
            .expect("Never called before peek check");
        let expression = self.parse_expression(Precedence::Lowest)?;

        if self.match_peek_token_kind(TokenKind::Semicolon) {
            self.lexer.next();
            Ok(AstNode::Statement(Statement::Expression(
                ExpressionStatement {
                    span: start_token.span,
                    expression,
                },
            )))
        } else {
            Ok(AstNode::Expression(expression))
        }
    }

    fn parse_let_declaration(&mut self) -> Result<AstNode, Error> {
        let let_token = self.lexer.next().expect("Never called before peek check");

        let identifier_token = self.expect_and_next(TokenKind::Identifier)?;
        //TODO: check :: and parse type

        let ty = if self.match_peek_token_kind(TokenKind::ColonColon) {
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
        Ok(AstNode::Statement(Statement::LetDeclaration(
            LetDeclaration {
                identifier: Expression::Identifier(
                    sf.span_text(&identifier_token.span).to_string(),
                    identifier_token.span,
                ),
                expression,
                span: Span {
                    start: let_token.span.start,
                    end: expression_span.end,
                    file: sf.id,
                },
                ty,
            },
        )))
    }

    // TODO: 0% test coverage
    fn parse_block(&mut self) -> Result<AstNode, Error> {
        let mut nodes = vec![];
        let opening_curly_brace = self.lexer.next().expect("Checked");
        let mut last_expression = None;

        // TODO: edge cases: empty statements, no statements
        while !self.match_peek_token_kind(TokenKind::RBrace)
            && !self.match_peek_token_kind(TokenKind::Eof)
        {
            //FIXME: sync parser on error
            let node = self.parse_node()?;
            if let AstNode::Expression(_) = node {
                if last_expression.is_some() {
                    //TODO: record error, sync parser
                    todo!();
                    continue;
                }
                last_expression = Some(node);
            } else {
                nodes.push(node);
            }
        }

        let closing_curly = self.expect_and_next(TokenKind::Semicolon)?;

        Ok(AstNode::Statement(Statement::Block(Block {
            nodes,
            last_expression: last_expression.map(|le| Box::new(le)),
            span: Span::from_spans(opening_curly_brace.span, closing_curly.span),
        })))
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, Error> {
        let mut left_expression = self.prefix_parse_fn()?(self)?;

        while !self.match_peek_token_kind(TokenKind::Semicolon)
            && precedence < self.peek_precedence()
        {
            left_expression = self.infix_parse_fn()?(self, left_expression)?;
        }

        Ok(left_expression)
    }

    fn parse_integer(&mut self) -> Result<Expression, Error> {
        let int_token = self.lexer.next().ok_or(Error::Eof)?;
        let sf = self.source_file.borrow();
        let int_literal = sf.span_text(&int_token.span);

        Ok(Expression::NumericLiteral(
            NumericLiteral::Integer(int_literal.parse().expect("Lexer failed")),
            int_token.span,
        ))
    }

    fn parse_identifier(&mut self) -> Result<Expression, Error> {
        self.lexer.next().map_or(Err(Error::Eof), |t| {
            Ok(Expression::Identifier(
                self.source_file.borrow().span_text(&t.span).to_string(),
                t.span,
            ))
        })
    }

    fn parse_boolean(&mut self) -> Result<Expression, Error> {
        let boolean_token = self.lexer.next().expect("Never called before peeking");

        if let TokenKind::KWTrue = boolean_token.kind {
            Ok(Expression::Bool(true, boolean_token.span))
        } else {
            Ok(Expression::Bool(false, boolean_token.span))
        }
    }

    //TODO: test coverage 0%
    fn parse_grouped_expression(&mut self) -> Result<Expression, Error> {
        let l_paren_token = self.lexer.next().expect("Never called before peeking");
        let grouped_expression = self.parse_expression(Precedence::Lowest)?;
        let grouped_expression_span = grouped_expression.span();

        self.expect_and_next(TokenKind::RParen)?;

        Ok(Expression::Grouped(
            Box::new(grouped_expression),
            Span::from_spans(l_paren_token.span, grouped_expression_span),
        ))
    }

    fn parse_infix_expression(&mut self, left_expression: Expression) -> Result<Expression, Error> {
        let current_token = self.lexer.next().expect("Peek checked before calling");
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

    fn parse_prefix_expression(&mut self) -> Result<Expression, Error> {
        let prefix_operator_token = self.lexer.next().expect("Always checked");
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

    fn prefix_parse_fn(&mut self) -> Result<PrefixParseFn, Error> {
        if let Some(token) = self.lexer.peek() {
            match token.kind {
                TokenKind::Integer => Ok(Box::new(|parser| parser.parse_integer())),
                TokenKind::Identifier => Ok(Box::new(|parser| parser.parse_identifier())),
                TokenKind::LParen => Ok(Box::new(|parser| parser.parse_grouped_expression())),
                TokenKind::KWTrue | TokenKind::KWFalse => {
                    Ok(Box::new(|parser| parser.parse_boolean()))
                }
                TokenKind::Minus | TokenKind::Bang => {
                    Ok(Box::new(|parser| parser.parse_prefix_expression()))
                }
                otherwise => {
                    eprintln!("Unexpected token of kind = {}", otherwise);
                    todo!();
                }
            }
        } else {
            Err(Error::Eof)
        }
    }

    fn infix_parse_fn(&mut self) -> Result<InfixParseFn, Error> {
        if let Some(token) = self.lexer.peek() {
            match token.kind {
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
                | TokenKind::GreaterEqual => {
                    Ok(Box::new(|parser, left| parser.parse_infix_expression(left)))
                }
                otherwise => todo!("Not yet implemented for {otherwise}"),
            }
        } else {
            Err(Error::Eof)
        }
    }

    fn parse_type_annotation(&mut self) -> Result<Type, Error> {
        match self.peek_token_kind() {
            TokenKind::KWInt => {
                self.lexer.next();
                Ok(Type::Integer)
            }
            _ => todo!(),
        }
    }

    fn peek_token_kind(&mut self) -> TokenKind {
        self.lexer.peek().map_or(TokenKind::Eof, |t| t.kind)
    }
    fn match_peek_token_kind(&mut self, kind: TokenKind) -> bool {
        self.peek_token_kind() == kind
    }

    fn peek_precedence(&mut self) -> Precedence {
        Precedence::from(self.peek_token_kind())
    }

    fn expect_and_next(&mut self, typ: TokenKind) -> Result<Token, Error> {
        let next_token = self.lexer.peek().ok_or(Error::Eof).copied()?;
        if next_token.kind == typ {
            Ok(self.lexer.next().unwrap())
        } else {
            Err(Error::Unexpected {
                expected: typ.to_string(),
                got: next_token.span,
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use std::cell::RefCell;

    use crate::{
        ast::{
            AstNode, Expression, InfixOp, NumericLiteral, PrefixOp, Statement, Type,
            visitor::ProgramPrinter,
        },
        lexer::lexer::Lexer,
        source::{SourceFile, Span},
    };

    use super::Parser;

    fn test_expression(expected: &Expression, actual: &Expression, code_context: &str) {
        match (expected, actual) {
            (
                Expression::NumericLiteral(NumericLiteral::Integer(e), _),
                Expression::NumericLiteral(NumericLiteral::Integer(a), _),
            ) => assert_eq!(
                e, a,
                "Test failed for {code_context}, expected {e}, got {a}"
            ),
            (Expression::Identifier(expected_name, _), Expression::Identifier(actual_name, _)) => {
                assert_eq!(
                    expected_name, actual_name,
                    "Test failed for {code_context}, expected {expected_name}, got {actual_name}"
                )
            }
            (Expression::Bool(bool_expected, _), Expression::Bool(bool_actual, _)) => {
                assert_eq!(
                    bool_expected, bool_actual,
                    "Test failed for {code_context}, expected {bool_expected}, got {bool_actual}"
                )
            }
            _ => unreachable!(),
        }
    }

    fn test_identifier(expected: &str, actual: &Expression) {
        if let Expression::Identifier(actual_name, _) = actual {
            assert_eq!(expected, actual_name);
        } else {
            unreachable!();
        }
    }

    #[test]
    fn test_happypath_parser_let_declarations() {
        let tests = vec![
            (
                "let foo = 1;",
                "foo",
                Expression::NumericLiteral(NumericLiteral::Integer(1), Span::default()),
                None,
            ),
            (
                "let foo :: int = 1;",
                "foo",
                Expression::NumericLiteral(NumericLiteral::Integer(1), Span::default()),
                Some(Type::Integer),
            ),
        ];

        for (s, ident, expr, ty) in tests {
            let source = RefCell::new(SourceFile::new(0, "test".to_string(), s));

            let mut tokens = Vec::new();
            let mut lexer = Lexer::new(&source);
            while let Some(token) = lexer.next() {
                tokens.push(token);
            }

            let mut parser = Parser::new(&source);
            let program = parser.parse();

            assert_eq!(1, program.nodes.len());
            if let AstNode::Statement(Statement::LetDeclaration(ld)) = program.nodes.get(0).unwrap()
            {
                test_identifier(ident, &ld.identifier);
                test_expression(&expr, &ld.expression, s);
                assert_eq!(ty, ld.ty);
            } else {
                unreachable!();
            }
        }
    }

    #[test]
    fn test_happypath_parse_identifier() {
        let tests = vec![("foo;", "foo"), ("bar;", "bar"), ("baz", "baz")];

        for (code, expected) in tests {
            let source = RefCell::new(SourceFile::new(0, "test".to_string(), code));

            let mut parser = Parser::new(&source);
            let program = parser.parse();

            assert_eq!(1, program.nodes.len());

            match program.nodes.get(0).unwrap() {
                AstNode::Statement(Statement::Expression(es)) => {
                    test_identifier(expected, &es.expression)
                }
                AstNode::Expression(expression) => test_identifier(expected, expression),
                _ => unreachable!(),
            }
        }
    }

    #[test]
    fn test_happypath_parse_assignments() {
        let tests = vec![
            (
                "foo = bar;",
                InfixOp::Assignment,
                Expression::Identifier("foo".to_string(), Span::default()),
                Expression::Identifier("bar".to_string(), Span::default()),
            ),
            (
                "foo = 3;",
                InfixOp::Assignment,
                Expression::Identifier("foo".to_string(), Span::default()),
                Expression::NumericLiteral(NumericLiteral::Integer(3), Span::default()),
            ),
        ];

        for (code, expected_op, expected_left, expected_right) in tests {
            let source = RefCell::new(SourceFile::new(0, "test".to_string(), code));

            let mut parser = Parser::new(&source);
            let program = parser.parse();

            assert_eq!(1, program.nodes.len());
            let expr = match program.nodes.into_iter().next().unwrap() {
                AstNode::Statement(Statement::Expression(es)) => es.expression,
                AstNode::Expression(expression) => expression,
                _ => unreachable!("Expected an infix expression"),
            };

            if let Expression::Infix(left, op, right, _) = &expr {
                test_expression(&expected_left, left, code);
                test_expression(&expected_right, right, code);
                assert_eq!(*op, expected_op);
            }
        }
    }

    #[test]
    fn test_happypath_parse_prefix_expressions() {
        let tests = vec![
            (
                "-5;",
                Expression::NumericLiteral(NumericLiteral::Integer(5), Span::default()),
                PrefixOp::Negation,
            ),
            (
                "!true;",
                Expression::Bool(true, Span::default()),
                PrefixOp::BoolNegation,
            ),
            (
                "!false;",
                Expression::Bool(false, Span::default()),
                PrefixOp::BoolNegation,
            ),
        ];

        for (code, expected_expr, expected_op) in tests {
            let source = RefCell::new(SourceFile::new(0, "test".to_string(), code));

            let mut parser = Parser::new(&source);
            let program = parser.parse();

            assert_eq!(1, program.nodes.len());

            let es = if let AstNode::Statement(Statement::Expression(es)) =
                program.nodes.into_iter().next().unwrap()
            {
                es.expression
            } else {
                unreachable!("Expected expression statemenet");
            };

            if let Expression::Prefix(op, expr, _) = es {
                test_expression(&expected_expr, &expr, code);
                assert_eq!(expected_op, op);
            } else {
                unreachable!("Expected a prefix expression");
            }
        }
    }

    #[test]
    fn test_happypath_parse_boolean_expressions() {
        let tests = vec![
            ("true;", Expression::Bool(true, Span::default())),
            ("false;", Expression::Bool(false, Span::default())),
        ];

        for (code, expected_expr) in tests {
            let source = RefCell::new(SourceFile::new(0, "test".to_string(), code));

            let mut parser = Parser::new(&source);
            let program = parser.parse();

            assert_eq!(1, program.nodes.len());

            let es = if let AstNode::Statement(Statement::Expression(es)) =
                program.nodes.into_iter().next().unwrap()
            {
                es.expression
            } else {
                unreachable!("Expected expression statemenet");
            };

            test_expression(&expected_expr, &es, code);
        }
    }

    #[test]
    fn test_happypath_parse_infix_expressions() {
        let tests = vec![
            (
                "5 + 5;",
                InfixOp::Add,
                Expression::NumericLiteral(NumericLiteral::Integer(5), Span::default()),
                Expression::NumericLiteral(NumericLiteral::Integer(5), Span::default()),
            ),
            (
                "5 - 5;",
                InfixOp::Subtract,
                Expression::NumericLiteral(NumericLiteral::Integer(5), Span::default()),
                Expression::NumericLiteral(NumericLiteral::Integer(5), Span::default()),
            ),
            (
                "5 * 5;",
                InfixOp::Multiply,
                Expression::NumericLiteral(NumericLiteral::Integer(5), Span::default()),
                Expression::NumericLiteral(NumericLiteral::Integer(5), Span::default()),
            ),
            (
                "5 / 5;",
                InfixOp::Divide,
                Expression::NumericLiteral(NumericLiteral::Integer(5), Span::default()),
                Expression::NumericLiteral(NumericLiteral::Integer(5), Span::default()),
            ),
            (
                "5 != 5;",
                InfixOp::NotEquals,
                Expression::NumericLiteral(NumericLiteral::Integer(5), Span::default()),
                Expression::NumericLiteral(NumericLiteral::Integer(5), Span::default()),
            ),
            (
                "5 == 5;",
                InfixOp::Equals,
                Expression::NumericLiteral(NumericLiteral::Integer(5), Span::default()),
                Expression::NumericLiteral(NumericLiteral::Integer(5), Span::default()),
            ),
            (
                "5 > 5;",
                InfixOp::GreaterThan,
                Expression::NumericLiteral(NumericLiteral::Integer(5), Span::default()),
                Expression::NumericLiteral(NumericLiteral::Integer(5), Span::default()),
            ),
            (
                "5 < 5;",
                InfixOp::LessThan,
                Expression::NumericLiteral(NumericLiteral::Integer(5), Span::default()),
                Expression::NumericLiteral(NumericLiteral::Integer(5), Span::default()),
            ),
            (
                "5 >= 5;",
                InfixOp::GreaterThanEquals,
                Expression::NumericLiteral(NumericLiteral::Integer(5), Span::default()),
                Expression::NumericLiteral(NumericLiteral::Integer(5), Span::default()),
            ),
            (
                "5 <= 5;",
                InfixOp::LessThanEquals,
                Expression::NumericLiteral(NumericLiteral::Integer(5), Span::default()),
                Expression::NumericLiteral(NumericLiteral::Integer(5), Span::default()),
            ),
            (
                "true != false;",
                InfixOp::NotEquals,
                Expression::Bool(true, Span::default()),
                Expression::Bool(false, Span::default()),
            ),
            (
                "false != true;",
                InfixOp::NotEquals,
                Expression::Bool(false, Span::default()),
                Expression::Bool(true, Span::default()),
            ),
            (
                "true != true;",
                InfixOp::NotEquals,
                Expression::Bool(true, Span::default()),
                Expression::Bool(true, Span::default()),
            ),
            (
                "false != false;",
                InfixOp::NotEquals,
                Expression::Bool(false, Span::default()),
                Expression::Bool(false, Span::default()),
            ),
            (
                "true == false;",
                InfixOp::Equals,
                Expression::Bool(true, Span::default()),
                Expression::Bool(false, Span::default()),
            ),
            (
                "false == true;",
                InfixOp::Equals,
                Expression::Bool(false, Span::default()),
                Expression::Bool(true, Span::default()),
            ),
            (
                "false == false;",
                InfixOp::Equals,
                Expression::Bool(false, Span::default()),
                Expression::Bool(false, Span::default()),
            ),
            (
                "true == true;",
                InfixOp::Equals,
                Expression::Bool(true, Span::default()),
                Expression::Bool(true, Span::default()),
            ),
        ];

        for (code, expected_op, expected_left, expected_right) in tests {
            let source = RefCell::new(SourceFile::new(0, "test".to_string(), code));

            let mut parser = Parser::new(&source);
            let program = parser.parse();

            assert_eq!(
                1,
                program.nodes.len(),
                "Test failed for {code}, expected 1 nodes, found {}",
                program.nodes.len()
            );

            let expr = if let AstNode::Statement(Statement::Expression(es)) =
                program.nodes.into_iter().next().unwrap()
            {
                es.expression
            } else {
                panic!("Test failed for {code}: not an expression statement")
            };

            if let Expression::Infix(left, op, right, _) = expr {
                test_expression(&expected_right, &right, code);
                test_expression(&expected_left, &left, code);
                assert_eq!(expected_op, op);
            } else {
                panic!("Test failed for {code}: not an infix expression")
            }
        }
    }

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

            assert_eq!(expected, ProgramPrinter::print(&program));
        }
    }
}
