use std::cell::RefCell;
use std::iter::Peekable;

use crate::ast::{AstNode, Expression, InfixOp, NumericLiteral, Precedence, Program};
use crate::diagnostics::Diagnostics;
use crate::lexer::lexer::Lexer;
use crate::source::{SourceFile, Span};
use crate::token::{Token, TokenKind};

type PrefixParseFn<'a> = Box<dyn FnMut(&mut Parser) -> Result<Expression, Error>>;
type InfixParseFn<'a> = Box<dyn FnMut(&mut Parser, Expression) -> Result<Expression, Error>>;

pub enum Error {
    Eof,
    Unexpected { expected: String, got: Span },
}

pub struct Parser<'a> {
    lexer: Peekable<Lexer<'a>>,
    source_file: &'a RefCell<SourceFile<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a RefCell<SourceFile<'a>>, diagnostics: &'a mut Diagnostics) -> Self {
        let lexer = Lexer::new(source, diagnostics).peekable();
        Self {
            lexer,
            source_file: source,
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
            TokenKind::KWLet => todo!(),
            TokenKind::Eof => Err(Error::Eof),
            _ => todo!(),
        }
    }

    fn parse_let_declaration(&mut self) -> Result<AstNode, Error> {
        let let_token = self.lexer.next().expect("Never called before peek check");
        let ident_token = self.expect_and_next(TokenKind::Identifier)?;
        //TODO: check :: and parse type
        let equals_token = self.expect_and_next(TokenKind::Equal)?;
        todo!("parse expression");
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Result<Expression, Error> {
        let mut left_expression = self.prefix_parse_fn()?(self)?;
        // while not semicolon and prec < peek prec
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

    fn prefix_parse_fn(&mut self) -> Result<PrefixParseFn<'a>, Error> {
        if let Some(token) = self.lexer.next() {
            match token.kind {
                TokenKind::Integer => Ok(Box::new(|parser| parser.parse_integer())),
                _ => todo!(),
            }
        } else {
            Err(Error::Eof)
        }
    }

    fn infix_parse_fn(&mut self) -> Result<InfixParseFn<'a>, Error> {
        if let Some(token) = self.lexer.peek() {
            match token.kind {
                TokenKind::Plus => Ok(Box::new(|parser, left| parser.parse_infix_expression(left))),
                _ => todo!(),
            }
        } else {
            Err(Error::Eof)
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
