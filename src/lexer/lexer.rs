use crate::source::SourceFile;
use crate::token::Token;
use crate::{diagnostics::Diagnostics, token::TokenKind};
use std::cell::RefCell;
use std::{iter::Peekable, str::CharIndices};

pub struct Lexer<'a> {
    source_file: &'a RefCell<SourceFile<'a>>,
    source: Peekable<CharIndices<'a>>,
    last_pos: usize,
    diagnostics: &'a mut Diagnostics,
}

impl<'a> Lexer<'a> {
    pub fn new(source_file: &'a RefCell<SourceFile<'a>>, diagnostics: &'a mut Diagnostics) -> Self {
        Self {
            source_file,
            source: source_file.borrow().contents.char_indices().peekable(),
            last_pos: 0,
            diagnostics,
        }
    }

    fn eat_whitespaces(&mut self) {
        while let Some((i, ch)) = self.source.next_if(|(_, c)| c.is_whitespace()) {
            if ch == '\n' && self.source.peek().is_some() {
                self.source_file.borrow_mut().append_line_start(i + 1);
            }
            self.last_pos = i;
        }
    }

    fn make_token(&self, kind: TokenKind, offset_start: usize, offset_end: usize) -> Token {
        Token::new(
            kind,
            self.source_file
                .borrow()
                .make_span(offset_start, offset_end),
        )
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let mut token: Option<Token> = None;

        self.eat_whitespaces();

        if let Some((i, ch)) = self.source.next() {
            self.last_pos = i;

            match ch {
                ',' => token = Some(self.make_token(TokenKind::Comma, i, i + 1)),
                '/' => match self.source.peek() {
                    Some((_, '/')) => {
                        while let Some((k, _)) = self.source.next_if(|(_, c)| *c != '\n') {
                            self.last_pos = k;
                        }
                        token = Some(self.make_token(
                            TokenKind::SingleLineComment,
                            i,
                            self.last_pos + 1,
                        ));
                        // skip newline as well
                        self.source.next();
                    }
                    _ => token = Some(self.make_token(TokenKind::Slash, i, i + 1)),
                },
                '+' => token = Some(self.make_token(TokenKind::Plus, i, i + 1)),
                '-' => token = Some(self.make_token(TokenKind::Minus, i, i + 1)),
                '*' => token = Some(self.make_token(TokenKind::Asterisk, i, i + 1)),
                '>' => match self.source.peek() {
                    Some((_, '=')) => {
                        token = Some(self.make_token(TokenKind::GreaterEqual, i, i + 2))
                    }
                    _ => token = Some(self.make_token(TokenKind::Greater, i, i + 1)),
                },
                '<' => match self.source.peek() {
                    Some((_, '=')) => token = Some(self.make_token(TokenKind::LessEqual, i, i + 2)),
                    _ => token = Some(self.make_token(TokenKind::Less, i, i + 1)),
                },
                '=' => match self.source.peek() {
                    Some((_, '=')) => {
                        token = Some(self.make_token(TokenKind::EqualEqual, i, i + 2))
                    }
                    _ => token = Some(self.make_token(TokenKind::Equal, i, i + 1)),
                },
                '!' => match self.source.peek() {
                    Some((_, '=')) => token = Some(self.make_token(TokenKind::BangEqual, i, i + 2)),
                    _ => token = Some(self.make_token(TokenKind::Bang, i, i + 1)),
                },
                ';' => token = Some(self.make_token(TokenKind::Semicolon, i, i + 1)),
                ':' => token = Some(self.make_token(TokenKind::Colon, i, i + 1)),
                '(' => token = Some(self.make_token(TokenKind::LParen, i, i + 1)),
                ')' => token = Some(self.make_token(TokenKind::RParen, i, i + 1)),
                '{' => token = Some(self.make_token(TokenKind::LBrace, i, i + 1)),
                '}' => token = Some(self.make_token(TokenKind::RBrace, i, i + 1)),
                '"' => {
                    //TODO: could be potentially replaced with .by_ref().take_while().last()
                    while let Some((j, _)) = self.source.next_if(|(_, c)| *c != '"') {
                        self.last_pos = j;
                    }
                    //we're either looking at a " or reached the end of file
                    let next_peek = self.source.next();
                    if next_peek.is_some_and(|(_, c)| c == '"') {
                        token = Some(self.make_token(TokenKind::StringLit, i, self.last_pos + 2));
                    } else {
                        token =
                            Some(self.make_token(TokenKind::UnbalancedQuote, i, self.last_pos + 1))
                    }
                }
                c if c.is_numeric() => {
                    while let Some((j, _)) = self.source.next_if(|(_, c)| c.is_numeric()) {
                        self.last_pos = j;
                    }
                    //TODO: add float lexing
                    token = Some(self.make_token(TokenKind::Integer, i, self.last_pos + 1));
                }
                c if c.is_alphabetic() => todo!(),
                _ => unreachable!("Oh well"),
            }
        }

        token
    }
}

#[cfg(test)]
mod tests {}
