use crate::token::{Span, Token};
use crate::{diagnostics::Diagnostics, token::TokenKind};
use std::{iter::Peekable, str::CharIndices};

pub struct Lexer<'a> {
    source: Peekable<CharIndices<'a>>,
    last_pos: usize,
    diagnostics: &'a mut Diagnostics,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str, diagnostics: &'a mut Diagnostics) -> Self {
        Self {
            source: source.char_indices().peekable(),
            last_pos: 0,
            diagnostics,
        }
    }

    fn eat_whitespaces(&mut self) {
        while let Some((i, _)) = self.source.next_if(|(_, c)| c.is_whitespace()) {
            self.last_pos = i;
        }
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
                ',' => token = Some(Token::new(TokenKind::Comma, Span::new(i, i + 1))),
                '/' => match self.source.peek() {
                    Some((_, '/')) => {
                        while let Some((k, _)) = self.source.next_if(|(_, c)| *c != '\n') {
                            self.last_pos = k;
                        }
                        token = Some(Token::new(
                            TokenKind::SingleLineComment,
                            Span::new(i, self.last_pos),
                        ));
                        // skip newline as well
                        self.source.next();
                    }
                    _ => token = Some(Token::new(TokenKind::Slash, Span::new(i, i + 1))),
                },
                '+' => token = Some(Token::new(TokenKind::Plus, Span::new(i, i + 1))),
                '-' => token = Some(Token::new(TokenKind::Minus, Span::new(i, i + 1))),
                '*' => token = Some(Token::new(TokenKind::Asterisk, Span::new(i, i + 1))),
                '>' => match self.source.peek() {
                    Some((_, '=')) => {
                        token = Some(Token::new(TokenKind::GreaterEqual, Span::new(i, i + 2)))
                    }
                    _ => token = Some(Token::new(TokenKind::Greater, Span::new(i, i + 1))),
                },
                '<' => match self.source.peek() {
                    Some((_, '=')) => {
                        token = Some(Token::new(TokenKind::LessEqual, Span::new(i, i + 2)))
                    }
                    _ => token = Some(Token::new(TokenKind::Less, Span::new(i, i + 1))),
                },
                '=' => match self.source.peek() {
                    Some((_, '=')) => {
                        token = Some(Token::new(TokenKind::EqualEqual, Span::new(i, i + 2)))
                    }
                    _ => token = Some(Token::new(TokenKind::Equal, Span::new(i, i + 1))),
                },
                '!' => match self.source.peek() {
                    Some((_, '=')) => {
                        token = Some(Token::new(TokenKind::BangEqual, Span::new(i, i + 2)))
                    }
                    _ => token = Some(Token::new(TokenKind::Bang, Span::new(i, i + 1))),
                },
                ';' => token = Some(Token::new(TokenKind::Semicolon, Span::new(i, i + 1))),
                ':' => token = Some(Token::new(TokenKind::Colon, Span::new(i, i + 1))),
                '(' => token = Some(Token::new(TokenKind::LParen, Span::new(i, i + 1))),
                ')' => token = Some(Token::new(TokenKind::RParen, Span::new(i, i + 1))),
                '{' => token = Some(Token::new(TokenKind::LBrace, Span::new(i, i + 1))),
                '}' => token = Some(Token::new(TokenKind::RBrace, Span::new(i, i + 1))),
                '"' => todo!(),
                c if c.is_numeric() => todo!(),
                c if c.is_alphabetic() => todo!(),
                _ => unreachable!("Oh well"),
            }
        }

        token
    }
}

#[cfg(test)]
mod tests {}
