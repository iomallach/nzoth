use crate::source::SourceFile;
use crate::token::Token;
use crate::token::TokenKind;
use std::cell::RefCell;
use std::{iter::Peekable, str::CharIndices};

pub struct Lexer<'a> {
    source_file: &'a RefCell<SourceFile<'a>>,
    source: Peekable<CharIndices<'a>>,
    last_pos: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(source_file: &'a RefCell<SourceFile<'a>>) -> Self {
        Self {
            source_file,
            source: source_file.borrow().contents.char_indices().peekable(),
            last_pos: 0,
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

        if let Some((start_offset, ch)) = self.source.next() {
            self.last_pos = start_offset;

            match ch {
                ',' => {
                    token = Some(self.make_token(TokenKind::Comma, start_offset, start_offset + 1))
                }
                '/' => match self.source.peek() {
                    Some((_, '/')) => {
                        while let Some((k, _)) = self.source.next_if(|(_, c)| *c != '\n') {
                            self.last_pos = k;
                        }
                        token = Some(self.make_token(
                            TokenKind::SingleLineComment,
                            start_offset,
                            self.last_pos + 1,
                        ));
                        // skip newline as well
                        self.source.next();
                    }
                    _ => {
                        token =
                            Some(self.make_token(TokenKind::Slash, start_offset, start_offset + 1))
                    }
                },
                '+' => match self.source.peek() {
                    Some((j, '+')) => {
                        let end_offset = j + 1;
                        token =
                            Some(self.make_token(TokenKind::PlusPlus, start_offset, end_offset));
                        self.source.next();
                    }
                    _ => {
                        token =
                            Some(self.make_token(TokenKind::Plus, start_offset, start_offset + 1))
                    }
                },
                '-' => match self.source.peek() {
                    Some((j, '-')) => {
                        let end_offset = j + 1;
                        token =
                            Some(self.make_token(TokenKind::MinusMinus, start_offset, end_offset));
                        self.source.next();
                    }
                    _ => {
                        token =
                            Some(self.make_token(TokenKind::Minus, start_offset, start_offset + 1))
                    }
                },
                '*' => {
                    token =
                        Some(self.make_token(TokenKind::Asterisk, start_offset, start_offset + 1))
                }
                '>' => match self.source.peek() {
                    Some((j, '=')) => {
                        let end_offset = j + 1;
                        token = Some(self.make_token(
                            TokenKind::GreaterEqual,
                            start_offset,
                            end_offset,
                        ));
                        self.source.next();
                    }
                    _ => {
                        token = Some(self.make_token(
                            TokenKind::Greater,
                            start_offset,
                            start_offset + 1,
                        ))
                    }
                },
                '<' => match self.source.peek() {
                    Some((j, '=')) => {
                        let end_offset = j + 1;
                        token =
                            Some(self.make_token(TokenKind::LessEqual, start_offset, end_offset));
                        self.source.next();
                    }
                    _ => {
                        token =
                            Some(self.make_token(TokenKind::Less, start_offset, start_offset + 1))
                    }
                },
                '=' => match self.source.peek() {
                    Some((j, '=')) => {
                        let end_offset = j + 1;
                        token =
                            Some(self.make_token(TokenKind::EqualEqual, start_offset, end_offset));
                        self.source.next();
                    }
                    _ => {
                        token =
                            Some(self.make_token(TokenKind::Equal, start_offset, start_offset + 1))
                    }
                },
                '!' => match self.source.peek() {
                    Some((j, '=')) => {
                        let end_offset = j + 1;
                        token =
                            Some(self.make_token(TokenKind::BangEqual, start_offset, end_offset));
                        self.source.next();
                    }
                    _ => {
                        token =
                            Some(self.make_token(TokenKind::Bang, start_offset, start_offset + 1))
                    }
                },
                ';' => {
                    token =
                        Some(self.make_token(TokenKind::Semicolon, start_offset, start_offset + 1))
                }
                ':' => {
                    if let Some((j, ':')) = self.source.peek() {
                        let end_offset = j + 1;
                        token =
                            Some(self.make_token(TokenKind::ColonColon, start_offset, end_offset));
                    } else {
                        token = Some(self.make_token(
                            TokenKind::Illegal,
                            start_offset,
                            start_offset + 1,
                        ));
                    }
                }
                '(' => {
                    token = Some(self.make_token(TokenKind::LParen, start_offset, start_offset + 1))
                }
                ')' => {
                    token = Some(self.make_token(TokenKind::RParen, start_offset, start_offset + 1))
                }
                '{' => {
                    token = Some(self.make_token(TokenKind::LBrace, start_offset, start_offset + 1))
                }
                '}' => {
                    token = Some(self.make_token(TokenKind::RBrace, start_offset, start_offset + 1))
                }
                '"' => {
                    //TODO: could be potentially replaced with .by_ref().take_while().last()
                    while let Some((j, _)) = self.source.next_if(|(_, c)| *c != '"') {
                        self.last_pos = j;
                    }
                    //we're either looking at a " or reached the end of file
                    let next = self.source.next();
                    if next.is_some_and(|(_, c)| c == '"') {
                        token = Some(self.make_token(
                            TokenKind::StringLit,
                            start_offset,
                            self.last_pos + 2,
                        ));
                    } else {
                        token = Some(self.make_token(
                            TokenKind::UnbalancedQuote,
                            start_offset,
                            self.last_pos + 1,
                        ))
                    }
                }
                c if c.is_numeric() => {
                    while let Some((j, _)) = self.source.next_if(|(_, c)| c.is_numeric()) {
                        self.last_pos = j;
                    }
                    // handle cases like 134ab, which is illegal
                    if self.source.peek().is_some_and(|(_, c)| c.is_alphabetic()) {
                        //TODO: ^ is wrong, we should rather look for an alphabetic and mark it
                        //illegal. Also consume until not alphanumeric
                        token = Some(self.make_token(
                            TokenKind::Illegal,
                            start_offset,
                            self.last_pos + 2,
                        ));
                    } else {
                        token = Some(self.make_token(
                            TokenKind::Integer,
                            start_offset,
                            self.last_pos + 1,
                        ));
                    }
                    //TODO: add float lexing
                }
                c if c.is_alphabetic() => {
                    while let Some((j, _)) = self.source.next_if(|(_, c)| c.is_alphanumeric()) {
                        self.last_pos = j;
                    }
                    let token_span = self
                        .source_file
                        .borrow()
                        .make_span(start_offset, self.last_pos + 1);
                    match self.source_file.borrow().span_text(&token_span) {
                        "let" => {
                            token = Some(self.make_token(
                                TokenKind::KWLet,
                                start_offset,
                                self.last_pos + 1,
                            ));
                        }
                        "true" => {
                            token = Some(self.make_token(
                                TokenKind::KWTrue,
                                start_offset,
                                self.last_pos + 1,
                            ));
                        }
                        "false" => {
                            token = Some(self.make_token(
                                TokenKind::KWFalse,
                                start_offset,
                                self.last_pos + 1,
                            ));
                        }
                        "int" => {
                            token = Some(self.make_token(
                                TokenKind::KWInt,
                                start_offset,
                                self.last_pos + 1,
                            ));
                        }
                        _ => {
                            token = Some(self.make_token(
                                TokenKind::Identifier,
                                start_offset,
                                self.last_pos + 1,
                            ));
                        }
                    }
                }
                _ => {
                    token =
                        Some(self.make_token(TokenKind::Illegal, start_offset, start_offset + 1));
                }
            }
        }

        token
    }
}

#[cfg(test)]
mod tests {
    use crate::source::SourceFile;

    use super::*;

    struct TokenTestCase {
        pub input: &'static str,
        pub expected: TokenKind,
        pub expected_literal: &'static str,
    }

    #[test]
    fn test_happypath_individual_tokens() {
        let test_source = vec![
            TokenTestCase {
                input: ",",
                expected: TokenKind::Comma,
                expected_literal: ",",
            },
            TokenTestCase {
                input: "/",
                expected: TokenKind::Slash,
                expected_literal: "/",
            },
            TokenTestCase {
                input: "//comment\n",
                expected: TokenKind::SingleLineComment,
                expected_literal: "//comment",
            },
            TokenTestCase {
                input: "//comment",
                expected: TokenKind::SingleLineComment,
                expected_literal: "//comment",
            },
            TokenTestCase {
                input: "+",
                expected: TokenKind::Plus,
                expected_literal: "+",
            },
            TokenTestCase {
                input: "-",
                expected: TokenKind::Minus,
                expected_literal: "-",
            },
            TokenTestCase {
                input: "*",
                expected: TokenKind::Asterisk,
                expected_literal: "*",
            },
            TokenTestCase {
                input: ">",
                expected: TokenKind::Greater,
                expected_literal: ">",
            },
            TokenTestCase {
                input: ">=",
                expected: TokenKind::GreaterEqual,
                expected_literal: ">=",
            },
            TokenTestCase {
                input: "<",
                expected: TokenKind::Less,
                expected_literal: "<",
            },
            TokenTestCase {
                input: "<=",
                expected: TokenKind::LessEqual,
                expected_literal: "<=",
            },
            TokenTestCase {
                input: "=",
                expected: TokenKind::Equal,
                expected_literal: "=",
            },
            TokenTestCase {
                input: "==",
                expected: TokenKind::EqualEqual,
                expected_literal: "==",
            },
            TokenTestCase {
                input: "!",
                expected: TokenKind::Bang,
                expected_literal: "!",
            },
            TokenTestCase {
                input: "!=",
                expected: TokenKind::BangEqual,
                expected_literal: "!=",
            },
            TokenTestCase {
                input: ";",
                expected: TokenKind::Semicolon,
                expected_literal: ";",
            },
            TokenTestCase {
                input: "::",
                expected: TokenKind::ColonColon,
                expected_literal: "::",
            },
            TokenTestCase {
                input: ":",
                expected: TokenKind::Illegal,
                expected_literal: ":",
            },
            TokenTestCase {
                input: "(",
                expected: TokenKind::LParen,
                expected_literal: "(",
            },
            TokenTestCase {
                input: ")",
                expected: TokenKind::RParen,
                expected_literal: ")",
            },
            TokenTestCase {
                input: "{",
                expected: TokenKind::LBrace,
                expected_literal: "{",
            },
            TokenTestCase {
                input: "}",
                expected: TokenKind::RBrace,
                expected_literal: "}",
            },
            TokenTestCase {
                input: "\"string_lit\"",
                expected: TokenKind::StringLit,
                expected_literal: "\"string_lit\"",
            },
            TokenTestCase {
                input: "\"unbalanced",
                expected: TokenKind::UnbalancedQuote,
                expected_literal: "\"unbalanced",
            },
            TokenTestCase {
                input: "1234",
                expected: TokenKind::Integer,
                expected_literal: "1234",
            },
            TokenTestCase {
                input: "124ab", //FIXME: actual = 124a, wrong
                expected: TokenKind::Illegal,
                expected_literal: "124a",
            },
            TokenTestCase {
                input: "12!",
                expected: TokenKind::Integer,
                expected_literal: "12",
            },
            TokenTestCase {
                input: "12,",
                expected: TokenKind::Integer,
                expected_literal: "12",
            },
            TokenTestCase {
                input: "++",
                expected: TokenKind::PlusPlus,
                expected_literal: "++",
            },
            TokenTestCase {
                input: "--",
                expected: TokenKind::MinusMinus,
                expected_literal: "--",
            },
            TokenTestCase {
                input: "foobar",
                expected: TokenKind::Identifier,
                expected_literal: "foobar",
            },
            TokenTestCase {
                input: "let",
                expected: TokenKind::KWLet,
                expected_literal: "let",
            },
            TokenTestCase {
                input: "true",
                expected: TokenKind::KWTrue,
                expected_literal: "true",
            },
            TokenTestCase {
                input: "false",
                expected: TokenKind::KWFalse,
                expected_literal: "false",
            },
            TokenTestCase {
                input: "int",
                expected: TokenKind::KWInt,
                expected_literal: "int",
            },
        ];

        for s in test_source.into_iter() {
            let source = RefCell::new(SourceFile::new(0, "test".to_string(), s.input));
            let mut lexer = Lexer::new(&source);
            let token = lexer
                .next()
                .expect(&format!("Unexpected end of file while lexing {}", s.input));

            assert_eq!(s.expected, token.kind);
            assert_eq!(s.expected_literal, source.borrow().span_text(&token.span));
        }
    }

    //TODO: add a comprehensive integration test for a code piece once the lexer is complete
}
