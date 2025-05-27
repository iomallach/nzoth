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
                    token = Some(self.make_token(TokenKind::Colon, start_offset, start_offset + 1))
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
                    if self.source.peek().is_some_and(|(_, c)| !c.is_whitespace()) {
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
                            ))
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
    }

    #[test]
    fn test_happypath_individual_tokens() {
        let test_source = vec![
            TokenTestCase {
                input: ",",
                expected: TokenKind::Comma,
            },
            TokenTestCase {
                input: "/",
                expected: TokenKind::Slash,
            },
            TokenTestCase {
                input: "//comment\n",
                expected: TokenKind::SingleLineComment,
            },
            TokenTestCase {
                input: "//comment",
                expected: TokenKind::SingleLineComment,
            },
            TokenTestCase {
                input: "+",
                expected: TokenKind::Plus,
            },
            TokenTestCase {
                input: "-",
                expected: TokenKind::Minus,
            },
            TokenTestCase {
                input: "*",
                expected: TokenKind::Asterisk,
            },
            TokenTestCase {
                input: ">",
                expected: TokenKind::Greater,
            },
            TokenTestCase {
                input: ">=",
                expected: TokenKind::GreaterEqual,
            },
            TokenTestCase {
                input: "<",
                expected: TokenKind::Less,
            },
            TokenTestCase {
                input: "<=",
                expected: TokenKind::LessEqual,
            },
            TokenTestCase {
                input: "=",
                expected: TokenKind::Equal,
            },
            TokenTestCase {
                input: "==",
                expected: TokenKind::EqualEqual,
            },
            TokenTestCase {
                input: "!",
                expected: TokenKind::Bang,
            },
            TokenTestCase {
                input: "!=",
                expected: TokenKind::BangEqual,
            },
            TokenTestCase {
                input: ";",
                expected: TokenKind::Semicolon,
            },
            TokenTestCase {
                input: ":",
                expected: TokenKind::Colon,
            },
            TokenTestCase {
                input: "(",
                expected: TokenKind::LParen,
            },
            TokenTestCase {
                input: ")",
                expected: TokenKind::RParen,
            },
            TokenTestCase {
                input: "{",
                expected: TokenKind::LBrace,
            },
            TokenTestCase {
                input: "}",
                expected: TokenKind::RBrace,
            },
            TokenTestCase {
                input: "\"string_lit\"",
                expected: TokenKind::StringLit,
            },
            TokenTestCase {
                input: "\"unbalanced",
                expected: TokenKind::UnbalancedQuote,
            },
            TokenTestCase {
                input: "1234",
                expected: TokenKind::Integer,
            },
            TokenTestCase {
                input: "124ab",
                expected: TokenKind::Illegal,
            },
            TokenTestCase {
                input: "12!",
                expected: TokenKind::Illegal,
            },
            TokenTestCase {
                input: "++",
                expected: TokenKind::PlusPlus,
            },
            TokenTestCase {
                input: "--",
                expected: TokenKind::MinusMinus,
            },
            TokenTestCase {
                input: "foobar",
                expected: TokenKind::Identifier,
            },
            TokenTestCase {
                input: "let",
                expected: TokenKind::KWLet,
            },
            TokenTestCase {
                input: "true",
                expected: TokenKind::KWTrue,
            },
            TokenTestCase {
                input: "false",
                expected: TokenKind::KWFalse,
            },
        ];

        for s in test_source.into_iter() {
            let source = RefCell::new(SourceFile::new(0, "test".to_string(), s.input));
            let mut diagnostics = Diagnostics::new();
            let mut lexer = Lexer::new(&source, &mut diagnostics);
            let token = lexer
                .next()
                .expect(&format!("Unexpected end of file while lexing {}", s.input));

            assert_eq!(token.kind, s.expected);
        }
    }

    //TODO: add a comprehensive integration test for a code piece once the lexer is complete
}
