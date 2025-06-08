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
                        self.source.next();
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
                    while let Some((j, _)) = self
                        .source
                        .next_if(|(_, c)| *c != '"' && *c != ';' && *c != '\n')
                    {
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
                    match self.source.peek() {
                        Some((_, c)) if c.is_alphabetic() => {
                            while let Some((j, _)) =
                                self.source.next_if(|(_, c)| c.is_alphanumeric())
                            {
                                self.last_pos = j;
                            }
                            token = Some(self.make_token(
                                TokenKind::Illegal,
                                start_offset,
                                self.last_pos + 1,
                            ));
                        }
                        Some((_, c)) if *c == '.' => {
                            // skip the dot
                            self.source.next();
                            while let Some((j, _)) = self.source.next_if(|(_, c)| c.is_numeric()) {
                                self.last_pos = j;
                            }
                            //TODO: handle 12.43abc?
                            token = Some(self.make_token(
                                TokenKind::Float,
                                start_offset,
                                self.last_pos + 1,
                            ))
                        }
                        _ => {
                            token = Some(self.make_token(
                                TokenKind::Integer,
                                start_offset,
                                self.last_pos + 1,
                            ))
                        }
                    }
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
                        "fn" => {
                            token = Some(self.make_token(
                                TokenKind::KWFn,
                                start_offset,
                                self.last_pos + 1,
                            ));
                        }
                        "return" => {
                            token = Some(self.make_token(
                                TokenKind::KWReturn,
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
