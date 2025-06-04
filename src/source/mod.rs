#[derive(Debug, Copy, Clone, Default)]
pub struct FileId(usize);

#[derive(Debug)]
pub struct SourceFile<'a> {
    pub id: FileId,
    pub name: String,
    pub contents: &'a str,
    line_offsets: Vec<usize>,
}

impl<'a> SourceFile<'a> {
    pub fn new(id: usize, name: String, contents: &'a str) -> Self {
        Self {
            id: FileId(id),
            name,
            contents,
            line_offsets: vec![0],
        }
    }

    pub fn append_line_start(&mut self, offset: usize) {
        self.line_offsets.push(offset);
    }

    pub fn position_at_offset(&self, offset: usize) -> Position {
        let line = self
            .line_offsets
            .binary_search(&offset)
            // Err(e) contains the position where it would be inserted
            .unwrap_or_else(|i| i.saturating_sub(1));
        let column = offset - self.line_offsets[line];

        Position {
            offset,
            line,
            column,
        }
    }

    pub fn span_text(&self, span: &Span) -> &str {
        &self.contents[span.start.offset..span.end.offset]
    }

    pub fn line_str(&self, line: usize) -> &str {
        let line_start = self.line_offsets[line];
        let line_end = if self.line_offsets.len() == 1 {
            self.contents.len() - 1
        } else if line + 1 >= self.line_offsets.len() {
            self.line_offsets.len()
        } else {
            self.line_offsets[line + 1]
        };

        &self.contents[line_start..line_end]
    }

    pub fn make_span(&self, offset_start: usize, offset_end: usize) -> Span {
        Span {
            start: self.position_at_offset(offset_start),
            end: self.position_at_offset(offset_end),
            file: self.id,
        }
    }

    pub fn end_of_file(&self) -> Span {
        Span {
            start: self.position_at_offset(self.contents.len() - 1),
            end: self.position_at_offset(self.contents.len() - 1),
            file: self.id,
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Default)]
pub struct Position {
    pub offset: usize,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Copy, Clone, Default)]
pub struct Span {
    pub start: Position,
    pub end: Position,
    pub file: FileId,
}

impl Span {
    pub fn from_spans(start_span: Span, end_span: Span) -> Self {
        Span {
            start: start_span.start,
            end: end_span.end,
            file: start_span.file,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{Position, SourceFile};

    #[test]
    fn test_append_line_start() {
        let mut source = SourceFile::new(0, "test".to_string(), "let x = 1;");
        assert_eq!(source.line_offsets, vec![0]);

        source.append_line_start(10);
        assert_eq!(source.line_offsets, vec![0, 10]);

        source.append_line_start(101);
        assert_eq!(source.line_offsets, vec![0, 10, 101]);
    }

    #[test]
    fn test_position_at_offset_oneline() {
        let source = SourceFile::new(0, "test".to_string(), "let x = y / z;");
        let tests = vec![
            (
                0,
                Position {
                    offset: 0,
                    line: 0,
                    column: 0,
                },
            ),
            (
                4,
                Position {
                    offset: 4,
                    line: 0,
                    column: 4,
                },
            ),
            (
                12,
                Position {
                    offset: 12,
                    line: 0,
                    column: 12,
                },
            ),
        ];
        for (offset, expected_position) in tests {
            assert_eq!(source.position_at_offset(offset), expected_position);
        }
    }

    #[test]
    fn test_position_at_offset_multiline() {
        let mut source = SourceFile::new(
            0,
            "test".to_string(),
            "let x = y / z;\nx - y + z;\nfoo :: bar;",
        );

        source.append_line_start(15);
        source.append_line_start(26);

        let tests = vec![
            (
                0,
                Position {
                    offset: 0,
                    line: 0,
                    column: 0,
                },
            ),
            (
                4,
                Position {
                    offset: 4,
                    line: 0,
                    column: 4,
                },
            ),
            (
                15,
                Position {
                    offset: 15,
                    line: 1,
                    column: 0,
                },
            ),
            (
                21,
                Position {
                    offset: 21,
                    line: 1,
                    column: 6,
                },
            ),
            (
                26,
                Position {
                    offset: 26,
                    line: 2,
                    column: 0,
                },
            ),
        ];
        for (offset, expected_position) in tests {
            assert_eq!(source.position_at_offset(offset), expected_position);
        }
    }

    #[test]
    fn test_span_text() {
        let mut source = SourceFile::new(
            0,
            "test".to_string(),
            "let x = y / z;\nx - y + z;\nfoo :: bar;",
        );

        source.append_line_start(15);
        source.append_line_start(26);

        let tests = vec![
            ((0, 3), "let"),
            ((4, 5), "x"),
            ((6, 7), "="),
            ((8, 9), "y"),
            ((10, 11), "/"),
            ((12, 13), "z"),
            ((13, 14), ";"),
            ((15, 16), "x"),
            ((17, 18), "-"),
            ((19, 20), "y"),
            ((21, 22), "+"),
            ((23, 24), "z"),
            ((24, 25), ";"),
            ((26, 29), "foo"),
        ];
        // "let x = y / z;\nx - y + z;\nfoo :: bar;",

        for ((start, end), expected) in tests {
            let span = source.make_span(start, end);
            assert_eq!(expected, source.span_text(&span));
        }
    }

    fn test_line_str() {
        let mut source = SourceFile::new(
            0,
            "test".to_string(),
            "let x = y / z;\nx - y + z;\nfoo :: bar;",
        );

        source.append_line_start(15);
        source.append_line_start(26);

        let tests = vec![(0, "let x = y / z;"), (1, "x - y + z;"), (2, "foo :: bar;")];

        for (line_no, expected) in tests {
            assert_eq!(expected, source.line_str(line_no));
        }
    }
}
