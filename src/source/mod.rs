#[derive(Debug, Copy, Clone)]
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
        let line_end = if line + 1 >= self.line_offsets.len() {
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
}

#[derive(Debug, Copy, Clone)]
pub struct Position {
    pub offset: usize,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, Copy, Clone)]
pub struct Span {
    pub start: Position,
    pub end: Position,
    pub file: FileId,
}
