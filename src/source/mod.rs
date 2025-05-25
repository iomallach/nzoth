#[derive(Debug)]
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
        //TODO: unwrap???
        let line = self.line_offsets.binary_search(&offset).unwrap();
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
}

#[derive(Debug)]
pub struct Position {
    pub offset: usize,
    pub line: usize,
    pub column: usize,
}

#[derive(Debug)]
pub struct Span {
    pub start: Position,
    pub end: Position,
    pub file: FileId,
}
