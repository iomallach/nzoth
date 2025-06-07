use std::cell::RefCell;

use nzoth::{parser::Parser, source::SourceFile};

fn main() {
    let code = "let foo;";
    let source = RefCell::new(SourceFile::new(0, "despair".to_string(), &code));
    let mut parser = Parser::new(&source);
    _ = parser.parse();
}
