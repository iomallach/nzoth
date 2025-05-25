pub struct Diagnostic {
    message: String,
    file: String,
}

pub struct Diagnostics {
    diagnostics: Vec<String>,
}

impl Diagnostics {
    pub fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
        }
    }
}
