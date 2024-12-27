use std::collections::HashMap;

#[derive(Clone, Debug)]
pub struct SymbolTable {
    locals: HashMap<String, usize>,
    last_offset: usize,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            locals: HashMap::new(),
            last_offset: 0,
        }
    }

    pub fn reset(&mut self) {
        self.locals.clear();
        self.last_offset = 0;
    }

    pub fn get_last_offset(&self) -> usize {
        self.last_offset
    }

    pub fn declare(&mut self, name: String) {
        self.last_offset += 8;
        let offset = self.last_offset;
        self.locals.insert(name, offset);
    }

    pub fn get_lvar_offset(&mut self, name: &str) -> usize {
        if let Some(offset) = self.locals.get(name) {
            return *offset;
        }
        panic!("Undeclared variable: {}", name);
    }
}
