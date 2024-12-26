use std::collections::HashMap;

#[derive(Debug)]
pub(super) struct LocalVariables {
    locals: HashMap<String, usize>,
    last_offset: usize,
}

impl LocalVariables {
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

    pub fn get_lvar_offset(&mut self, ident: &str) -> usize {
        if let Some(offset) = self.locals.get(ident) {
            return *offset;
        }
        self.last_offset += 8;
        let offset = self.last_offset;
        self.locals.insert(ident.to_string(), offset);
        offset
    }
}
