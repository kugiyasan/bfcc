use std::collections::HashMap;

use crate::parser::{DeclarationSpecifier, Declarator};

#[derive(Clone, Debug)]
struct VarType {
    specs: Vec<DeclarationSpecifier>,
    declarator: Declarator,
    offset: usize,
}

#[derive(Clone, Debug)]
pub struct SymbolTable {
    table: HashMap<String, VarType>,
    total_offset: HashMap<String, usize>,
    current_func_name: String,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            table: HashMap::new(),
            total_offset: HashMap::new(),
            current_func_name: "".to_string(),
        }
    }

    pub fn declare_func(&mut self, func_name: String) {
        if self.total_offset.contains_key(&func_name) {
            panic!("{} is already defined", func_name);
        }

        self.current_func_name = func_name.clone();
        self.total_offset.insert(func_name, 0);
    }

    pub fn set_current_func(&mut self, func_name: String) {
        self.current_func_name = func_name;
    }

    pub fn get_offset(&self, func_name: &str) -> usize {
        *self.total_offset.get(func_name).expect("Unknown function")
    }

    pub fn declare_var(&mut self, specs: Vec<DeclarationSpecifier>, declarator: Declarator) {
        let var_name = declarator.direct.get_name();
        self.total_offset
            .entry(self.current_func_name.clone())
            .and_modify(|offset| *offset += 8);
        let offset = *self.total_offset.get(&self.current_func_name).unwrap();

        let name = format!("{}::{}", self.current_func_name, var_name);
        let var_type = VarType {
            specs,
            declarator,
            offset,
        };
        self.table.insert(name, var_type);
    }

    pub fn get_lvar_offset(&mut self, var_name: &str) -> usize {
        let name = format!("{}::{}", self.current_func_name, var_name);
        if let Some(var_type) = self.table.get(&name) {
            return var_type.offset;
        }
        panic!("Undeclared variable: {}", name);
    }
}
