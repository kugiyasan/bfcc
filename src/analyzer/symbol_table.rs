use std::collections::HashMap;

use crate::parser::{DeclarationSpecifier, Declarator, DirectDeclarator, TypeSpecifier};

use super::Type;

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

    fn format_var_name(&self, var_name: &str) -> String {
        format!("{}::{}", self.current_func_name, var_name)
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

        let name = self.format_var_name(&var_name);
        let var_type = VarType {
            specs,
            declarator,
            offset,
        };
        self.table.insert(name, var_type);
    }

    pub fn get_lvar_offset(&mut self, var_name: &str) -> usize {
        let name = self.format_var_name(var_name);
        if let Some(var_type) = self.table.get(&name) {
            return var_type.offset;
        }
        panic!("Undeclared variable: {}", name);
    }

    fn get_primary_type(&self, specs: &Vec<DeclarationSpecifier>) -> Type {
        for spec in specs {
            if let DeclarationSpecifier::TypeSpecifier(ts) = spec {
                return match ts {
                    TypeSpecifier::Int => Type::Int,
                    TypeSpecifier::Void => Type::Void,
                    _ => todo!(),
                };
            }
        }
        panic!("Variable of unknown type");
    }

    pub fn get_var_type(&self, var_name: &str) -> Type {
        let name = self.format_var_name(var_name);
        let var_type = self.table.get(&name).expect("Undeclared variable");

        let mut t = self.get_primary_type(&var_type.specs);
        let mut pointer = &var_type.declarator.pointer;
        while let Some(p) = pointer {
            t = Type::Ptr(Box::new(t));
            pointer = &p.pointer;
        }
        t
    }
}
