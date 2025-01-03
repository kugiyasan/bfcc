use std::collections::{HashMap, HashSet};

use crate::parser::{
    ConstantExpr, DeclarationSpecifier, Declarator, DirectDeclarator, ExprKind, Primary,
    TypeSpecifier, Unary,
};

use super::Type;

#[derive(Clone, Debug)]
struct VarType {
    ty: Type,
    offset: usize,
}

#[derive(Clone, Debug)]
pub struct SymbolTable {
    table: HashMap<String, VarType>,
    total_offset: HashMap<String, usize>,
    current_func_name: String,
    strings: HashMap<String, usize>,
    labels: HashSet<String>,
}

#[derive(Clone, Debug)]
pub enum LvarOffset {
    Local(usize),
    Global,
    String(usize),
}

impl SymbolTable {
    pub fn new() -> Self {
        let mut st = Self {
            table: HashMap::new(),
            total_offset: HashMap::new(),
            current_func_name: "".to_string(),
            strings: HashMap::new(),
            labels: HashSet::new(),
        };
        st.declare_func("".to_string());
        st
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
        let ty = self.convert_type(&specs, &declarator);
        let var_name = declarator.direct.get_name();
        let name = self.format_var_name(&var_name);
        self._declare_var(ty, name);
    }

    fn _declare_var(&mut self, ty: Type, name: String) {
        self.total_offset
            .entry(self.current_func_name.clone())
            .and_modify(|offset| *offset += ty.sizeof());
        let offset = *self.total_offset.get(&self.current_func_name).unwrap();

        let var_type = VarType { ty, offset };
        self.table.insert(name, var_type);
    }

    pub fn declare_string(&mut self, s: Vec<u8>) {
        let name = s.iter().map(|&b| b as char).collect::<String>();
        let ty = Type::Array(Box::new(Type::Char), s.len());
        self._declare_var(ty, name.clone());
        self.strings.insert(name, self.strings.len());
    }

    pub fn get_strings(&self) -> &HashMap<String, usize> {
        &self.strings
    }

    fn _get_var_type(&self, var_name: &str) -> &VarType {
        let name = self.format_var_name(var_name);
        let var_type = self.table.get(&name).or_else(|| {
            let name = format!("::{}", var_name);
            self.table.get(&name)
        });
        var_type.expect("Undeclared variable")
    }

    pub fn get_lvar_offset(&self, var_name: &str) -> LvarOffset {
        let name = self.format_var_name(var_name);
        if let Some(var_type) = self.table.get(&name) {
            return LvarOffset::Local(var_type.offset);
        }

        let name = format!("::{}", var_name);
        if self.table.contains_key(&name) {
            return LvarOffset::Global;
        }

        let string_id = self.strings.get(var_name).expect("Undeclared variable");
        LvarOffset::String(*string_id)
    }

    pub fn get_var_type(&self, var_name: &str) -> Type {
        self._get_var_type(var_name).ty.clone()
    }

    pub fn add_label(&mut self, name: String) {
        self.labels.insert(name);
    }

    pub fn convert_type(&self, specs: &Vec<DeclarationSpecifier>, declarator: &Declarator) -> Type {
        self._convert_type(self.get_primary_type(specs), declarator)
    }

    fn get_primary_type(&self, specs: &Vec<DeclarationSpecifier>) -> Type {
        for spec in specs {
            if let DeclarationSpecifier::TypeSpecifier(ts) = spec {
                return match ts {
                    TypeSpecifier::Void => Type::Void,
                    TypeSpecifier::Char => Type::Char,
                    TypeSpecifier::Int => Type::Int,
                    _ => todo!(),
                };
            }
        }
        panic!("Variable of unknown type");
    }

    fn get_var_type_from_direct_declarator(
        &self,
        t: Type,
        direct_declarator: &DirectDeclarator,
    ) -> Type {
        match direct_declarator {
            DirectDeclarator::Ident(_) => t,
            DirectDeclarator::Declarator(d) => self._convert_type(t, d),
            DirectDeclarator::Array(dd, e) => {
                let t = self.get_var_type_from_direct_declarator(t, dd);
                let Some(ConstantExpr::Identity(ExprKind::Unary(Unary::Identity(Primary::Num(
                    size,
                ))))) = e
                else {
                    todo!("Can't handle ConstExpr");
                };
                Type::Array(Box::new(t), *size as usize)
            }
            _ => todo!(),
        }
    }

    fn _convert_type(&self, mut t: Type, declarator: &Declarator) -> Type {
        let mut pointer = &declarator.pointer;
        while let Some(p) = pointer {
            t = Type::Ptr(Box::new(t));
            pointer = &p.pointer;
        }

        self.get_var_type_from_direct_declarator(t, &declarator.direct)
    }
}
