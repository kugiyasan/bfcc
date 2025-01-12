use std::collections::{HashMap, HashSet};

use crate::parser::{
    ConstantExpr, DeclarationSpecifier, Declarator, DirectDeclarator, ExprKind, Primary,
    StructDeclarator, StructOrUnion, StructOrUnionSpecifier, TypeSpecifier, TypeSpecifierTrait,
    Unary,
};

use super::Ty;

#[derive(Clone, Debug)]
struct VarType {
    ty: Ty,
    offset: usize,
}

#[derive(Clone, Debug, Default)]
pub struct SymbolTable {
    locals: HashMap<String, VarType>,
    globals: HashMap<String, Ty>,
    total_offset: HashMap<String, usize>,
    strings: HashMap<String, usize>,
    labels: HashSet<String>,
    structs: HashMap<String, Option<Vec<(String, Ty)>>>,
    last_anonymous_struct_id: usize,

    current_func_name: String,
}

#[derive(Clone, Debug)]
pub enum LvarOffset {
    Local(usize),
    Global,
    String(usize),
}

impl SymbolTable {
    pub fn new() -> Self {
        Self::default()
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

    fn new_anonymous_struct(&mut self) -> String {
        let s = format!(".anonymous_struct{}", self.last_anonymous_struct_id);
        self.last_anonymous_struct_id += 1;
        s
    }

    pub fn get_offset(&self, func_name: &str) -> usize {
        *self.total_offset.get(func_name).expect("Unknown function")
    }

    pub fn declare_local(&mut self, specs: Vec<DeclarationSpecifier>, declarator: Declarator) {
        let ty = self.from_specs_and_declarator(&specs, &declarator);
        let var_name = declarator.direct.get_name();
        let name = self.format_var_name(&var_name);
        self._declare_var(ty, name);
    }

    pub fn declare_global(&mut self, specs: Vec<DeclarationSpecifier>, declarator: Declarator) {
        let ty = self.from_specs_and_declarator(&specs, &declarator);
        let var_name = declarator.direct.get_name();
        self.globals.insert(var_name, ty);
    }

    pub fn declare_var_with_offset(
        &mut self,
        specs: Vec<DeclarationSpecifier>,
        declarator: Declarator,
        offset: usize,
    ) {
        let ty = self.from_specs_and_declarator(&specs, &declarator);
        let var_name = declarator.direct.get_name();
        let name = self.format_var_name(&var_name);
        self._declare_var_with_offset(ty, name, offset);
    }

    fn _declare_var(&mut self, ty: Ty, name: String) {
        let size = ty.sizeof(&self);
        self._declare_var_with_offset(ty, name, size)
    }

    fn _declare_var_with_offset(&mut self, ty: Ty, name: String, offset: usize) {
        self.total_offset
            .entry(self.current_func_name.clone())
            .and_modify(|v| *v += offset);
        let offset = *self.total_offset.get(&self.current_func_name).unwrap();

        let var_type = VarType { ty, offset };
        self.locals.insert(name, var_type);
    }

    pub fn declare_string(&mut self, s: Vec<u8>) {
        let name = s.iter().map(|&b| b as char).collect::<String>();
        let ty = Ty::Array(Box::new(Ty::Char), s.len());
        self._declare_var(ty, name.clone());
        self.strings.insert(name, self.strings.len());
    }

    pub fn get_strings(&self) -> &HashMap<String, usize> {
        &self.strings
    }

    pub fn get_lvar_offset(&self, var_name: &str) -> LvarOffset {
        let name = self.format_var_name(var_name);
        if let Some(var_type) = self.locals.get(&name) {
            return LvarOffset::Local(var_type.offset);
        }

        if self.globals.contains_key(var_name) {
            return LvarOffset::Global;
        }

        let string_id = self.strings.get(var_name).expect("Undeclared variable");
        LvarOffset::String(*string_id)
    }

    pub fn get_var_type(&self, var_name: &str) -> Ty {
        let name = self.format_var_name(var_name);
        let ty = self
            .locals
            .get(&name)
            .map(|v| &v.ty)
            .or_else(|| self.globals.get(var_name));
        ty.expect("Undeclared variable").clone()
    }

    pub fn add_label(&mut self, name: String) {
        self.labels.insert(name);
    }

    pub fn get_struct_definition(&self, name: &str) -> &Vec<(String, Ty)> {
        self.structs
            .get(name)
            .expect("Undefined struct")
            .as_ref()
            .expect("Undefined struct")
    }

    pub fn get_struct_field(&self, name: &str, field: &str) -> (usize, &Ty) {
        let sds = self.get_struct_definition(&name);
        let mut offset = 0;

        for (s, ty) in sds {
            if s == field {
                return (offset, ty);
            }
            offset += ty.sizeof(&self);
        }

        panic!("Accessing unknown field {:?} on struct {:?}", field, name);
    }

    pub fn from_specs_and_declarator(
        &mut self,
        specs: &Vec<DeclarationSpecifier>,
        declarator: &Declarator,
    ) -> Ty {
        let ty = self.parse_primary_type(specs);
        self.parse_declarator(ty, declarator)
    }

    fn parse_primary_type<T>(&mut self, specs: &Vec<T>) -> Ty
    where
        T: TypeSpecifierTrait,
    {
        for spec in specs {
            if let Some(ts) = spec.get_type_specifier() {
                return match ts {
                    TypeSpecifier::Void => Ty::Void,
                    TypeSpecifier::Char => Ty::Char,
                    TypeSpecifier::Short => Ty::Short,
                    TypeSpecifier::Int => Ty::Int,
                    TypeSpecifier::Long => Ty::Long,
                    TypeSpecifier::StructOrUnionSpecifier(s) => {
                        self.parse_struct_or_union_specifier(s)
                    }
                    _ => todo!(),
                };
            }
        }
        panic!("Variable of unknown type");
    }

    fn parse_struct_or_union_specifier(&mut self, s: &StructOrUnionSpecifier) -> Ty {
        match s {
            StructOrUnionSpecifier::WithDeclaration(StructOrUnion::Struct, ident, sds) => {
                let s = ident
                    .as_ref()
                    .map(|s| s.clone())
                    .unwrap_or_else(|| self.new_anonymous_struct());
                self.structs.insert(s.clone(), None);

                let mut tys = vec![];
                for struct_declaration in sds {
                    let ty = self.parse_primary_type(&struct_declaration.specs);
                    for sd in struct_declaration.declarators.iter() {
                        let StructDeclarator::Declarator(d) = sd else {
                            todo!();
                        };
                        let t = self.parse_declarator(ty.clone(), d);
                        tys.push((d.direct.get_name(), t));
                    }
                }

                self.structs.insert(s.clone(), Some(tys));
                Ty::Struct(s)
            }
            StructOrUnionSpecifier::Identifier(StructOrUnion::Struct, ident) => {
                Ty::Struct(ident.clone())
            }
            _ => todo!(),
        }
    }

    fn parse_declarator(&mut self, mut t: Ty, declarator: &Declarator) -> Ty {
        let mut pointer = &declarator.pointer;
        while let Some(p) = pointer {
            t = Ty::Ptr(Box::new(t));
            pointer = &p.pointer;
        }

        self.parse_direct_declarator(t, &declarator.direct)
    }

    fn parse_direct_declarator(&mut self, t: Ty, direct_declarator: &DirectDeclarator) -> Ty {
        match direct_declarator {
            DirectDeclarator::Ident(_) => t,
            DirectDeclarator::Declarator(d) => self.parse_declarator(t, d),
            DirectDeclarator::Array(dd, e) => {
                let t = self.parse_direct_declarator(t, dd);
                let Some(ConstantExpr::Identity(ExprKind::Unary(Unary::Identity(Primary::Num(
                    size,
                ))))) = e
                else {
                    todo!("Can't handle ConstExpr");
                };
                Ty::Array(Box::new(t), *size as usize)
            }
            _ => todo!(),
        }
    }
}
