use std::collections::{HashMap, HashSet};

use crate::parser::{
    AbstractDeclarator, DeclarationSpecifier, Declarator, DirectAbstractDeclarator,
    DirectDeclarator, EnumSpecifier, Enumerator, ParamDeclaration, ParamTypeList, Pointer,
    StructDeclarator, StructOrUnion, StructOrUnionSpecifier, TypeSpecifier, TypeSpecifierTrait,
    Typedefs,
};

use super::Ty;

#[derive(Clone, Debug)]
struct VarType {
    ty: Ty,
    offset: usize,
}

mod ty_counter {
    pub const VOID: usize = 1 << 0;
    pub const BOOL: usize = 1 << 2;
    pub const CHAR: usize = 1 << 4;
    pub const SHORT: usize = 1 << 6;
    pub const INT: usize = 1 << 8;
    pub const LONG: usize = 1 << 10;
    pub const FLOAT: usize = 1 << 12;
    pub const DOUBLE: usize = 1 << 14;
    // pub const OTHER: usize = 1 << 16;
    pub const SIGNED: usize = 1 << 17;
    pub const UNSIGNED: usize = 1 << 18;
}

#[derive(Clone, Debug, Default)]
pub struct SymbolTable {
    locals: HashMap<String, VarType>,
    globals: HashMap<String, Ty>,
    total_offset: HashMap<String, usize>,
    strings: HashMap<String, usize>,
    labels: HashSet<String>,
    structs: HashMap<String, Option<Vec<(String, Ty)>>>,
    unions: HashMap<String, Option<Vec<(String, Ty)>>>,
    enums: HashMap<String, HashMap<String, usize>>,
    typedefs: Typedefs,

    last_anonymous_struct_id: usize,
    last_anonymous_union_id: usize,
    last_anonymous_enum_id: usize,
    current_func_name: String,
}

#[derive(Clone, Debug)]
pub enum LvarOffset {
    Local(usize),
    Global,
    String(usize),
}

impl SymbolTable {
    pub fn new(typedefs: Typedefs) -> Self {
        Self {
            typedefs,
            ..Default::default()
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

    fn new_anonymous_struct(&mut self) -> String {
        let s = format!(".anonymous_struct{}", self.last_anonymous_struct_id);
        self.last_anonymous_struct_id += 1;
        s
    }

    fn new_anonymous_union(&mut self) -> String {
        let s = format!(".anonymous_union{}", self.last_anonymous_union_id);
        self.last_anonymous_union_id += 1;
        s
    }

    fn new_anonymous_enum(&mut self) -> String {
        let s = format!(".anonymous_enum{}", self.last_anonymous_enum_id);
        self.last_anonymous_enum_id += 1;
        s
    }

    pub fn get_offset(&self, func_name: &str) -> usize {
        *self.total_offset.get(func_name).expect("Unknown function")
    }

    pub fn declare_local(&mut self, specs: &Vec<DeclarationSpecifier>, declarator: &Declarator) {
        let ty = self.from_specs_and_declarator(specs, declarator);
        let var_name = declarator.direct.get_name();
        let name = self.format_var_name(&var_name);
        self._declare_local(ty, name);
    }

    pub fn declare_global(&mut self, specs: &Vec<DeclarationSpecifier>, declarator: &Declarator) {
        let ty = self.from_specs_and_declarator(specs, declarator);
        let var_name = declarator.direct.get_name();
        self.globals.insert(var_name, ty);
    }

    pub fn declare_local_with_offset(
        &mut self,
        specs: Vec<DeclarationSpecifier>,
        declarator: Declarator,
        offset: usize,
    ) {
        let ty = self.from_specs_and_declarator(&specs, &declarator);
        let var_name = declarator.direct.get_name();
        let name = self.format_var_name(&var_name);
        self._declare_local_with_offset(ty, name, offset);
    }

    fn _declare_local(&mut self, ty: Ty, name: String) {
        let size = ty.sizeof(self);
        self._declare_local_with_offset(ty, name, size)
    }

    fn _declare_local_with_offset(&mut self, ty: Ty, name: String, offset: usize) {
        self.total_offset
            .entry(self.current_func_name.clone())
            .and_modify(|v| *v += offset);
        let offset = *self.total_offset.get(&self.current_func_name).unwrap();

        let var_type = VarType { ty, offset };
        self.locals.insert(name, var_type);
    }

    pub fn declare_string(&mut self, s: Vec<u8>) {
        let name = s.iter().map(|&b| b as char).collect::<String>();
        let ty = Ty::Array(Box::new(Ty::I8), s.len());
        self._declare_local_with_offset(ty, name.clone(), 8);
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
        self.locals
            .get(&name)
            .map(|v| &v.ty)
            .or_else(|| self.globals.get(var_name))
            .unwrap_or_else(|| panic!("Undeclared variable: {}", var_name))
            .clone()
    }

    pub fn add_label(&mut self, name: String) {
        self.labels.insert(name);
    }

    pub fn get_struct_definition(&self, name: &str) -> &Vec<(String, Ty)> {
        self.structs
            .get(name)
            .unwrap_or_else(|| panic!("Undefined struct: {}", name))
            .as_ref()
            .unwrap_or_else(|| panic!("Undefined struct: {}", name))
    }

    pub fn get_union_definition(&self, name: &str) -> &Vec<(String, Ty)> {
        self.unions
            .get(name)
            .unwrap_or_else(|| panic!("Undefined union: {}", name))
            .as_ref()
            .unwrap_or_else(|| panic!("Undefined union: {}", name))
    }

    pub fn get_struct_field(&self, name: &str, field: &str) -> (usize, &Ty) {
        let sds = self.get_struct_definition(name);
        let mut offset = 0;

        for (s, ty) in sds {
            if s == field {
                return (offset, ty);
            }
            offset += ty.sizeof(self);
        }

        panic!("Accessing unknown field {:?} on struct {:?}", field, name);
    }

    pub fn get_union_field(&self, name: &str, field: &str) -> &Ty {
        let uds = self.get_union_definition(name);
        for (s, ty) in uds {
            if s == field {
                return ty;
            }
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

    pub fn from_specs_and_abstract_declarator<T>(
        &mut self,
        specs: &Vec<T>,
        declarator: &Option<AbstractDeclarator>,
    ) -> Ty
    where
        T: TypeSpecifierTrait,
    {
        let ty = self.parse_primary_type(specs);
        if let Some(d) = declarator {
            self.parse_abstract_declarator(ty, d)
        } else {
            ty
        }
    }

    /// inspired from https://github.com/rui314/chibicc/blob/main/parse.c#L381
    pub fn parse_primary_type<T>(&mut self, specs: &Vec<T>) -> Ty
    where
        T: TypeSpecifierTrait,
    {
        let mut counter = 0;
        for spec in specs {
            if let Some(ts) = spec.get_type_specifier() {
                match ts {
                    TypeSpecifier::Void => counter += ty_counter::VOID,
                    TypeSpecifier::Bool => counter += ty_counter::BOOL,
                    TypeSpecifier::Char => counter += ty_counter::CHAR,
                    TypeSpecifier::Short => counter += ty_counter::SHORT,
                    TypeSpecifier::Int => counter += ty_counter::INT,
                    TypeSpecifier::Long => counter += ty_counter::LONG,

                    TypeSpecifier::Float => counter += ty_counter::FLOAT,
                    TypeSpecifier::Double => counter += ty_counter::DOUBLE,
                    TypeSpecifier::Signed => counter += ty_counter::SIGNED,
                    TypeSpecifier::Unsigned => counter += ty_counter::UNSIGNED,

                    TypeSpecifier::StructOrUnionSpecifier(s) => {
                        return self.parse_struct_or_union_specifier(s);
                    }
                    TypeSpecifier::EnumSpecifier(e) => {
                        return self.parse_enum_specifier(e);
                    }
                    TypeSpecifier::TypedefName(typedef_name) => {
                        let (s, d) = self
                            .typedefs
                            .get(typedef_name)
                            .expect("Typedef name is not in the typedef HashMap");
                        return self.from_specs_and_declarator(&s.clone(), &d.clone());
                    }
                };
            }
        }
        self.counter_to_ty(counter)
    }

    fn counter_to_ty(&self, counter: usize) -> Ty {
        use ty_counter::*;
        match counter {
            VOID => Ty::Void,
            v if v == BOOL => Ty::Bool,
            v if v == CHAR || v == SIGNED + CHAR => Ty::I8,
            v if v == UNSIGNED + CHAR => Ty::U8,
            v if v == SHORT
                || v == SHORT + INT
                || v == SIGNED + SHORT
                || v == SIGNED + SHORT + INT =>
            {
                Ty::I16
            }
            v if v == UNSIGNED + SHORT || v == UNSIGNED + SHORT + INT => Ty::U16,
            v if v == INT || v == SIGNED || v == SIGNED + INT => Ty::I32,
            v if v == UNSIGNED || v == UNSIGNED + INT => Ty::U32,
            v if v == LONG
                || v == LONG + INT
                || v == LONG + LONG
                || v == LONG + LONG + INT
                || v == SIGNED + LONG
                || v == SIGNED + LONG + INT
                || v == SIGNED + LONG + LONG
                || v == SIGNED + LONG + LONG + INT =>
            {
                Ty::I64
            }
            v if v == UNSIGNED + LONG
                || v == UNSIGNED + LONG + INT
                || v == UNSIGNED + LONG + LONG
                || v == UNSIGNED + LONG + LONG + INT =>
            {
                Ty::U64
            }
            v if v == FLOAT => Ty::F32,
            v if v == DOUBLE => Ty::F64,
            v if v == LONG + DOUBLE => Ty::F128,
            _ => panic!("Variable of unknown type"),
        }
    }

    fn parse_struct_or_union_specifier(&mut self, s: &StructOrUnionSpecifier) -> Ty {
        match s {
            StructOrUnionSpecifier::WithDeclaration(StructOrUnion::Struct, ident, sds) => {
                let s = ident.clone().unwrap_or_else(|| self.new_anonymous_struct());
                self.structs.insert(s.clone(), None);

                let mut tys = vec![];
                for struct_declaration in sds {
                    let ty = self.parse_primary_type(&struct_declaration.specs);
                    for sd in struct_declaration.declarators.iter() {
                        let StructDeclarator::Declarator(d) = sd else {
                            unimplemented!();
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
            StructOrUnionSpecifier::WithDeclaration(StructOrUnion::Union, ident, sds) => {
                let u = ident.clone().unwrap_or_else(|| self.new_anonymous_union());
                self.unions.insert(u.clone(), None);

                let mut tys = vec![];
                for struct_declaration in sds {
                    let ty = self.parse_primary_type(&struct_declaration.specs);
                    for sd in struct_declaration.declarators.iter() {
                        let StructDeclarator::Declarator(d) = sd else {
                            unimplemented!();
                        };
                        let t = self.parse_declarator(ty.clone(), d);
                        tys.push((d.direct.get_name(), t));
                    }
                }

                self.unions.insert(u.clone(), Some(tys));
                Ty::Union(u)
            }
            StructOrUnionSpecifier::Identifier(StructOrUnion::Union, ident) => {
                Ty::Union(ident.clone())
            }
        }
    }

    fn parse_enum_specifier(&mut self, e: &EnumSpecifier) -> Ty {
        match e {
            EnumSpecifier::WithEnumerator(ident, enumerators) => {
                let e = ident.clone().unwrap_or_else(|| self.new_anonymous_enum());

                let mut variants = HashMap::new();
                let mut index = 0;
                for enumerator in enumerators {
                    match enumerator {
                        Enumerator::Identifier(ident) => {
                            variants.insert(ident.clone(), index);
                        }
                        Enumerator::Init(ident, expr) => {
                            index = expr.constant_fold(self) as usize;
                            variants.insert(ident.clone(), index);
                        }
                    };
                    index += 1;
                }

                self.enums.insert(e.clone(), variants);
                Ty::Enum(e)
            }
            EnumSpecifier::Identifier(ident) => Ty::Enum(ident.clone()),
        }
    }

    fn parse_pointer(&self, mut ty: Ty, mut pointer: &Option<Pointer>) -> Ty {
        while let Some(p) = pointer {
            ty = Ty::Ptr(Box::new(ty));
            pointer = &p.pointer;
        }

        ty
    }

    fn parse_declarator(&mut self, mut ty: Ty, declarator: &Declarator) -> Ty {
        ty = self.parse_pointer(ty, &declarator.pointer);

        self.parse_direct_declarator(ty, &declarator.direct)
    }

    fn parse_direct_declarator(&mut self, ty: Ty, direct_declarator: &DirectDeclarator) -> Ty {
        match direct_declarator {
            DirectDeclarator::Ident(_) => ty,
            DirectDeclarator::Declarator(d) => self.parse_declarator(ty, d),
            DirectDeclarator::Array(dd, e) => {
                let t = self.parse_direct_declarator(ty, dd);
                if let Some(e) = e {
                    let size = e.constant_fold(self);
                    Ty::Array(Box::new(t), size as usize)
                } else {
                    Ty::Ptr(Box::new(t))
                }
            }
            DirectDeclarator::ParamTypeList(dd, ptl) => self.parse_param_type_list(ty, dd, ptl),
        }
    }

    fn parse_param_type_list(&mut self, ty: Ty, dd: &DirectDeclarator, ptl: &ParamTypeList) -> Ty {
        let return_ty = self.parse_direct_declarator(ty, dd);

        let args = ptl
            .params
            .iter()
            .map(|pd| match pd {
                ParamDeclaration::Declarator(s, d) => self.from_specs_and_declarator(s, d),
                ParamDeclaration::AbstractDeclarator(s, None) => self.parse_primary_type(s),
                ParamDeclaration::AbstractDeclarator(s, Some(ad)) => {
                    let ty = self.parse_primary_type(s);
                    self.parse_abstract_declarator(ty, ad)
                }
            })
            .collect();

        Ty::Func(Box::new(return_ty), args)
    }

    fn parse_abstract_declarator(&mut self, mut ty: Ty, declarator: &AbstractDeclarator) -> Ty {
        match declarator {
            AbstractDeclarator::Pointer(p) => self.parse_pointer(ty, &Some(p.clone())),
            AbstractDeclarator::DirectAbstractDeclarator(p, dad) => {
                ty = self.parse_pointer(ty, p);
                self.parse_direct_abstract_declarator(ty, dad)
            }
        }
    }

    fn parse_direct_abstract_declarator(
        &mut self,
        mut ty: Ty,
        direct_abstract_declarator: &DirectAbstractDeclarator,
    ) -> Ty {
        match direct_abstract_declarator {
            DirectAbstractDeclarator::AbstractDeclarator(dad) => {
                self.parse_abstract_declarator(ty, dad)
            }
            DirectAbstractDeclarator::Array(dad, e) => {
                if let Some(d) = dad {
                    ty = self.parse_direct_abstract_declarator(ty, d);
                }
                let size = e
                    .as_ref()
                    .map(|e| e.constant_fold(self))
                    .unwrap_or_else(|| todo!("Can't handle implicit sized array"));
                Ty::Array(Box::new(ty), size as usize)
            }
            DirectAbstractDeclarator::ParamTypeList(_, _) => todo!(),
        }
    }
}
