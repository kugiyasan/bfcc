use super::SymbolTable;

#[derive(Clone, Debug, PartialEq)]
pub enum Ty {
    Void,
    Char,
    Short,
    Int,
    Long,
    Ptr(Box<Ty>),
    Array(Box<Ty>, usize),
    Func(Box<Ty>, Vec<Ty>),
    Struct(String),
}

impl Ty {
    pub fn sizeof(&self, symbol_table: &SymbolTable) -> usize {
        match self {
            Ty::Void => 0,
            Ty::Char => 1,
            Ty::Short => 2,
            Ty::Int => 4,
            Ty::Long => 8,
            Ty::Ptr(_) => 8,
            Ty::Array(t, size) => t.sizeof(symbol_table) * size,
            Ty::Struct(name) => {
                let tys = symbol_table.get_struct_definition(name);
                tys.iter().map(|(_, ty)| ty.sizeof(symbol_table)).sum()
            }
            Ty::Func(_, _) => panic!("sizeof function is not allowed"),
        }
    }

    pub fn is_compatible(&self, other: &Self) -> bool {
        match (self, other) {
            (t1, t2) if t1 == t2 => true,
            (t1, t2) if t1.is_numeric() && t2.is_numeric() => true,
            _ => false,
        }
    }

    fn is_numeric(&self) -> bool {
        matches!(self, Ty::Char | Ty::Short | Ty::Int | Ty::Long)
    }

    pub fn get_inner(&self) -> Option<Ty> {
        match self {
            Ty::Ptr(ty) | Ty::Array(ty, _) => Some(*ty.clone()),
            _ => None,
        }
    }
}
