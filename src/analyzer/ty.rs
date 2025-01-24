use super::SymbolTable;

#[derive(Clone, Debug, PartialEq)]
pub enum Ty {
    Void,
    Bool,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
    Ptr(Box<Ty>),
    Array(Box<Ty>, usize),
    Func(Box<Ty>, Vec<Ty>),
    Struct(String),
    Union(String),
}

impl Ty {
    pub fn sizeof(&self, symbol_table: &SymbolTable) -> usize {
        match self {
            Ty::Void => 0,
            Ty::Bool | Ty::I8 | Ty::U8 => 1,
            Ty::I16 | Ty::U16 => 2,
            Ty::I32 | Ty::U32 | Ty::F32 => 4,
            Ty::I64 | Ty::U64 | Ty::F64 | Ty::Ptr(_) => 8,
            Ty::Array(t, size) => t.sizeof(symbol_table) * size,
            Ty::Struct(name) => {
                let tys = symbol_table.get_struct_definition(name);
                tys.iter().map(|(_, ty)| ty.sizeof(symbol_table)).sum()
            }
            Ty::Union(name) => {
                let tys = symbol_table.get_union_definition(name);
                tys.iter()
                    .map(|(_, ty)| ty.sizeof(symbol_table))
                    .max()
                    .expect("Zero-sized unions are not supported")
            }
            Ty::Func(_, _) => panic!("sizeof function is not allowed"),
        }
    }

    pub fn assert_compatible(&self, other: &Self) {
        assert!(
            self.is_compatible(other),
            "{:?} is not compatible with {:?}",
            self,
            other
        );
    }

    pub fn is_compatible(&self, other: &Self) -> bool {
        match (self, other) {
            (t1, t2) if t1 == t2 => true,
            (t1, t2) if t1.is_numeric() && t2.is_numeric() => true,
            (Ty::Ptr(p1), Ty::Ptr(p2)) => p1.is_compatible(p2),
            (Ty::Void, _) | (_, Ty::Void) => true,
            _ => false,
        }
    }

    pub fn is_numeric(&self) -> bool {
        matches!(self, Ty::I8 | Ty::I16 | Ty::I32 | Ty::I64)
    }

    pub fn get_inner(&self) -> Option<Ty> {
        match self {
            Ty::Ptr(ty) | Ty::Array(ty, _) => Some(*ty.clone()),
            _ => None,
        }
    }
}
