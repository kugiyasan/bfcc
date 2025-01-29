use crate::parser::{
    AbstractDeclarator, BinOp, ConstantExpr, DirectAbstractDeclarator, EnumSpecifier, Pointer,
    Primary, SpecifierQualifier, StructOrUnion, StructOrUnionSpecifier, TypeName, TypeSpecifier,
    Unary,
};

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
    F128,
    Ptr(Box<Ty>),
    Array(Box<Ty>, Option<usize>),
    Func(Box<Ty>, Vec<Ty>),
    Struct(String),
    Union(String),
    Enum(String),
}

impl Ty {
    pub fn sizeof(&self, symbol_table: &SymbolTable) -> usize {
        match self {
            Ty::Void => 0,
            Ty::Bool | Ty::I8 | Ty::U8 | Ty::Enum(_) => 1,
            Ty::I16 | Ty::U16 => 2,
            Ty::I32 | Ty::U32 | Ty::F32 => 4,
            Ty::I64 | Ty::U64 | Ty::F64 | Ty::Ptr(_) => 8,
            Ty::F128 => 16,
            Ty::Array(t, Some(size)) => t.sizeof(symbol_table) * size,
            Ty::Array(t, None) => unreachable!("Size of array {:?} should have been computed", t),
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
        matches!(
            self,
            Ty::I8 | Ty::I16 | Ty::I32 | Ty::I64 | Ty::U8 | Ty::U16 | Ty::U32 | Ty::U64
        )
    }

    pub fn is_signed(&self) -> bool {
        matches!(self, Ty::I8 | Ty::I16 | Ty::I32 | Ty::I64)
    }

    pub fn get_inner(&self) -> Option<Ty> {
        match self {
            Ty::Ptr(ty) | Ty::Array(ty, _) => Some(*ty.clone()),
            _ => None,
        }
    }

    pub fn get_return_type(&self) -> Option<Ty> {
        match self {
            Ty::Func(ty, _) => Some(*ty.clone()),
            _ => None,
        }
    }

    pub fn to_typename(&self) -> TypeName {
        self._to_typename(None, None)
    }

    fn _to_typename(
        &self,
        pointer: Option<Pointer>,
        dad: Option<DirectAbstractDeclarator>,
    ) -> TypeName {
        if let Some(specs) = self.to_specifier_qualifier() {
            let declarator = match (pointer, dad) {
                (None, None) => None,
                (Some(p), None) => Some(AbstractDeclarator::Pointer(p)),
                (p, Some(d)) => Some(AbstractDeclarator::DirectAbstractDeclarator(p, d)),
            };
            return TypeName { specs, declarator };
        }

        if let Ty::Ptr(ty) = self {
            let p = Pointer {
                qualifiers: vec![],
                pointer: Box::new(pointer),
            };
            return ty._to_typename(Some(p), dad);
        }
        if let Ty::Array(ty, Some(size)) = self {
            let expr =
                ConstantExpr::Identity(BinOp::Unary(Unary::Identity(Primary::Num(*size as i64))));
            let dad = Some(DirectAbstractDeclarator::Array(None, Some(expr)));
            return ty._to_typename(pointer, dad);
        }
        if let Ty::Func(_, _) = self {
            todo!("{:?}", self);
        }

        unreachable!("{:?}", self);
    }

    fn to_specifier_qualifier(&self) -> Option<Vec<SpecifierQualifier>> {
        Some(
            self.to_type_specifier()?
                .into_iter()
                .map(SpecifierQualifier::TypeSpecifier)
                .collect(),
        )
    }

    fn to_type_specifier(&self) -> Option<Vec<TypeSpecifier>> {
        match self {
            Ty::Void => Some(vec![TypeSpecifier::Void]),
            Ty::Bool => Some(vec![TypeSpecifier::Bool]),
            Ty::I8 => Some(vec![TypeSpecifier::Char]),
            Ty::I16 => Some(vec![TypeSpecifier::Short]),
            Ty::I32 => Some(vec![TypeSpecifier::Int]),
            Ty::I64 => Some(vec![TypeSpecifier::Long]),
            Ty::U8 => Some(vec![TypeSpecifier::Unsigned, TypeSpecifier::Char]),
            Ty::U16 => Some(vec![TypeSpecifier::Unsigned, TypeSpecifier::Short]),
            Ty::U32 => Some(vec![TypeSpecifier::Unsigned, TypeSpecifier::Int]),
            Ty::U64 => Some(vec![TypeSpecifier::Unsigned, TypeSpecifier::Long]),
            Ty::F32 => Some(vec![TypeSpecifier::Float]),
            Ty::F64 => Some(vec![TypeSpecifier::Double]),
            Ty::F128 => Some(vec![TypeSpecifier::Long, TypeSpecifier::Double]),

            Ty::Ptr(_) => None,
            Ty::Array(_, _) => None,
            Ty::Func(_, _) => None,
            Ty::Struct(name) => Some(vec![TypeSpecifier::StructOrUnionSpecifier(
                StructOrUnionSpecifier::Identifier(StructOrUnion::Struct, name.clone()),
            )]),
            Ty::Union(name) => Some(vec![TypeSpecifier::StructOrUnionSpecifier(
                StructOrUnionSpecifier::Identifier(StructOrUnion::Union, name.clone()),
            )]),
            Ty::Enum(name) => Some(vec![TypeSpecifier::EnumSpecifier(
                EnumSpecifier::Identifier(name.clone()),
            )]),
        }
    }
}
