use crate::parser::{
    ConstantExpr, DeclarationSpecifier, Declarator, DirectDeclarator, ExprKind, Primary,
    TypeSpecifier, Unary,
};

#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Void,
    Char,
    Int,
    Ptr(Box<Type>),
    Array(Box<Type>, usize),
    Func(Box<Type>, Vec<Type>),
}

impl Type {
    pub fn sizeof(&self) -> usize {
        match self {
            Type::Void => 0,
            Type::Char => 1,
            Type::Int => 4,
            Type::Ptr(_) => 8,
            Type::Array(t, size) => t.sizeof() * size,
            Type::Func(_, _) => panic!("sizeof function is not allowed"),
        }
    }

    pub fn is_compatible_type(&self, other: &Self) -> bool {
        match (self, other) {
            (t1, t2) if t1 == t2 => true,
            (Type::Int, Type::Char) => true,
            (Type::Char, Type::Int) => true,
            _ => false,
        }
    }

    pub fn get_inner(&self) -> Option<Type> {
        match self {
            Type::Ptr(ty) | Type::Array(ty, _) => Some(*ty.clone()),
            _ => None,
        }
    }

    pub fn from_specs_and_declarator(
        specs: &Vec<DeclarationSpecifier>,
        declarator: &Declarator,
    ) -> Type {
        Self::_convert_type(Self::get_primary_type(specs), declarator)
    }

    fn get_primary_type(specs: &Vec<DeclarationSpecifier>) -> Type {
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

    fn get_var_type_from_direct_declarator(t: Type, direct_declarator: &DirectDeclarator) -> Type {
        match direct_declarator {
            DirectDeclarator::Ident(_) => t,
            DirectDeclarator::Declarator(d) => Self::_convert_type(t, d),
            DirectDeclarator::Array(dd, e) => {
                let t = Self::get_var_type_from_direct_declarator(t, dd);
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

    fn _convert_type(mut t: Type, declarator: &Declarator) -> Type {
        let mut pointer = &declarator.pointer;
        while let Some(p) = pointer {
            t = Type::Ptr(Box::new(t));
            pointer = &p.pointer;
        }

        Self::get_var_type_from_direct_declarator(t, &declarator.direct)
    }
}
