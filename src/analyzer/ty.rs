use crate::parser::{
    ConstantExpr, DeclarationSpecifier, Declarator, DirectDeclarator, ExprKind, Primary,
    TypeSpecifier, Unary,
};

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
}

impl Ty {
    pub fn sizeof(&self) -> usize {
        match self {
            Ty::Void => 0,
            Ty::Char => 1,
            Ty::Short => 2,
            Ty::Int => 4,
            Ty::Long => 8,
            Ty::Ptr(_) => 8,
            Ty::Array(t, size) => t.sizeof() * size,
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
        match self {
            Ty::Char | Ty::Short | Ty::Int | Ty::Long => true,
            _ => false,
        }
    }

    pub fn get_inner(&self) -> Option<Ty> {
        match self {
            Ty::Ptr(ty) | Ty::Array(ty, _) => Some(*ty.clone()),
            _ => None,
        }
    }

    pub fn from_specs_and_declarator(
        specs: &Vec<DeclarationSpecifier>,
        declarator: &Declarator,
    ) -> Ty {
        Self::_convert_type(Self::get_primary_type(specs), declarator)
    }

    fn get_primary_type(specs: &Vec<DeclarationSpecifier>) -> Ty {
        for spec in specs {
            if let DeclarationSpecifier::TypeSpecifier(ts) = spec {
                return match ts {
                    TypeSpecifier::Void => Ty::Void,
                    TypeSpecifier::Char => Ty::Char,
                    TypeSpecifier::Short => Ty::Short,
                    TypeSpecifier::Int => Ty::Int,
                    TypeSpecifier::Long => Ty::Long,
                    _ => todo!(),
                };
            }
        }
        panic!("Variable of unknown type");
    }

    fn get_var_type_from_direct_declarator(t: Ty, direct_declarator: &DirectDeclarator) -> Ty {
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
                Ty::Array(Box::new(t), *size as usize)
            }
            _ => todo!(),
        }
    }

    fn _convert_type(mut t: Ty, declarator: &Declarator) -> Ty {
        let mut pointer = &declarator.pointer;
        while let Some(p) = pointer {
            t = Ty::Ptr(Box::new(t));
            pointer = &p.pointer;
        }

        Self::get_var_type_from_direct_declarator(t, &declarator.direct)
    }
}
