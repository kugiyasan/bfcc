use crate::parser::{
    ConstantExpr, DeclarationSpecifier, Declarator, DirectDeclarator, ExprKind, Primary,
    StructDeclarator, StructOrUnion, StructOrUnionSpecifier, TypeSpecifier, TypeSpecifierTrait,
    Unary,
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
    Struct(Vec<(String, Ty)>),
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
            Ty::Struct(tys) => tys.iter().map(|t| t.1.sizeof()).sum(),
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

    pub fn from_specs_and_declarator(
        specs: &Vec<DeclarationSpecifier>,
        declarator: &Declarator,
    ) -> Ty {
        Self::parse_declarator(Self::parse_primary_type(specs), declarator)
    }

    fn parse_primary_type<T>(specs: &Vec<T>) -> Ty
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
                        Self::parse_struct_or_union_specifier(s)
                    }
                    _ => todo!(),
                };
            }
        }
        panic!("Variable of unknown type");
    }

    fn parse_struct_or_union_specifier(s: &StructOrUnionSpecifier) -> Ty {
        let mut tys = vec![];

        match s {
            StructOrUnionSpecifier::WithDeclaration(StructOrUnion::Struct, ident, sds) => {
                for struct_declaration in sds {
                    let ty = Self::parse_primary_type(&struct_declaration.specs);
                    for sd in struct_declaration.declarators.iter() {
                        let StructDeclarator::Declarator(d) = sd else {
                            todo!();
                        };
                        let t = Self::parse_declarator(ty.clone(), d);
                        tys.push((d.direct.get_name(), t));
                    }
                }
            }
            _ => todo!(),
        };

        Ty::Struct(tys)
    }

    fn parse_declarator(mut t: Ty, declarator: &Declarator) -> Ty {
        let mut pointer = &declarator.pointer;
        while let Some(p) = pointer {
            t = Ty::Ptr(Box::new(t));
            pointer = &p.pointer;
        }

        Self::parse_direct_declarator(t, &declarator.direct)
    }

    fn parse_direct_declarator(t: Ty, direct_declarator: &DirectDeclarator) -> Ty {
        match direct_declarator {
            DirectDeclarator::Ident(_) => t,
            DirectDeclarator::Declarator(d) => Self::parse_declarator(t, d),
            DirectDeclarator::Array(dd, e) => {
                let t = Self::parse_direct_declarator(t, dd);
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
