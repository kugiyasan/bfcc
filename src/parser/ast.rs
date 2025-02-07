use super::{Assign, ConstantExpr, Expr};

#[derive(Clone, Debug, PartialEq)]
pub struct TranslationUnit(pub Vec<ExternalDeclaration>);

#[derive(Clone, Debug, PartialEq)]
pub enum ExternalDeclaration {
    FuncDef(FuncDef),
    Declaration(Declaration),
}

#[derive(Clone, Debug, PartialEq)]
pub struct FuncDef {
    pub specs: Vec<DeclarationSpecifier>,
    pub declarator: Declarator,
    pub stmt: CompoundStmt,
}

#[derive(Clone, Debug, PartialEq)]
pub enum DeclarationSpecifier {
    StorageClassSpecifier(StorageClassSpecifier),
    TypeSpecifier(TypeSpecifier),
    TypeQualifier(TypeQualifier),
}

impl DeclarationSpecifier {
    pub fn is_extern(&self) -> bool {
        *self == DeclarationSpecifier::StorageClassSpecifier(StorageClassSpecifier::Extern)
    }

    pub fn is_static(&self) -> bool {
        *self == DeclarationSpecifier::StorageClassSpecifier(StorageClassSpecifier::Static)
    }

    pub fn is_typedef(&self) -> bool {
        *self == DeclarationSpecifier::StorageClassSpecifier(StorageClassSpecifier::Typedef)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum StorageClassSpecifier {
    Auto,
    Register,
    Static,
    Extern,
    Typedef,
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeSpecifier {
    Void,
    Bool,
    Char,
    Short,
    Int,
    Long,
    Float,
    Double,
    Signed,
    Unsigned,
    StructOrUnionSpecifier(StructOrUnionSpecifier),
    EnumSpecifier(EnumSpecifier),
    TypedefName(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum TypeQualifier {
    Const,
    Volatile,
}

#[derive(Clone, Debug, PartialEq)]
pub enum StructOrUnionSpecifier {
    WithDeclaration(StructOrUnion, Option<String>, Vec<StructDeclaration>),
    Identifier(StructOrUnion, String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum StructOrUnion {
    Struct,
    Union,
}

#[derive(Clone, Debug, PartialEq)]
pub enum InitDeclarator {
    Declarator(Declarator),
    DeclaratorAndInitializer(Declarator, Initializer),
}

#[derive(Clone, Debug, PartialEq)]
pub enum SpecifierQualifier {
    TypeSpecifier(TypeSpecifier),
    TypeQualifier(TypeQualifier),
}

pub trait TypeSpecifierTrait {
    fn get_type_specifier(&self) -> Option<&TypeSpecifier>;
}

impl TypeSpecifierTrait for DeclarationSpecifier {
    fn get_type_specifier(&self) -> Option<&TypeSpecifier> {
        if let DeclarationSpecifier::TypeSpecifier(ts) = self {
            Some(ts)
        } else {
            None
        }
    }
}

impl TypeSpecifierTrait for SpecifierQualifier {
    fn get_type_specifier(&self) -> Option<&TypeSpecifier> {
        if let SpecifierQualifier::TypeSpecifier(ts) = self {
            Some(ts)
        } else {
            None
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructDeclaration {
    pub specs: Vec<SpecifierQualifier>,
    pub declarators: Vec<StructDeclarator>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum StructDeclarator {
    Declarator(Declarator),
    BitField(Option<Declarator>, ConstantExpr),
}

#[derive(Clone, Debug, PartialEq)]
pub enum EnumSpecifier {
    WithEnumerator(Option<String>, Vec<Enumerator>),
    Identifier(String),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Enumerator {
    Identifier(String),
    Init(String, ConstantExpr),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Declarator {
    pub pointer: Option<Pointer>,
    pub direct: DirectDeclarator,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Pointer {
    pub qualifiers: Vec<TypeQualifier>,
    pub pointer: Box<Option<Pointer>>,
}

impl Pointer {
    pub fn get_number_of_consumed_tokens(&self) -> usize {
        let ct = &(*self.pointer)
            .as_ref()
            .map_or(0, |p| p.get_number_of_consumed_tokens());

        1 + self.qualifiers.len() + ct
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum DirectDeclarator {
    Ident(String),
    Declarator(Box<Declarator>),
    Array(Box<DirectDeclarator>, Option<ConstantExpr>),
    ParamTypeList(Box<DirectDeclarator>, ParamTypeList),
}

impl DirectDeclarator {
    pub fn get_name(&self) -> String {
        match self {
            DirectDeclarator::Ident(ident) => ident.to_string(),
            DirectDeclarator::Declarator(d) => d.direct.get_name(),
            DirectDeclarator::Array(dd, _) => dd.get_name(),
            DirectDeclarator::ParamTypeList(dd, _) => dd.get_name(),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ParamTypeList {
    pub params: Vec<ParamDeclaration>,
    pub variadic: bool,
}

#[derive(Clone, Debug, PartialEq)]
pub enum ParamDeclaration {
    Declarator(Vec<DeclarationSpecifier>, Box<Declarator>),
    AbstractDeclarator(Vec<DeclarationSpecifier>, Option<Box<AbstractDeclarator>>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Initializer {
    Assign(Assign),
    Vec(Vec<Initializer>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeName {
    pub specs: Vec<SpecifierQualifier>,
    pub declarator: Option<AbstractDeclarator>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum AbstractDeclarator {
    Pointer(Pointer),
    DirectAbstractDeclarator(Option<Pointer>, DirectAbstractDeclarator),
}

#[derive(Clone, Debug, PartialEq)]
pub enum DirectAbstractDeclarator {
    AbstractDeclarator(Box<AbstractDeclarator>),
    Array(Option<Box<DirectAbstractDeclarator>>, Option<ConstantExpr>),
    ParamTypeList(Option<Box<DirectAbstractDeclarator>>, Option<ParamTypeList>),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Stmt {
    Label(String, Box<Stmt>),
    Case(ConstantExpr, Box<Stmt>),
    Default(Box<Stmt>),

    SemiColon,
    Expr(Expr),
    Compound(CompoundStmt),

    If(Expr, Box<Stmt>, Option<Box<Stmt>>),
    Switch(Expr, Box<Stmt>),

    While(Expr, Box<Stmt>),
    DoWhile(Box<Stmt>, Expr),
    For(Option<Expr>, Option<Expr>, Option<Expr>, Box<Stmt>),
    ForWithDeclaration(Declaration, Option<Expr>, Option<Expr>, Box<Stmt>),

    Goto(String),
    Continue,
    Break,
    Return(Option<Expr>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct CompoundStmt(pub Vec<DeclarationOrStmt>);

#[derive(Clone, Debug, PartialEq)]
pub enum DeclarationOrStmt {
    Declaration(Declaration),
    Stmt(Stmt),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Declaration {
    pub specs: Vec<DeclarationSpecifier>,
    pub inits: Vec<InitDeclarator>,
}
