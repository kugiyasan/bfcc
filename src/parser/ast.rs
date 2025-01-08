#![allow(dead_code)]

use super::{ConstantExpr, Expr};

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
    DeclaratorAndInitializer(Declarator, ()), // todo
}

#[derive(Clone, Debug, PartialEq)]
pub enum SpecifierQualifier {
    TypeSpecifier(TypeSpecifier),
    TypeQualifier(TypeQualifier),
}

#[derive(Clone, Debug, PartialEq)]
pub struct StructDeclaration {
    pub specs: Vec<SpecifierQualifier>,
    pub declarators: Vec<StructDeclarator>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum StructDeclarator {
    Declarator(Declarator),
    WithExpr(Option<Declarator>, ConstantExpr),
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
    pub types: Vec<TypeQualifier>,
    pub pointer: Box<Option<Pointer>>,
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
    AbstractDeclarator(Vec<DeclarationSpecifier>, Box<Declarator>), // todo
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

    Goto(String),
    Continue,
    Break,
    Return(Expr),
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
