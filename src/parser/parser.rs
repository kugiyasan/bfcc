use crate::lexer::{Token, TokenKind};

use super::{
    AbstractDeclarator, Assign, AssignOpKind, BinOp, BinOpKind, CompoundStmt, ConstantExpr,
    Declaration, DeclarationOrStmt, DeclarationSpecifier, Declarator, DirectAbstractDeclarator,
    DirectDeclarator, EnumSpecifier, Enumerator, Expr, ExternalDeclaration, FuncDef,
    InitDeclarator, Initializer, ParamDeclaration, ParamTypeList, Pointer, Primary,
    SpecifierQualifier, Stmt, StorageClassSpecifier, StructDeclaration, StructDeclarator,
    StructOrUnion, StructOrUnionSpecifier, TranslationUnit, TypeName, TypeQualifier, TypeSpecifier,
    Unary,
};

pub type Typedefs = Vec<(String, Vec<DeclarationSpecifier>, Declarator)>;

pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
    typedefs: Typedefs,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            index: 0,
            typedefs: Vec::new(),
        }
    }

    pub fn get_typedefs(&self) -> &Typedefs {
        &self.typedefs
    }

    pub fn parse(&mut self) -> TranslationUnit {
        self.parse_translation_unit()
    }

    fn is_eof(&mut self) -> bool {
        self.index >= self.tokens.len()
    }

    fn consume(&mut self, kind: &TokenKind) -> bool {
        if self.is_eof() {
            return false;
        }
        let t = &self.tokens[self.index];
        if &t.kind != kind {
            return false;
        }
        self.index += 1;
        true
    }

    fn consume_ident(&mut self) -> Option<String> {
        if self.is_eof() {
            return None;
        }
        let t = self.tokens[self.index].clone();
        if let TokenKind::Ident(name) = t.kind {
            self.index += 1;
            Some(name)
        } else {
            None
        }
    }

    fn expect(&mut self, kind: &TokenKind) {
        if self.is_eof() {
            panic!("Expected {:?}, however received EOF", kind);
        }
        let t = &self.tokens[self.index];
        if &t.kind != kind {
            panic!(
                "Unexpected token at index {}: {:?} (was expecting {:?})",
                self.index, t.kind, kind
            );
        }
        self.index += 1;
    }

    fn expect_ident(&mut self) -> String {
        if self.is_eof() {
            panic!("Unexpected EOF");
        }
        let k = self.tokens[self.index].kind.clone();
        if let TokenKind::Ident(name) = k {
            self.index += 1;
            return name;
        }
        panic!(
            "Unexpected token at index {}: {:?} (was expecting Ident)",
            self.index, k
        );
    }

    fn peek(&mut self, offset: usize) -> &TokenKind {
        if self.is_eof() {
            panic!("Unexpected EOF");
        }

        &self.tokens[self.index + offset].kind
    }

    /// translation-unit = (external-declaration)*
    fn parse_translation_unit(&mut self) -> TranslationUnit {
        let mut external_declarations = vec![];

        while !self.is_eof() {
            external_declarations.push(self.parse_external_declaration());
        }

        TranslationUnit(external_declarations)
    }

    /// external-declaration = func-def | declaration
    /// func-def = declaration-specifiers? declarator compound-stmt
    fn parse_external_declaration(&mut self) -> ExternalDeclaration {
        let specs = self.parse_declaration_specifiers();
        let Some(declarator) = self.parse_declarator() else {
            self.expect(&TokenKind::SemiColon);
            return ExternalDeclaration::Declaration(Declaration {
                specs,
                inits: vec![],
            });
        };

        if self.consume(&TokenKind::LeftCurlyBrace) {
            let mut stmts = vec![];
            while !self.consume(&TokenKind::RightCurlyBrace) {
                stmts.push(self.parse_declaration_or_stmt());
            }
            let fd = FuncDef {
                specs,
                declarator,
                stmt: CompoundStmt(stmts),
            };
            ExternalDeclaration::FuncDef(fd)
        } else {
            let init_declarator = self._parse_init_declarator(declarator);
            let d = self._parse_declaration(specs, init_declarator);
            ExternalDeclaration::Declaration(d)
        }
    }

    fn parse_declaration_or_stmt(&mut self) -> DeclarationOrStmt {
        if let Some(d) = self.parse_declaration() {
            DeclarationOrStmt::Declaration(d)
        } else {
            DeclarationOrStmt::Stmt(self.parse_stmt())
        }
    }

    /// declaration = declaration-specifiers (init-declarator ("," init-declarator)*)? ";"
    fn parse_declaration(&mut self) -> Option<Declaration> {
        let specs = self.parse_declaration_specifiers();
        if specs.is_empty() {
            return None;
        }

        let init_declarator = self.parse_init_declarator();
        Some(self._parse_declaration(specs, init_declarator))
    }

    fn _parse_declaration(
        &mut self,
        specs: Vec<DeclarationSpecifier>,
        init_declarator: InitDeclarator,
    ) -> Declaration {
        let mut inits = vec![init_declarator];

        if self.consume(&TokenKind::SemiColon) {
            self.insert_to_typedefs(&specs, &inits);
            return Declaration { specs, inits };
        }

        while self.consume(&TokenKind::Comma) {
            inits.push(self.parse_init_declarator());
        }

        self.expect(&TokenKind::SemiColon);

        self.insert_to_typedefs(&specs, &inits);
        Declaration { specs, inits }
    }

    fn insert_to_typedefs(
        &mut self,
        specs: &Vec<DeclarationSpecifier>,
        inits: &Vec<InitDeclarator>,
    ) {
        if specs.iter().any(|s| s.is_typedef()) {
            for init in inits {
                let InitDeclarator::Declarator(d) = init else {
                    panic!("Illegal initializer in typedef");
                };

                self.typedefs
                    .push((d.direct.get_name(), specs.clone(), d.clone()));
            }
        }
    }

    /// declaration-specifiers = declaration-specifier*
    fn parse_declaration_specifiers(&mut self) -> Vec<DeclarationSpecifier> {
        let mut declaration_specifiers = vec![];

        while let Some(ds) = self.parse_declaration_specifier() {
            declaration_specifiers.push(ds);
        }

        declaration_specifiers
    }

    /// declaration-specifier = storage-class-specifier
    ///                       | type-specifier
    ///                       | type-qualifier
    fn parse_declaration_specifier(&mut self) -> Option<DeclarationSpecifier> {
        if let Some(s) = self.parse_storage_class_specifier() {
            Some(DeclarationSpecifier::StorageClassSpecifier(s))
        } else if let Some(s) = self.parse_type_specifier() {
            Some(DeclarationSpecifier::TypeSpecifier(s))
        } else {
            self.parse_type_qualifier()
                .map(DeclarationSpecifier::TypeQualifier)
        }
    }

    /// storage-class-specifier = "auto" | "register" | "static" | "extern" | "typedef"
    fn parse_storage_class_specifier(&mut self) -> Option<StorageClassSpecifier> {
        if self.consume(&TokenKind::Auto) {
            Some(StorageClassSpecifier::Auto)
        } else if self.consume(&TokenKind::Register) {
            Some(StorageClassSpecifier::Register)
        } else if self.consume(&TokenKind::Static) {
            Some(StorageClassSpecifier::Static)
        } else if self.consume(&TokenKind::Extern) {
            Some(StorageClassSpecifier::Extern)
        } else if self.consume(&TokenKind::Typedef) {
            Some(StorageClassSpecifier::Typedef)
        } else {
            None
        }
    }

    fn parse_type_specifier(&mut self) -> Option<TypeSpecifier> {
        if self.consume(&TokenKind::Void) {
            Some(TypeSpecifier::Void)
        } else if self.consume(&TokenKind::Bool) {
            Some(TypeSpecifier::Bool)
        } else if self.consume(&TokenKind::Char) {
            Some(TypeSpecifier::Char)
        } else if self.consume(&TokenKind::Short) {
            Some(TypeSpecifier::Short)
        } else if self.consume(&TokenKind::Int) {
            Some(TypeSpecifier::Int)
        } else if self.consume(&TokenKind::Long) {
            Some(TypeSpecifier::Long)
        } else if self.consume(&TokenKind::Float) {
            Some(TypeSpecifier::Float)
        } else if self.consume(&TokenKind::Double) {
            Some(TypeSpecifier::Double)
        } else if self.consume(&TokenKind::Signed) {
            Some(TypeSpecifier::Signed)
        } else if self.consume(&TokenKind::Unsigned) {
            Some(TypeSpecifier::Unsigned)
        } else if self.peek(0) == &TokenKind::Struct || self.peek(0) == &TokenKind::Union {
            let spec = self.parse_struct_or_union_specifier();
            Some(TypeSpecifier::StructOrUnionSpecifier(spec))
        } else if self.consume(&TokenKind::Enum) {
            let spec = self.parse_enum_specifier();
            Some(TypeSpecifier::EnumSpecifier(spec))
        } else if let Some(ident) = self.consume_ident() {
            if self.typedefs.iter().any(|(k, _, _)| k == &ident) {
                Some(TypeSpecifier::TypedefName(ident))
            } else {
                self.index -= 1;
                None
            }
        } else {
            None
        }
    }

    /// type-qualifier = "const" | "volatile"
    fn parse_type_qualifier(&mut self) -> Option<TypeQualifier> {
        if self.consume(&TokenKind::Const) {
            Some(TypeQualifier::Const)
        } else if self.consume(&TokenKind::Volatile) {
            Some(TypeQualifier::Volatile)
        } else {
            None
        }
    }

    /// struct-or-union-specifier = struct-or-union identifier? "{" struct-declaration+ "}"
    ///                           | struct-or-union identifier
    fn parse_struct_or_union_specifier(&mut self) -> StructOrUnionSpecifier {
        let struct_or_union = self.parse_struct_or_union();
        let name = self.consume_ident();

        if self.consume(&TokenKind::LeftCurlyBrace) {
            let mut struct_declarations = vec![self.parse_struct_declaration()];
            while !self.consume(&TokenKind::RightCurlyBrace) {
                struct_declarations.push(self.parse_struct_declaration());
            }
            StructOrUnionSpecifier::WithDeclaration(struct_or_union, name, struct_declarations)
        } else {
            StructOrUnionSpecifier::Identifier(struct_or_union, name.unwrap())
        }
    }

    /// struct-or-union = "struct" | "union"
    fn parse_struct_or_union(&mut self) -> StructOrUnion {
        if self.consume(&TokenKind::Struct) {
            StructOrUnion::Struct
        } else if self.consume(&TokenKind::Union) {
            StructOrUnion::Union
        } else {
            panic!();
        }
    }

    /// init-declarator = declarator
    ///                 | declarator "=" initializer
    fn parse_init_declarator(&mut self) -> InitDeclarator {
        let d = self.parse_declarator().unwrap();
        self._parse_init_declarator(d)
    }

    fn _parse_init_declarator(&mut self, declarator: Declarator) -> InitDeclarator {
        if self.consume(&TokenKind::Equal) {
            let i = self.parse_initializer();
            InitDeclarator::DeclaratorAndInitializer(declarator, i)
        } else {
            InitDeclarator::Declarator(declarator)
        }
    }

    /// struct-declaration = specifier-qualifier+ struct-declarator+ ";"
    fn parse_struct_declaration(&mut self) -> StructDeclaration {
        let mut specs = vec![self.parse_specifier_qualifier().unwrap()];
        while let Some(sq) = self.parse_specifier_qualifier() {
            specs.push(sq);
        }

        let mut declarators = vec![self.parse_struct_declarator().unwrap()];
        while self.consume(&TokenKind::Comma) {
            declarators.push(self.parse_struct_declarator().unwrap());
        }

        self.expect(&TokenKind::SemiColon);
        StructDeclaration { specs, declarators }
    }

    /// specifier-qualifier = type-specifier
    ///                     | type-qualifier
    fn parse_specifier_qualifier(&mut self) -> Option<SpecifierQualifier> {
        if let Some(ts) = self.parse_type_specifier() {
            Some(SpecifierQualifier::TypeSpecifier(ts))
        } else {
            self.parse_type_qualifier()
                .map(SpecifierQualifier::TypeQualifier)
        }
    }

    /// struct-declarator = declarator
    ///                   | declarator? ":" constant-expr
    fn parse_struct_declarator(&mut self) -> Option<StructDeclarator> {
        let d = self.parse_declarator();
        if self.consume(&TokenKind::Colon) {
            let c = self.parse_constant_expr();
            Some(StructDeclarator::BitField(d, c))
        } else {
            Some(StructDeclarator::Declarator(d?))
        }
    }

    /// enum-specifier = "enum" identifier? "{" enumerator ("," enumerator)* ","? "}"
    ///                | "enum" identifier
    /// enumerator = identifier
    ///            | identifier "=" constant-expr
    fn parse_enum_specifier(&mut self) -> EnumSpecifier {
        // TokenKind::Enum has already been consumed
        let ident = self.consume_ident();

        if self.consume(&TokenKind::LeftCurlyBrace) {
            let mut enumerator = vec![];
            while !self.consume(&TokenKind::RightCurlyBrace) {
                let i = self.expect_ident();
                if self.consume(&TokenKind::Equal) {
                    let c = self.parse_constant_expr();
                    enumerator.push(Enumerator::Init(i, c));
                } else {
                    enumerator.push(Enumerator::Identifier(i));
                }
                if !self.consume(&TokenKind::Comma) {
                    self.expect(&TokenKind::RightCurlyBrace);
                    break;
                }
            }

            EnumSpecifier::WithEnumerator(ident, enumerator)
        } else {
            EnumSpecifier::Identifier(ident.unwrap())
        }
    }

    /// declarator = pointer? direct-declarator
    fn parse_declarator(&mut self) -> Option<Declarator> {
        let pointer = self.parse_pointer();

        if let Some(direct) = self.parse_direct_declarator() {
            Some(Declarator { pointer, direct })
        } else {
            if let Some(p) = pointer {
                self.index -= p.get_number_of_consumed_tokens();
            }
            None
        }
    }

    /// direct-declarator = identifier
    ///                   | (declarator)
    ///                   | direct-declarator [ constant-expression? ]
    ///                   | direct-declarator ( parameter-type-list? )
    fn parse_direct_declarator(&mut self) -> Option<DirectDeclarator> {
        let mut dd = if self.consume(&TokenKind::LeftParen) {
            let d = self.parse_declarator().unwrap();
            self.expect(&TokenKind::RightParen);
            DirectDeclarator::Declarator(Box::new(d))
        } else {
            let name = self.consume_ident()?;
            DirectDeclarator::Ident(name.clone())
        };

        loop {
            if self.consume(&TokenKind::LeftSquareBrace) {
                if self.consume(&TokenKind::RightSquareBrace) {
                    dd = DirectDeclarator::Array(Box::new(dd), None);
                } else {
                    let expr = Some(self.parse_constant_expr());
                    self.expect(&TokenKind::RightSquareBrace);
                    dd = DirectDeclarator::Array(Box::new(dd), expr);
                }
            } else if self.consume(&TokenKind::LeftParen) {
                if self.consume(&TokenKind::RightParen) {
                    let ptl = ParamTypeList {
                        params: vec![],
                        variadic: false,
                    };
                    dd = DirectDeclarator::ParamTypeList(Box::new(dd), ptl);
                } else {
                    let ptl = self
                        .parse_param_type_list()
                        .expect("Expected ParamTypeList");
                    self.expect(&TokenKind::RightParen);
                    dd = DirectDeclarator::ParamTypeList(Box::new(dd), ptl);
                }
            } else {
                return Some(dd);
            }
        }
    }

    /// pointer = "*" type-qualifier* pointer?
    fn parse_pointer(&mut self) -> Option<Pointer> {
        if !self.consume(&TokenKind::Star) {
            return None;
        };

        let mut qualifiers = vec![];
        while let Some(t) = self.parse_type_qualifier() {
            qualifiers.push(t);
        }

        Some(Pointer {
            qualifiers,
            pointer: Box::new(self.parse_pointer()),
        })
    }

    /// parameter-type-list = param-declaration* ("," ...)?
    fn parse_param_type_list(&mut self) -> Option<ParamTypeList> {
        let pd = self.parse_param_declaration()?;
        let mut pds = vec![pd];
        while self.consume(&TokenKind::Comma) {
            if let Some(pd) = self.parse_param_declaration() {
                pds.push(pd);
            };
            if self.consume(&TokenKind::ThreeDots) {
                return Some(ParamTypeList {
                    params: pds,
                    variadic: true,
                });
            }
        }

        Some(ParamTypeList {
            params: pds,
            variadic: false,
        })
    }

    /// param-declaration = declaration-specifiers declarator
    ///                   | declaration-specifiers abstract-declarator?
    fn parse_param_declaration(&mut self) -> Option<ParamDeclaration> {
        let specs = self.parse_declaration_specifiers();
        if specs.is_empty() {
            return None;
        }

        if let Some(d) = self.parse_declarator() {
            Some(ParamDeclaration::Declarator(specs, Box::new(d)))
        } else {
            let ad = self.parse_abstract_declarator();
            Some(ParamDeclaration::AbstractDeclarator(
                specs,
                ad.map(Box::new),
            ))
        }
    }

    /// initializer = assign
    ///             | "{" (initializer ",")* (initializer ","?)? "}"
    /// **This initializer allows the non-standard empty initializer**
    fn parse_initializer(&mut self) -> Initializer {
        if self.consume(&TokenKind::LeftCurlyBrace) {
            let mut inits = vec![];
            while !self.consume(&TokenKind::RightCurlyBrace) {
                inits.push(self.parse_initializer());
                if !self.consume(&TokenKind::Comma) && self.consume(&TokenKind::RightCurlyBrace) {
                    break;
                }
            }
            Initializer::Vec(inits)
        } else {
            Initializer::Assign(self.parse_assign())
        }
    }

    /// type-name = specifier-qualifier+ abstract-declarator?
    fn parse_type_name(&mut self) -> Option<TypeName> {
        let mut specs = vec![self.parse_specifier_qualifier()?];
        while let Some(sq) = self.parse_specifier_qualifier() {
            specs.push(sq);
        }
        let ad = self.parse_abstract_declarator();
        Some(TypeName {
            specs,
            declarator: ad,
        })
    }

    /// abstract-declarator = pointer
    ///                     | pointer? direct-abstract-declarator
    fn parse_abstract_declarator(&mut self) -> Option<AbstractDeclarator> {
        let p = self.parse_pointer();
        if let Some(dad) = self.parse_direct_abstract_declarator() {
            Some(AbstractDeclarator::DirectAbstractDeclarator(p, dad))
        } else {
            p.map(AbstractDeclarator::Pointer)
        }
    }

    /// direct-abstract-declarator = "(" abstract-declarator ")"
    ///                            | direct-abstract-declarator? "[" constant-expr? "]"
    ///                            | direct-abstract-declarator? "(" param-type-list? ")"
    fn parse_direct_abstract_declarator(&mut self) -> Option<DirectAbstractDeclarator> {
        if self.consume(&TokenKind::LeftParen) {
            let ad = self.parse_abstract_declarator().unwrap();
            self.expect(&TokenKind::RightParen);
            return Some(DirectAbstractDeclarator::AbstractDeclarator(Box::new(ad)));
        }

        let mut node = None;

        loop {
            if self.consume(&TokenKind::LeftSquareBrace) {
                if self.consume(&TokenKind::RightSquareBrace) {
                    node = Some(DirectAbstractDeclarator::Array(node.map(Box::new), None));
                } else {
                    let ce = self.parse_constant_expr();
                    self.expect(&TokenKind::RightSquareBrace);
                    node = Some(DirectAbstractDeclarator::Array(
                        node.map(Box::new),
                        Some(ce),
                    ));
                }
            } else if self.consume(&TokenKind::LeftParen) {
                let ptl = self.parse_param_type_list();
                self.expect(&TokenKind::RightParen);
                node = Some(DirectAbstractDeclarator::ParamTypeList(
                    node.map(Box::new),
                    ptl,
                ));
            } else {
                return node;
            }
        }
    }

    /// stmt = ";"
    ///      | "case" constant-expr ":" stmt
    ///      | "default" ":" stmt
    ///      | "{" stmt* "}"
    ///      | "if" "(" expr ")" stmt ("else" stmt)?
    ///      | "switch" "(" expr ")" stmt
    ///      | "while" "(" expr ")" stmt
    ///      | "do" stmt "while" "(" expr ")" ";"
    ///      | "for" "(" declaration expr? ";" expr? ")" stmt
    ///      | "for" "(" expr? ";" expr? ";" expr? ")" stmt
    ///      | "goto" ident ";"
    ///      | "continue" ";"
    ///      | "break" ";"
    ///      | "return" expr? ";"
    ///      | ident ":" stmt
    ///      | expr ";"
    fn parse_stmt(&mut self) -> Stmt {
        if self.consume(&TokenKind::SemiColon) {
            Stmt::SemiColon
        } else if self.consume(&TokenKind::Case) {
            let expr = self.parse_constant_expr();
            self.expect(&TokenKind::Colon);
            Stmt::Case(expr, Box::new(self.parse_stmt()))
        } else if self.consume(&TokenKind::Default) {
            self.expect(&TokenKind::Colon);
            Stmt::Default(Box::new(self.parse_stmt()))
        } else if self.consume(&TokenKind::LeftCurlyBrace) {
            let mut stmts = vec![];
            while !self.consume(&TokenKind::RightCurlyBrace) {
                stmts.push(self.parse_declaration_or_stmt());
            }
            Stmt::Compound(CompoundStmt(stmts))
        } else if self.consume(&TokenKind::If) {
            self.expect(&TokenKind::LeftParen);
            let expr = self.parse_expr();
            self.expect(&TokenKind::RightParen);
            let stmt = self.parse_stmt();
            let else_stmt = if self.consume(&TokenKind::Else) {
                Some(Box::new(self.parse_stmt()))
            } else {
                None
            };
            Stmt::If(expr, Box::new(stmt), else_stmt)
        } else if self.consume(&TokenKind::Switch) {
            self.expect(&TokenKind::LeftParen);
            let expr = self.parse_expr();
            self.expect(&TokenKind::RightParen);
            let stmt = self.parse_stmt();
            Stmt::Switch(expr, Box::new(stmt))
        } else if self.consume(&TokenKind::While) {
            self.expect(&TokenKind::LeftParen);
            let expr = self.parse_expr();
            self.expect(&TokenKind::RightParen);
            let stmt = self.parse_stmt();
            Stmt::While(expr, Box::new(stmt))
        } else if self.consume(&TokenKind::Do) {
            let stmt = self.parse_stmt();
            self.expect(&TokenKind::While);
            self.expect(&TokenKind::LeftParen);
            let expr = self.parse_expr();
            self.expect(&TokenKind::RightParen);
            Stmt::DoWhile(Box::new(stmt), expr)
        } else if self.consume(&TokenKind::For) {
            self.parse_for()
        } else if self.consume(&TokenKind::Goto) {
            let ident = self.expect_ident();
            self.expect(&TokenKind::SemiColon);
            Stmt::Goto(ident)
        } else if self.consume(&TokenKind::Continue) {
            self.expect(&TokenKind::SemiColon);
            Stmt::Continue
        } else if self.consume(&TokenKind::Break) {
            self.expect(&TokenKind::SemiColon);
            Stmt::Break
        } else if self.consume(&TokenKind::Return) {
            if self.consume(&TokenKind::SemiColon) {
                Stmt::Return(None)
            } else {
                let expr = self.parse_expr();
                self.expect(&TokenKind::SemiColon);
                Stmt::Return(Some(expr))
            }
        } else if self.peek(1) == &TokenKind::Colon {
            let ident = self.expect_ident();
            self.expect(&TokenKind::Colon);
            Stmt::Label(ident, Box::new(self.parse_stmt()))
        } else {
            let expr = self.parse_expr();
            self.expect(&TokenKind::SemiColon);
            Stmt::Expr(expr)
        }
    }

    fn parse_for(&mut self) -> Stmt {
        self.expect(&TokenKind::LeftParen);
        let expr1 = if !self.consume(&TokenKind::SemiColon) {
            if let Some(d) = self.parse_declaration() {
                let expr2 = self.parse_expr_opt(&TokenKind::SemiColon);
                let expr3 = self.parse_expr_opt(&TokenKind::RightParen);
                let stmt = self.parse_stmt();
                return Stmt::ForWithDeclaration(d, expr2, expr3, Box::new(stmt));
            }

            let expr = Some(self.parse_expr());
            self.expect(&TokenKind::SemiColon);
            expr
        } else {
            None
        };
        let expr2 = self.parse_expr_opt(&TokenKind::SemiColon);
        let expr3 = self.parse_expr_opt(&TokenKind::RightParen);
        let stmt = self.parse_stmt();
        Stmt::For(expr1, expr2, expr3, Box::new(stmt))
    }

    fn parse_expr_opt(&mut self, tk: &TokenKind) -> Option<Expr> {
        if !self.consume(tk) {
            let expr = Some(self.parse_expr());
            self.expect(tk);
            expr
        } else {
            None
        }
    }

    /// expr = assign ("," assign)*
    fn parse_expr(&mut self) -> Expr {
        let mut assigns = vec![self.parse_assign()];
        while self.consume(&TokenKind::Comma) {
            assigns.push(self.parse_assign());
        }

        Expr(assigns)
    }

    /// assign = constant-expr
    ///        | unary assign-op-kind   assign
    fn parse_assign(&mut self) -> Assign {
        let c = self.parse_constant_expr();
        if let Some(kind) = self.parse_assign_op_kind() {
            let ConstantExpr::Identity(BinOp::Unary(unary)) = c else {
                panic!(
                    "Unexpected non-unary expression on the left hand of an assignment: {:?}",
                    c
                );
            };
            Assign::Assign(unary, kind, Box::new(self.parse_assign()))
        } else {
            Assign::Const(c)
        }
    }

    /// assign-op-kind = "=" | "*=" | "/=" | "%=" | "+=" | "-=" | "<<=" | ">>=" | "&=" | "^=" | "|="
    fn parse_assign_op_kind(&mut self) -> Option<AssignOpKind> {
        if self.consume(&TokenKind::Equal) {
            Some(AssignOpKind::Assign)
        } else if self.consume(&TokenKind::StarEqual) {
            Some(AssignOpKind::MulAssign)
        } else if self.consume(&TokenKind::SlashEqual) {
            Some(AssignOpKind::DivAssign)
        } else if self.consume(&TokenKind::PercentEqual) {
            Some(AssignOpKind::ModAssign)
        } else if self.consume(&TokenKind::PlusEqual) {
            Some(AssignOpKind::AddAssign)
        } else if self.consume(&TokenKind::MinusEqual) {
            Some(AssignOpKind::SubAssign)
        } else if self.consume(&TokenKind::LeftShiftEqual) {
            Some(AssignOpKind::LeftShiftAssign)
        } else if self.consume(&TokenKind::RightShiftEqual) {
            Some(AssignOpKind::RightShiftAssign)
        } else if self.consume(&TokenKind::AmpersandEqual) {
            Some(AssignOpKind::AndAssign)
        } else if self.consume(&TokenKind::HatEqual) {
            Some(AssignOpKind::XorAssign)
        } else if self.consume(&TokenKind::PipeEqual) {
            Some(AssignOpKind::OrAssign)
        } else {
            None
        }
    }

    /// constant-expr = logical-or-expr
    ///               | logical-or-expr "?" expr ":" constant-expr
    fn parse_constant_expr(&mut self) -> ConstantExpr {
        let logical_or = self.parse_logical_or();

        if self.consume(&TokenKind::Question) {
            let expr = self.parse_expr();
            self.expect(&TokenKind::Colon);
            let constant_expr = self.parse_constant_expr();
            ConstantExpr::Ternary(logical_or, expr, Box::new(constant_expr))
        } else {
            ConstantExpr::Identity(logical_or)
        }
    }

    fn parse_cast_wrapped(&mut self) -> BinOp {
        BinOp::Unary(self.parse_cast())
    }

    /// cast = unary
    ///      | "(" type-name ")" cast
    fn parse_cast(&mut self) -> Unary {
        if self.consume(&TokenKind::LeftParen) {
            if let Some(tn) = self.parse_type_name() {
                self.expect(&TokenKind::RightParen);
                return Unary::Cast(Box::new(tn), Box::new(self.parse_cast()));
            } else {
                self.index -= 1;
            }
        }
        self.parse_unary()
    }

    /// unary = postfix-expr
    ///       | "+" cast
    ///       | "-" cast
    ///       | "*" cast
    ///       | "&" cast
    ///       | "~" cast
    ///       | "!" cast
    ///       | "++" unary
    ///       | "--" unary
    ///       | "sizeof" unary
    ///       | "sizeof" "(" type-name ")"
    fn parse_unary(&mut self) -> Unary {
        if self.consume(&TokenKind::Plus) {
            self.parse_cast()
        } else if self.consume(&TokenKind::Minus) {
            Unary::Neg(Box::new(self.parse_cast()))
        } else if self.consume(&TokenKind::Ampersand) {
            Unary::Ref(Box::new(self.parse_cast()))
        } else if self.consume(&TokenKind::Star) {
            Unary::Deref(Box::new(self.parse_cast()))
        } else if self.consume(&TokenKind::Tilde) {
            Unary::BitwiseNot(Box::new(self.parse_cast()))
        } else if self.consume(&TokenKind::Exclamation) {
            Unary::LogicalNot(Box::new(self.parse_cast()))
        } else if self.consume(&TokenKind::PlusPlus) {
            Unary::PrefixIncrement(Box::new(self.parse_cast()))
        } else if self.consume(&TokenKind::MinusMinus) {
            Unary::PrefixDecrement(Box::new(self.parse_cast()))
        } else if self.consume(&TokenKind::Sizeof) {
            if self.consume(&TokenKind::LeftParen) {
                if let Some(tn) = self.parse_type_name() {
                    self.expect(&TokenKind::RightParen);
                    return Unary::SizeofType(Box::new(tn));
                } else {
                    self.index -= 1;
                }
            }
            Unary::Sizeof(Box::new(self.parse_unary()))
        } else {
            self.parse_postfix_expr()
        }
    }

    /// postfix-expr = primary
    ///              | postfix-expr "[" expr "]"
    ///              | postfix-expr "(" expr? ")"
    ///              | postfix-expr "." ident
    ///              | postfix-expr "->" ident
    ///              | postfix-expr "++"
    ///              | postfix-expr "--"
    fn parse_postfix_expr(&mut self) -> Unary {
        let mut node = Unary::Identity(self.parse_primary());

        loop {
            if self.consume(&TokenKind::LeftSquareBrace) {
                let expr = self.parse_expr();
                self.expect(&TokenKind::RightSquareBrace);
                node = Unary::Index(Box::new(node), expr);
            } else if self.consume(&TokenKind::LeftParen) {
                if self.consume(&TokenKind::RightParen) {
                    node = Unary::Call(Box::new(node), None);
                } else {
                    let expr = self.parse_expr();
                    self.expect(&TokenKind::RightParen);
                    node = Unary::Call(Box::new(node), Some(expr));
                }
            } else if self.consume(&TokenKind::Dot) {
                let ident = self.expect_ident();
                node = Unary::Field(Box::new(node), ident);
            } else if self.consume(&TokenKind::Arrow) {
                let ident = self.expect_ident();
                node = Unary::PointerField(Box::new(node), ident);
            } else if self.consume(&TokenKind::PlusPlus) {
                node = Unary::PostfixIncrement(Box::new(node));
            } else if self.consume(&TokenKind::MinusMinus) {
                node = Unary::PostfixDecrement(Box::new(node));
            } else {
                return node;
            }
        }
    }

    /// primary = ident
    ///         | num
    ///         | string
    ///         | "(" expr ")"
    fn parse_primary(&mut self) -> Primary {
        match &self.tokens[self.index].kind {
            TokenKind::Ident(ident) => {
                self.index += 1;
                Primary::Ident(ident.clone())
            }
            TokenKind::Num(num) => {
                self.index += 1;
                Primary::Num(*num)
            }
            TokenKind::String(b) => {
                self.index += 1;
                Primary::String(b.clone())
            }
            TokenKind::LeftParen => {
                self.index += 1;
                let expr = self.parse_expr();
                self.expect(&TokenKind::RightParen);
                Primary::Expr(Box::new(expr))
            }
            t => panic!("Unexpected token at index {}: {:?}", self.index, t),
        }
    }
}

macro_rules! parse_binop {
    ($doc: literal, $fn_name: ident, $inner: ident, $($tk: expr => $bok: expr,)+) => {
        impl Parser {
            #[doc=$doc]
            fn $fn_name(&mut self) -> BinOp {
                let mut node = self.$inner();

                loop {
                    $(if self.consume(&$tk) {
                        node = BinOp::Binary(
                            $bok,
                            Box::new(node),
                            Box::new(self.$inner()),
                        );
                        continue;
                    })+
                    return node;
                }
            }
        }
    };
}

parse_binop!(
    "logical-or = logical-and (\"||\" logical-and)*",
    parse_logical_or,
    parse_logical_and,
    TokenKind::PipePipe => BinOpKind::LogicalOr,
);

parse_binop!(
    "logical-and = bitwise-or (\"&&\" bitwise-or)*",
    parse_logical_and,
    parse_bitwise_or,
    TokenKind::AmpersandAmpersand => BinOpKind::LogicalAnd,
);

parse_binop!(
    "bitwise-or = bitwise-xor (\"|\" bitwise-xor)*",
    parse_bitwise_or,
    parse_bitwise_xor,
    TokenKind::Pipe => BinOpKind::BitwiseOr,
);

parse_binop!(
    "bitwise-xor = bitwise-and (\"^\" bitwise-and)*",
    parse_bitwise_xor,
    parse_bitwise_and,
    TokenKind::Hat => BinOpKind::BitwiseXor,
);

parse_binop!(
    "bitwise-and = equality (\"&\" equality)*",
    parse_bitwise_and,
    parse_equality,
    TokenKind::Ampersand => BinOpKind::BitwiseAnd,
);

parse_binop!(
    "equality = rel ((\"==\" | \"!=\" ) rel)*",
    parse_equality,
    parse_rel,
    TokenKind::DoubleEqual => BinOpKind::Equal,
    TokenKind::NotEqual => BinOpKind::NotEqual,
);

parse_binop!(
    "rel = shift ((\"<\" | \"<=\" | \">\" | \">=\" ) shift)*",
    parse_rel,
    parse_shift,
    TokenKind::LessThan => BinOpKind::LessThan,
    TokenKind::LessEqual => BinOpKind::LessEqual,
    TokenKind::GreaterThan => BinOpKind::GreaterThan,
    TokenKind::GreaterEqual => BinOpKind::GreaterEqual,
);

parse_binop!(
    "shift = add ((\"<<\" | \">>\") add)*",
    parse_shift,
    parse_add,
    TokenKind::LeftShift => BinOpKind::LeftShift,
    TokenKind::RightShift => BinOpKind::RightShift,
);

parse_binop!(
    "add = mul ((\"+\" | \"-\") mul)*",
    parse_add,
    parse_mul,
    TokenKind::Plus => BinOpKind::Add,
    TokenKind::Minus => BinOpKind::Sub,
);

parse_binop!(
    "mul = cast ((\"*\" | \"/\" | \"%\") cast)*",
    parse_mul,
    parse_cast_wrapped,
    TokenKind::Star => BinOpKind::Mul,
    TokenKind::Slash => BinOpKind::Div,
    TokenKind::Percent => BinOpKind::Mod,
);
