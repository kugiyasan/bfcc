use crate::lexer::{Token, TokenKind};

use super::{
    Assign, AssignOpKind, BinOpKind, CompoundStmt, ConstantExpr, Declaration, DeclarationOrStmt,
    DeclarationSpecifier, Declarator, DirectDeclarator, Expr, ExprKind, ExternalDeclaration,
    FuncDef, Identifier, InitDeclarator, ParamDeclaration, ParamTypeList, Pointer, Primary, Stmt,
    StorageClassSpecifier, TranslationUnit, TypeQualifier, TypeSpecifier, Unary,
};

pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, index: 0 }
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
        // todo: std::mem::discriminant
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
        // todo: std::mem::discriminant
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
        let declarator = self.parse_declarator();

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
            let inits = vec![InitDeclarator::Declarator(declarator)];
            // todo: replaced while with if, this disables recursive init declarator
            // if let Some(i) = self.parse_init_declarator() {
            //     inits.push(i);
            // }
            self.expect(&TokenKind::SemiColon);

            let d = Declaration { specs, inits };
            ExternalDeclaration::Declaration(d)
        }
    }

    /// stmt = ";"
    ///      | expr ";"
    ///      | "{" stmt* "}"
    ///      | "if" "(" expr ")" stmt ("else" stmt)?
    ///      | "while" "(" expr ")" stmt
    ///      | "for" "(" expr? ";" expr? ";" expr? ")" stmt
    ///      | "return" expr ";"
    fn parse_stmt(&mut self) -> Stmt {
        if self.consume(&TokenKind::SemiColon) {
            Stmt::SemiColon
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
        } else if self.consume(&TokenKind::While) {
            self.expect(&TokenKind::LeftParen);
            let expr = self.parse_expr();
            self.expect(&TokenKind::RightParen);
            let stmt = self.parse_stmt();
            Stmt::While(expr, Box::new(stmt))
        } else if self.consume(&TokenKind::For) {
            self.parse_for()
        } else if self.consume(&TokenKind::Return) {
            let expr = self.parse_expr();
            self.expect(&TokenKind::SemiColon);
            Stmt::Return(expr)
        } else {
            let expr = self.parse_expr();
            self.expect(&TokenKind::SemiColon);
            Stmt::Expr(expr)
        }
    }

    fn parse_declaration_or_stmt(&mut self) -> DeclarationOrStmt {
        if let Some(d) = self.parse_declaration() {
            DeclarationOrStmt::Declaration(d)
        } else {
            DeclarationOrStmt::Stmt(self.parse_stmt())
        }
    }

    /// declaration = declaration-specifiers init-declarator ("," init-declarator)* ";"
    fn parse_declaration(&mut self) -> Option<Declaration> {
        let specs = self.parse_declaration_specifiers();
        if specs.is_empty() {
            return None;
        }

        let mut inits = vec![];
        // todo: replaced while with if, this disables recursive init declarator
        if let Some(i) = self.parse_init_declarator() {
            inits.push(i);
        }

        self.expect(&TokenKind::SemiColon);
        Some(Declaration { specs, inits })
    }

    /// init-declarator = declarator
    ///                 | declarator "=" initializer
    fn parse_init_declarator(&mut self) -> Option<InitDeclarator> {
        let d = self.parse_declarator();
        if self.consume(&TokenKind::Equal) {
            todo!();
            // let i = self.parse_initializer();
            // return Some(InitDeclarator::DeclaratorAndInitializer(d, i));
        }
        Some(InitDeclarator::Declarator(d))
    }

    /// declarator = pointer? direct-declarator
    fn parse_declarator(&mut self) -> Declarator {
        Declarator {
            pointer: self.parse_pointer(),
            direct: self.parse_direct_declarator(),
        }
    }

    /// pointer = "*" type-qualifier* pointer?
    fn parse_pointer(&mut self) -> Option<Pointer> {
        if !self.consume(&TokenKind::Star) {
            return None;
        };

        let mut types = vec![];
        while let Some(t) = self.parse_type_qualifier() {
            types.push(t);
        }

        Some(Pointer {
            types,
            pointer: Box::new(self.parse_pointer()),
        })
    }

    /// direct-declarator = identifier
    ///                   | (declarator)
    ///                   | direct-declarator [ constant-expression? ]
    ///                   | direct-declarator ( parameter-type-list? )
    fn parse_direct_declarator(&mut self) -> DirectDeclarator {
        if self.consume(&TokenKind::LeftParen) {
            let d = self.parse_declarator();
            self.expect(&TokenKind::RightParen);
            return DirectDeclarator::Declarator(Box::new(d));
        }

        let name = self.expect_ident();
        let d = Box::new(DirectDeclarator::Ident(Identifier { name: name.clone() }));

        if self.consume(&TokenKind::LeftSquareBrace) {
            if self.consume(&TokenKind::RightSquareBrace) {
                return DirectDeclarator::Array(d, None);
            }

            let expr = Some(self.parse_constant_expr());
            self.expect(&TokenKind::RightSquareBrace);
            return DirectDeclarator::Array(d, expr);
        } else if self.consume(&TokenKind::LeftParen) {
            if self.consume(&TokenKind::RightParen) {
                let ptl = ParamTypeList {
                    params: vec![],
                    variadic: false,
                };
                return DirectDeclarator::ParamTypeList(d, ptl);
            }

            let param_list = self
                .parse_param_type_list()
                .expect("Expected ParamTypeList");
            self.expect(&TokenKind::RightParen);
            return DirectDeclarator::ParamTypeList(d, param_list);
        }

        DirectDeclarator::Ident(Identifier { name })
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

        let d = self.parse_declarator();
        Some(ParamDeclaration::Declarator(specs, Box::new(d)))
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
        } else if let Some(q) = self.parse_type_qualifier() {
            Some(DeclarationSpecifier::TypeQualifier(q))
        } else {
            None
        }
    }

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
        } else if self.consume(&TokenKind::Struct) {
            Some(TypeSpecifier::StructOrUnionSpecifier())
        } else if self.consume(&TokenKind::Enum) {
            Some(TypeSpecifier::EnumSpecifier())
        } else if self.consume(&TokenKind::Typedef) {
            Some(TypeSpecifier::TypedefName())
        } else {
            None
        }
    }

    fn parse_type_qualifier(&mut self) -> Option<TypeQualifier> {
        if self.consume(&TokenKind::Const) {
            Some(TypeQualifier::Const)
        } else if self.consume(&TokenKind::Volatile) {
            Some(TypeQualifier::Volatile)
        } else {
            None
        }
    }

    fn parse_for(&mut self) -> Stmt {
        self.expect(&TokenKind::LeftParen);
        let expr1 = if !self.consume(&TokenKind::SemiColon) {
            let expr = Some(self.parse_expr());
            self.expect(&TokenKind::SemiColon);
            expr
        } else {
            None
        };
        let expr2 = if !self.consume(&TokenKind::SemiColon) {
            let expr = Some(self.parse_expr());
            self.expect(&TokenKind::SemiColon);
            expr
        } else {
            None
        };
        let expr3 = if !self.consume(&TokenKind::RightParen) {
            let expr = Some(self.parse_expr());
            self.expect(&TokenKind::RightParen);
            expr
        } else {
            None
        };
        let stmt = self.parse_stmt();
        Stmt::For(expr1, expr2, expr3, Box::new(stmt))
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
            let ConstantExpr::Identity(ExprKind::Unary(unary)) = c else {
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

    fn parse_constant_expr(&mut self) -> ConstantExpr {
        let eq = self.parse_equality();

        if self.consume(&TokenKind::Question) {
            let expr = self.parse_expr();
            self.expect(&TokenKind::Colon);
            let constant_expr = self.parse_constant_expr();
            ConstantExpr::Ternary(eq, expr, Box::new(constant_expr))
        } else {
            ConstantExpr::Identity(eq)
        }
    }

    /// equality = relational
    ///          | "==" equality
    ///          | "!=" equality
    fn parse_equality(&mut self) -> ExprKind {
        let rel = self.parse_relational();

        if self.consume(&TokenKind::DoubleEqual) {
            ExprKind::Binary(
                BinOpKind::Equal,
                Box::new(rel),
                Box::new(self.parse_equality()),
            )
        } else if self.consume(&TokenKind::NotEqual) {
            ExprKind::Binary(
                BinOpKind::NotEqual,
                Box::new(rel),
                Box::new(self.parse_equality()),
            )
        } else {
            rel
        }
    }

    /// relational = add
    ///            | "<"  relational
    ///            | "<=" relational
    ///            | ">"  relational
    ///            | ">=" relational
    fn parse_relational(&mut self) -> ExprKind {
        let add = self.parse_add();

        if self.consume(&TokenKind::LessThan) {
            ExprKind::Binary(
                BinOpKind::LessThan,
                Box::new(add),
                Box::new(self.parse_relational()),
            )
        } else if self.consume(&TokenKind::LessEqual) {
            ExprKind::Binary(
                BinOpKind::LessEqual,
                Box::new(add),
                Box::new(self.parse_relational()),
            )
        } else if self.consume(&TokenKind::GreaterThan) {
            ExprKind::Binary(
                BinOpKind::GreaterThan,
                Box::new(add),
                Box::new(self.parse_relational()),
            )
        } else if self.consume(&TokenKind::GreaterEqual) {
            ExprKind::Binary(
                BinOpKind::GreaterEqual,
                Box::new(add),
                Box::new(self.parse_relational()),
            )
        } else {
            add
        }
    }

    /// add = mul
    ///     | add "+" mul
    ///     | add "-" mul
    fn parse_add(&mut self) -> ExprKind {
        let mul = self.parse_mul();

        if self.consume(&TokenKind::Plus) {
            ExprKind::Binary(BinOpKind::Add, Box::new(mul), Box::new(self.parse_add()))
        } else if self.consume(&TokenKind::Minus) {
            ExprKind::Binary(BinOpKind::Sub, Box::new(mul), Box::new(self.parse_add()))
        } else {
            mul
        }
    }

    /// mul = unary
    ///     | mul "*" unary
    ///     | mul "/" unary
    ///     | mul "%" unary
    fn parse_mul(&mut self) -> ExprKind {
        let unary = self.parse_cast();

        if self.consume(&TokenKind::Star) {
            ExprKind::Binary(
                BinOpKind::Mul,
                Box::new(ExprKind::Unary(unary)),
                Box::new(self.parse_mul()),
            )
        } else if self.consume(&TokenKind::Slash) {
            ExprKind::Binary(
                BinOpKind::Div,
                Box::new(ExprKind::Unary(unary)),
                Box::new(self.parse_mul()),
            )
        } else {
            ExprKind::Unary(unary)
        }
    }

    /// cast = unary
    ///      | "(" typename ")" cast
    fn parse_cast(&mut self) -> Unary {
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
            Unary::Sizeof(Box::new(self.parse_unary()))
        } else {
            self.parse_postfix_expr()
        }
    }

    fn parse_postfix_expr(&mut self) -> Unary {
        let unary = Unary::Identity(self.parse_primary());

        if self.consume(&TokenKind::LeftSquareBrace) {
            let expr = self.parse_expr();
            self.expect(&TokenKind::RightSquareBrace);
            Unary::Index(Box::new(unary), expr)
        } else if self.consume(&TokenKind::LeftParen) {
            if self.consume(&TokenKind::RightParen) {
                Unary::Call(Box::new(unary), None)
            } else {
                let expr = self.parse_expr();
                self.expect(&TokenKind::RightParen);
                Unary::Call(Box::new(unary), Some(expr))
            }
        } else if self.consume(&TokenKind::Dot) {
            let name = self
                .consume_ident()
                .expect("Expected identifier after field access");
            Unary::Field(Box::new(unary), Identifier { name })
        } else if self.consume(&TokenKind::Arrow) {
            let name = self
                .consume_ident()
                .expect("Expected identifier after field access");
            Unary::PointerField(Box::new(unary), Identifier { name })
        } else if self.consume(&TokenKind::PlusPlus) {
            Unary::PostfixIncrement(Box::new(unary))
        } else if self.consume(&TokenKind::MinusMinus) {
            Unary::PostfixDecrement(Box::new(unary))
        } else {
            unary
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
                Primary::Ident(Identifier {
                    name: ident.clone(),
                })
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

            t => panic!("Unexpected token: {:?}", t),
        }
    }
}
