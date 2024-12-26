use crate::lexer::{Token, TokenKind};

use super::{
    local_variables::LocalVariables, Assign, AssignOpKind, BinOpKind, CompoundStmt, ConstantExpr,
    Declaration, DeclarationOrStmt, DeclarationSpecifier, Declarator, DirectDeclarator, Expr,
    ExprKind, FuncDef, Identifier, InitDeclarator, ParamDeclaration, ParamList, Pointer, Primary,
    Stmt, StorageClassSpecifier, TranslationUnit, TypeQualifier, TypeSpecifier, Unary,
};

#[derive(Debug)]
pub struct Parser {
    tokens: Vec<Token>,
    index: usize,
    locals: LocalVariables,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            index: 0,
            locals: LocalVariables::new(),
        }
    }

    pub fn parse(&mut self) -> TranslationUnit {
        self.parse_program()
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
        return true;
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

    /// program = func*
    fn parse_program(&mut self) -> TranslationUnit {
        let mut funcs = vec![];

        while self.index < self.tokens.len() {
            funcs.push(self.parse_func());
        }

        TranslationUnit(funcs)
    }

    /// func = "int" name "(" args ")" "{" stmt* "}"
    fn parse_func(&mut self) -> FuncDef {
        self.locals.reset();
        self.expect(&TokenKind::Int);

        let t = self.tokens[self.index].clone();
        let TokenKind::Ident(name) = t.kind else {
            panic!("Expected function definition, got {:?}", t.kind);
        };

        self.index += 1;
        let args = self.parse_args();

        let mut stmts = vec![];
        self.expect(&TokenKind::OpenCurlyBrace);
        while !self.consume(&TokenKind::CloseCurlyBrace) {
            stmts.push(self.parse_declaration_or_stmt());
        }

        FuncDef {
            name,
            args,
            stmt: CompoundStmt(stmts),
            local_offset: self.locals.get_last_offset(),
        }
    }

    /// args = ("int" ident ("," "int" ident)*)?
    fn parse_args(&mut self) -> Vec<usize> {
        let mut args = vec![];
        self.expect(&TokenKind::OpenParen);

        if !self.consume(&TokenKind::CloseParen) {
            self.expect(&TokenKind::Int);
            let ident = self.expect_ident();
            let offset = self.locals.get_lvar_offset(&ident);
            args.push(offset);
            while self.consume(&TokenKind::Comma) {
                self.expect(&TokenKind::Int);
                let ident = self.expect_ident();
                let offset = self.locals.get_lvar_offset(&ident);
                args.push(offset);
            }
            self.expect(&TokenKind::CloseParen);
        }

        args
    }

    /// stmt = expr ";"
    ///      | "{" stmt* "}"
    ///      | "if" "(" expr ")" stmt ("else" stmt)?
    ///      | "while" "(" expr ")" stmt
    ///      | "for" "(" expr? ";" expr? ";" expr? ")" stmt
    ///      | "return" expr ";"
    fn parse_stmt(&mut self) -> Stmt {
        if self.consume(&TokenKind::OpenCurlyBrace) {
            let mut stmts = vec![];
            while !self.consume(&TokenKind::CloseCurlyBrace) {
                stmts.push(self.parse_declaration_or_stmt());
            }
            Stmt::Compound(CompoundStmt(stmts))
        } else if self.consume(&TokenKind::If) {
            self.expect(&TokenKind::OpenParen);
            let expr = self.parse_expr();
            self.expect(&TokenKind::CloseParen);
            let stmt = self.parse_stmt();
            let else_stmt = if self.consume(&TokenKind::Else) {
                Some(Box::new(self.parse_stmt()))
            } else {
                None
            };
            Stmt::If(expr, Box::new(stmt), else_stmt)
        } else if self.consume(&TokenKind::While) {
            self.expect(&TokenKind::OpenParen);
            let expr = self.parse_expr();
            self.expect(&TokenKind::CloseParen);
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

    fn parse_declaration(&mut self) -> Option<Declaration> {
        let Some(ds) = self.parse_declaration_specifier() else {
            return None;
        };
        let mut specs = vec![ds];
        while let Some(ds) = self.parse_declaration_specifier() {
            specs.push(ds);
        }

        let mut inits = vec![];
        // todo: replaced while with if, this disables recursive init declarator
        if let Some(i) = self.parse_init_declarator() {
            inits.push(i);
        }

        self.expect(&TokenKind::SemiColon);
        Some(Declaration { specs, inits })
    }

    fn parse_init_declarator(&mut self) -> Option<InitDeclarator> {
        Some(InitDeclarator::Declarator(self.parse_declarator()))
    }

    fn parse_declarator(&mut self) -> Declarator {
        Declarator {
            pointer: self.parse_pointer(),
            direct: self.parse_direct_declarator(),
        }
    }

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

    fn parse_direct_declarator(&mut self) -> DirectDeclarator {
        if let Some(name) = self.consume_ident() {
            DirectDeclarator::Ident(Identifier {
                offset: self.locals.get_lvar_offset(&name),
            })
        } else if self.consume(&TokenKind::OpenParen) {
            let d = self.parse_declarator();
            self.expect(&TokenKind::CloseParen);
            DirectDeclarator::Declarator(Box::new(d))
        } else {
            let d = Box::new(self.parse_direct_declarator());
            if self.consume(&TokenKind::OpenSquareBrace) {
                let expr = Some(self.parse_expr());
                self.expect(&TokenKind::CloseSquareBrace);
                return DirectDeclarator::Array(d, expr);
            }
            self.expect(&TokenKind::OpenParen);
            if let Some(param_list) = self.parse_param_list() {
                self.expect(&TokenKind::CloseParen);
                return DirectDeclarator::ParamList(d, param_list);
            }
            let mut identifiers = vec![];
            while let Some(name) = self.consume_ident() {
                let offset = self.locals.get_lvar_offset(&name);
                identifiers.push(Identifier { offset });
            }
            self.expect(&TokenKind::CloseParen);
            DirectDeclarator::Identifiers(d, identifiers)
        }
    }

    fn parse_param_list(&mut self) -> Option<ParamList> {
        let Some(pd) = self.parse_param_declaration() else {
            return None;
        };

        let mut pds = vec![pd];
        while self.consume(&TokenKind::Comma) {
            if let Some(pd) = self.parse_param_declaration() {
                pds.push(pd);
            };
            if self.consume(&TokenKind::ThreeDots) {
                return Some(ParamList {
                    params: pds,
                    variadic: true,
                });
            }
        }

        Some(ParamList {
            params: pds,
            variadic: false,
        })
    }

    fn parse_param_declaration(&mut self) -> Option<ParamDeclaration> {
        let Some(ds) = self.parse_declaration_specifier() else {
            return None;
        };

        let mut specs = vec![ds];
        while let Some(ds) = self.parse_declaration_specifier() {
            specs.push(ds);
        }

        Some(ParamDeclaration::Identity(specs))
        // if let Some(d) = self.parse_declarator() {
        //     Some(ParamDeclaration::Declarator(specs, Box::new(d)))
        // } else {
        //     Some(ParamDeclaration::Identity(specs))
        // }
    }

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
        self.expect(&TokenKind::OpenParen);
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
        let expr3 = if !self.consume(&TokenKind::CloseParen) {
            let expr = Some(self.parse_expr());
            self.expect(&TokenKind::CloseParen);
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
    ///        | unary "="   assign
    ///        | unary "*="  assign
    ///        | unary "/="  assign
    ///        | unary "%="  assign
    ///        | unary "+="  assign
    ///        | unary "-="  assign
    ///        | unary "<<=" assign
    ///        | unary ">>=" assign
    ///        | unary "&="  assign
    ///        | unary "^="  assign
    ///        | unary "|="  assign
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
        } else if self.consume(&TokenKind::LeftShiftAssign) {
            Some(AssignOpKind::LeftShiftAssign)
        } else if self.consume(&TokenKind::RightShiftAssign) {
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

    /// unary = primary
    ///       | "+" unary
    ///       | "-" unary
    ///       | "*" unary
    ///       | "&" unary
    ///       | "~" unary
    ///       | "!" unary
    fn parse_unary(&mut self) -> Unary {
        if self.consume(&TokenKind::Plus) {
            self.parse_unary()
        } else if self.consume(&TokenKind::Minus) {
            Unary::Neg(Box::new(self.parse_unary()))
        } else if self.consume(&TokenKind::Ampersand) {
            Unary::Ref(Box::new(self.parse_unary()))
        } else if self.consume(&TokenKind::Star) {
            Unary::Deref(Box::new(self.parse_unary()))
        } else if self.consume(&TokenKind::Tilde) {
            Unary::BitwiseNot(Box::new(self.parse_unary()))
        } else if self.consume(&TokenKind::Not) {
            Unary::LogicalNot(Box::new(self.parse_unary()))
        } else {
            Unary::Identity(self.parse_primary())
        }
    }

    /// primary = num
    ///         | ident ("(" (expr (, expr)*)? ")")?
    ///         | "(" expr ")"
    fn parse_primary(&mut self) -> Primary {
        match &self.tokens[self.index].kind {
            TokenKind::OpenParen => {
                self.index += 1;
                let expr = self.parse_expr();
                self.expect(&TokenKind::CloseParen);
                Primary::Expr(Box::new(expr))
            }
            TokenKind::Num(num) => {
                self.index += 1;
                Primary::Num(*num)
            }
            TokenKind::Ident(ident) => {
                self.index += 1;
                self.parse_ident(ident.clone())
            }

            t => panic!("Unexpected token: {:?}", t),
        }
    }

    fn parse_ident(&mut self, name: String) -> Primary {
        if self.consume(&TokenKind::OpenParen) {
            if self.consume(&TokenKind::CloseParen) {
                Primary::FunctionCall(name, None)
            } else {
                let expr = self.parse_expr();
                self.expect(&TokenKind::CloseParen);
                Primary::FunctionCall(name, Some(expr))
            }
        } else {
            let offset = self.locals.get_lvar_offset(&name);
            Primary::Ident(Identifier { offset })
        }
    }
}
