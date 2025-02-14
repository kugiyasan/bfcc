use bfcc::analyzer::{SemanticVisitor, SymbolTable, Ty};
use bfcc::lexer::Lexer;
use bfcc::parser::*;

fn wrap_in_main(input: &str) -> String {
    format!("int main() {{{}}}", input)
}

fn generate_ast(input: &str) -> TranslationUnit {
    let tokens = Lexer::tokenize(input);
    let mut parser = Parser::new(tokens);
    let mut translation_unit = parser.parse();
    let typedefs = parser.get_typedefs();
    let mut visitor = SemanticVisitor::new(typedefs.clone());
    let _symbol_table = visitor.visit_translation_unit(&mut translation_unit);
    translation_unit
}

fn extract_node(ast: &TranslationUnit) -> &BinOp {
    let ExternalDeclaration::FuncDef(func_def) = &ast.0[0] else {
        panic!();
    };
    let DeclarationOrStmt::Stmt(Stmt::Expr(expr)) = &func_def.stmt.0[2] else {
        panic!();
    };
    let Assign::Assign(_, AssignOpKind::Assign, assign) = &expr.0[0] else {
        panic!();
    };
    let Assign::Const(ConstantExpr::Identity(expr_kind)) = &**assign else {
        panic!();
    };
    &expr_kind
}

fn extract_binop(binop: &BinOp) -> &BinOp {
    let BinOp::Unary(Unary::Ref(r)) = binop else {
        panic!();
    };
    let Unary::Deref(ref d) = **r else { panic!() };
    let Unary::Identity(Primary::Expr(ref e)) = **d else {
        panic!();
    };
    let Assign::Const(ConstantExpr::Identity(ref b)) = e.0[0] else {
        panic!();
    };

    &b
}

#[test]
fn compare_ast() {
    let input1 = "
int arr[2];
int *p;
p = &arr[1];
return 0;
";
    let input2 = "
int arr[2];
int *p;
p = arr + 1;
return 0;
";

    let ast1 = generate_ast(&wrap_in_main(input1));
    let ast2 = generate_ast(&wrap_in_main(input2));
    let BinOp::Binary(k1, l1, r1) = extract_binop(extract_node(&ast1)) else {
        panic!();
    };
    let BinOp::Binary(k2, l2, r2) = extract_node(&ast2) else {
        panic!();
    };
    assert_eq!(k1, k2);
    assert_eq!(l1, l2);
    let typedefs = Typedefs::new();
    let mut symbol_table = SymbolTable::new(typedefs);
    assert_eq!(
        r1.constant_fold(&mut symbol_table),
        r2.constant_fold(&mut symbol_table)
    );
}

#[test]
fn ty_to_typename() {
    let expected_ty = Ty::Ptr(Box::new(Ty::Array(Box::new(Ty::I8), Some(0))));

    let ce = ConstantExpr::Identity(BinOp::Unary(Unary::Identity(Primary::Num(0))));
    let expected_tn = TypeName {
        specs: vec![SpecifierQualifier::TypeSpecifier(TypeSpecifier::Char)],
        declarator: Some(AbstractDeclarator::DirectAbstractDeclarator(
            Some(Pointer {
                qualifiers: vec![],
                pointer: Box::new(None),
            }),
            DirectAbstractDeclarator::Array(None, Some(ce)),
        )),
    };

    let actual_tn = expected_ty.to_typename();
    assert_eq!(actual_tn, expected_tn);

    let typedefs = Typedefs::new();
    let mut symbol_table = SymbolTable::new(typedefs);
    let actual_ty = symbol_table
        .from_specs_and_abstract_declarator(&expected_tn.specs, &expected_tn.declarator);
    assert_eq!(actual_ty, expected_ty);
}
