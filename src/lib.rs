pub mod analyzer;
pub mod codegen;
pub mod lexer;
pub mod parser;

use analyzer::SemanticVisitor;
use codegen::Codegen;
use lexer::Lexer;
use parser::Parser;

pub fn compile(user_input: &str) {
    let tokens = Lexer::tokenize(&user_input);

    let mut parser = Parser::new(tokens);
    let mut translation_unit = parser.parse();
    // dbg!(&translation_unit);

    let mut visitor = SemanticVisitor::new();
    let symbol_table = visitor.visit_translation_unit(&mut translation_unit);
    // dbg!(&translation_unit);
    dbg!(&symbol_table);

    let mut codegen = Codegen::new(symbol_table);
    codegen.generate(translation_unit);
}
