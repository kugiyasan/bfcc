pub mod analyzer;
pub mod codegen;
pub mod lexer;
pub mod parser;

use std::io::Write;

use analyzer::SemanticVisitor;
use codegen::Codegen;
use lexer::Lexer;
use parser::Parser;

pub fn compile(user_input: &str, output: Box<dyn Write>) {
    let tokens = Lexer::tokenize(user_input);
    // let token_kinds = tokens.iter().map(|t| &t.kind).collect::<Vec<_>>();
    // dbg!(token_kinds);

    let mut parser = Parser::new(tokens);
    let mut translation_unit = parser.parse();
    let typedefs = parser.get_typedefs();
    // dbg!(&translation_unit);

    let mut visitor = SemanticVisitor::new(typedefs.clone());
    let symbol_table = visitor.visit_translation_unit(&mut translation_unit);
    // dbg!(&translation_unit);
    // dbg!(&symbol_table);

    let mut codegen = Codegen::new(output, symbol_table);
    codegen.generate(translation_unit);
}
