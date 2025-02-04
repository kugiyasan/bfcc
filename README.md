# BFCC
BFCC (Blazingly Fast C Compiler) is a C compiler written in Rust.
Despite the name, BFCC is focused on implementing the majority of C99 features
and haven't done anything special to improve performance. :clown_face:

This project started by following
[Rui Ueyama's book about building a C compiler](https://www.sigbus.info/compilerbook),
which is a great starting point. The book starts with a compiler that can compile a number into a program that returns that number, and gradually adds features.
[By the end of the book](https://github.com/kugiyasan/bfcc/tree/f563b330240d39ce8a50c109d6926d4cfa88b483),
BFCC supported local and global variables, pointers, arrays, function declarations, types and string literals.

## Features
- The compilation happens in 4 big steps: Lexer, Parser, SemanticVisitor and Codegen (see [lib.rs](./src/lib.rs)).
- The compiler emits intel-syntax assembly that emulates a stack machine, which allows to directly go from the AST down to assembly without passing by a SSA form.
- BFCC is able to directly use GCC stdlib by using
[a few macro definitions tricks](https://github.com/kugiyasan/bfcc/blob/main/src/main.rs#L23)
to remove implementation-specific code during preprocessing.
- The preprocessing is currently done by GCC.

## Usage
```sh
git clone https://github.com/kugiyasan/bfcc
cd bfcc
cargo build

# Run tests
./tests/test.sh

# Run a C file
./target/debug/bfcc main.c -o a.s
gcc -o a.out a.s
./a.out

# Compile directly from stdin
printf 'int main() { return 0; }' | bfcc - 2>/dev/null | gcc -x assembler -o a.out - 
```

## References
- Rui Ueyama's book:
https://www.sigbus.info/compilerbook

- C-testsuite (A testsuite against which BFCC is tested, BFCC passes 122 out of the 220 tests):
https://github.com/c-testsuite/c-testsuite

- ANSI C grammar in BNF:
https://cs.wmich.edu/~gupta/teaching/cs4850/sumII06/The%20syntax%20of%20C%20in%20Backus-Naur%20form.htm

- C99 standard:
https://www.open-std.org/jtc1/sc22/wg14/www/docs/n1256.pdf

- compiler-dev-tutorial:
https://radiilab.github.io/compiler-dev-tutorial/
