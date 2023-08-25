// https://www.sigbus.info/compilerbook

use std::env;

fn main() {
    let env: Vec<String> = env::args_os().map(|s| s.into_string().unwrap()).collect();
    if env.len() != 2 {
        eprintln!("Wrong number of arguments!");
        return;
    }

    println!(".intel_syntax noprefix");
    println!(".globl main");
    println!("main:");
    println!("  mov rax, {}", env[1].parse::<u8>().unwrap());
    println!("  ret");
}
