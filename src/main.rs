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
    let chars: Vec<_> = env[1].chars().collect();
    let mut p = 0;
    let n: String = chars.iter().take_while(|c| c.is_numeric()).collect();
    println!("mov rax, {}", n.parse::<i32>().unwrap());
    p += n.len();

    while p < chars.len() {
        let c = chars[p];
        match c {
            '+' => {
                let n: String = chars[p+1..].iter().take_while(|c| c.is_numeric()).collect();
                println!("add rax, {}", n.parse::<i32>().unwrap());
                p += n.len() + 1;
            }
            '-' => {
                let n: String = chars[p+1..].iter().take_while(|c| c.is_numeric()).collect();
                println!("sub rax, {}", n.parse::<i32>().unwrap());
                p += n.len() + 1;
            }
            _ => {
                eprintln!("unexpected character: {}", c);
                return;
            }
        }
    }

    println!("  ret");
}
