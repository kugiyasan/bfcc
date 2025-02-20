use clap::Parser;
use clap_stdin::FileOrStdin;
use std::{
    fs::File,
    io::{self, Write},
    path::PathBuf,
    process::{Command, Stdio},
};

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    input_file: FileOrStdin,

    #[arg(short, long)]
    print_preprocessed: bool,

    #[arg(short, long, value_name = "FILE")]
    output_file: Option<PathBuf>,
}

fn run_preprocessor(input_file_content: &[u8]) -> Result<String, Box<dyn std::error::Error>> {
    let args = [
        "-E",
        "-P",
        "-D",
        "__builtin_va_list=char *",
        "-D",
        "__attribute__(x)=",
        "-D",
        "__restrict=",
        "-D",
        "__asm__(x)=",
        "-D",
        "__extension__=",
        "-D",
        "__inline=",
        "-D",
        "__builtin_bswap16=__bswap_constant_16",
        "-D",
        "__builtin_bswap32=__bswap_constant_32",
        "-D",
        "__builtin_bswap64=__bswap_constant_64",
        "-",
    ];
    let mut gcc = Command::new("gcc")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .args(args)
        .spawn()?;

    let mut stdin = gcc.stdin.take().unwrap();
    stdin.write_all(input_file_content)?;
    drop(stdin);

    let output = gcc.wait_with_output()?;
    if !output.status.success() {
        return Err(output.status.to_string().into());
    }
    let preprocessed_content = String::from_utf8(output.stdout)?;
    Ok(preprocessed_content)
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();
    let input_file_content = cli.input_file.contents()?;

    let preprocessed_content = run_preprocessor(input_file_content.as_bytes())?;

    if cli.print_preprocessed {
        println!("{}", preprocessed_content);
        return Ok(());
    }

    let output: Box<dyn Write> = if let Some(f) = cli.output_file {
        Box::new(File::create(f)?)
    } else {
        Box::new(io::stdout())
    };
    bfcc::compile(&preprocessed_content, output);
    Ok(())
}
