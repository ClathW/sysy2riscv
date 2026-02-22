mod ast;
mod codegen;
mod irgen;
mod sem_analyze;

use crate::codegen::GenerateAsm;
use koopa::back::KoopaGenerator;
use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::fs::read_to_string;
use std::io::Error;
use std::io::{ErrorKind, Result};

lalrpop_mod!(sysy);

fn main() -> Result<()> {
    let args: Vec<String> = args().collect();
    if args.len() != 5 {
        return Err(Error::new(
            ErrorKind::InvalidInput,
            "Usage: compiler -mode input_file -o output_file",
        ));
    }
    let mode = &args[1];
    let input = &args[2];
    let output = &args[4];

    let input = read_to_string(input)?;

    let mut ast = sysy::CompUnitParser::new().parse(&input).unwrap();

    // 输出解析得到的 AST
    // println!("{:#?}", ast);

    crate::sem_analyze::constant_eval(&mut ast);

    // println!("The symbol map is {:?}", symbol_table);

    // println!("After constant evaluation:");
    // println!("{:#?}", ast);

    let program = crate::irgen::gen_program(&ast);

    match mode.as_str() {
        "-koopa" => {
            let mut g = KoopaGenerator::new(Vec::new());
            g.generate_on(&program).unwrap();
            let text_form_ir = std::str::from_utf8(&g.writer()).unwrap().to_string();
            println!("{}", text_form_ir);
            std::fs::write(output, text_form_ir)?;
            Ok(())
        }
        "-riscv" => {
            let asm = program.generate();
            println!("{}", asm);
            std::fs::write(output, asm).unwrap();
            Ok(())
        }
        _ => Err(Error::new(
            ErrorKind::InvalidInput,
            format!("unknown mode: {}", mode),
        )),
    }
}
