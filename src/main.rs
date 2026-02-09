mod ast;
mod codegen;
mod ir;

use crate::codegen::GenerateAsm;
use koopa::back::KoopaGenerator;
use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::fs::read_to_string;
use std::io::Error;
use std::io::{ErrorKind, Result};

// 引用 lalrpop 生成的解析器
// 因为我们刚刚创建了 sysy.lalrpop, 所以模块名是 sysy
lalrpop_mod!(sysy);

fn main() -> Result<()> {
    // 解析命令行参数
    let mut args = args();
    args.next();
    let mode = args.next().unwrap();
    let input = args.next().unwrap();
    args.next();
    let output = args.next().unwrap();

    // 读取输入文件
    let input = read_to_string(input)?;

    let ast = sysy::CompUnitParser::new().parse(&input).unwrap();

    // 输出解析得到的 AST
    println!("{:#?}", ast);

    let program = crate::ir::gen_program(&ast);

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
