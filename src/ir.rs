use koopa::back::KoopaGenerator;
use koopa::ir::builder::LocalBuilder;
use koopa::ir::{builder_traits::*, *};

use crate::ast::*;

pub fn gen_program(ast: &CompUnit) -> Program {
    let mut program = Program::new();
    gen_func(&mut program, &ast.func_def);
    program
}

fn gen_func(program: &mut Program, func: &FuncDef) {
    // let functype = ast.func_type;
    // let type = match functype {
    //     FuncType::Int => Type::get_i32(),
    // }
    let funcname = func.ident.clone();
    if &funcname != "main" {
        panic!("function name: {funcname}, is not main");
    }
    let main = program.new_func(FunctionData::new(
        "@main".into(),
        Vec::new(),
        Type::get_i32(),
    ));

    let func_data = program.func_mut(main);

    gen_block(func_data, &func.block);
}

fn gen_block(func_data: &mut FunctionData, block: &Block) {
    gen_stmt(func_data, &block.stmt)
}

fn gen_stmt(func_data: &mut FunctionData, stmt: &Stmt) {
    let entry = func_data
        .dfg_mut()
        .new_bb()
        .basic_block(Some("%entry".into()));

    func_data.layout_mut().bbs_mut().extend([entry]);
    let one = func_data.dfg_mut().new_value().integer(stmt.num);
    let ret = func_data.dfg_mut().new_value().ret(Some(one));
    func_data
        .layout_mut()
        .bb_mut(entry)
        .insts_mut()
        .push_key_back(ret);
}
