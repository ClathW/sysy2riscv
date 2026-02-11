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
    // let one = func_data.dfg_mut().new_value().integer(//TODO);
    // let ret = func_data.dfg_mut().new_value().ret(Some(one));
    // let _ = func_data
    //     .layout_mut()
    //     .bb_mut(entry)
    //     .insts_mut()
    //     .push_key_back(ret);
    let exp = &stmt.exp;
    let ret_val = gen_exp(func_data, exp);
    let ret = func_data.dfg_mut().new_value().ret(Some(ret_val));
    let _ = func_data
        .layout_mut()
        .bb_mut(entry)
        .insts_mut()
        .push_key_back(ret);
}

fn gen_exp(func_data: &mut FunctionData, exp: &Exp) -> Value {
    gen_add_exp(func_data, &exp.add_exp)
}

fn gen_add_exp(func_data: &mut FunctionData, add_exp: &AddExp) -> Value {
    match add_exp {
        AddExp::Mul(mul) => gen_mul_exp(func_data, mul),
        AddExp::Add { add, op, mul } => {
            let lhs = gen_add_exp(func_data, add);
            let rhs = gen_mul_exp(func_data, mul);
            let entry = func_data.layout().bbs().front_key().copied().unwrap();
            match op {
                AddOp::Minus => {
                    let result = func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Sub, lhs, rhs);
                    let _ = func_data
                        .layout_mut()
                        .bb_mut(entry)
                        .insts_mut()
                        .push_key_back(result);
                    result
                }
                AddOp::Plus => {
                    let result = func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Add, lhs, rhs);
                    let _ = func_data
                        .layout_mut()
                        .bb_mut(entry)
                        .insts_mut()
                        .push_key_back(result);
                    result
                }
            }
        }
    }
}

fn gen_mul_exp(func_data: &mut FunctionData, mul_exp: &MulExp) -> Value {
    match mul_exp {
        MulExp::Unary(unary) => gen_unary_exp(func_data, unary),
        MulExp::Mul { mul, op, unary } => {
            let lhs = gen_mul_exp(func_data, mul);
            let rhs = gen_unary_exp(func_data, unary);
            let entry = func_data.layout().bbs().front_key().copied().unwrap();
            match op {
                MulOp::Div => {
                    let result = func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Div, lhs, rhs);
                    let _ = func_data
                        .layout_mut()
                        .bb_mut(entry)
                        .insts_mut()
                        .push_key_back(result);
                    result
                }
                MulOp::Mod => {
                    let result = func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Mod, lhs, rhs);
                    let _ = func_data
                        .layout_mut()
                        .bb_mut(entry)
                        .insts_mut()
                        .push_key_back(result);
                    result
                }
                MulOp::Mul => {
                    let result = func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Mul, lhs, rhs);
                    let _ = func_data
                        .layout_mut()
                        .bb_mut(entry)
                        .insts_mut()
                        .push_key_back(result);
                    result
                }
            }
        }
    }
}

fn gen_unary_exp(func_data: &mut FunctionData, unaryexp: &UnaryExp) -> Value {
    match unaryexp {
        UnaryExp::PrimaryExp(pri_exp) => gen_prime_exp(func_data, pri_exp),
        UnaryExp::Unary { op, exp } => {
            let val = gen_unary_exp(func_data, exp);
            let entry = func_data.layout().bbs().front_key().copied().unwrap();

            match op {
                UnaryOp::Minus => {
                    let zero = func_data.dfg_mut().new_value().integer(0);
                    let result = func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Sub, zero, val);
                    let _ = func_data
                        .layout_mut()
                        .bb_mut(entry)
                        .insts_mut()
                        .push_key_back(result);
                    result
                }
                UnaryOp::Plus => val,
                UnaryOp::Not => {
                    let zero = func_data.dfg_mut().new_value().integer(0);
                    let result = func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Eq, zero, val);
                    let _ = func_data
                        .layout_mut()
                        .bb_mut(entry)
                        .insts_mut()
                        .push_key_back(result);
                    result
                }
            }
        }
    }
}

fn gen_prime_exp(func_data: &mut FunctionData, pri_exp: &PrimaryExp) -> Value {
    match pri_exp {
        PrimaryExp::Exp(exp) => gen_exp(func_data, exp),
        PrimaryExp::Number(num) => func_data.dfg_mut().new_value().integer(*num),
    }
}
