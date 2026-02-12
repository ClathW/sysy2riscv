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
    // gen_stmt(func_data, &block.stmt)
    for item in &block.block_items {
        match item {
            BlockItem::Stmt(stmt) => gen_stmt(func_data, stmt),
            BlockItem::Decl(decl) => gen_decl(func_data, decl),
        }
    }
}

fn gen_decl(_func_data: &mut FunctionData, _decl: &Decl) {
    unreachable!("常量声明应该在语义分析阶段被删除")
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
    gen_lor_exp(func_data, &exp.lor_exp)
}

fn gen_lor_exp(func_data: &mut FunctionData, lor_exp: &LOrExp) -> Value {
    match lor_exp {
        LOrExp::LAnd(land) => gen_land_exp(func_data, land),
        LOrExp::LOr { lor, land } => {
            let entry = func_data.layout().bbs().front_key().copied().unwrap();
            let lval = gen_lor_exp(func_data, lor);
            let zero = func_data.dfg_mut().new_value().integer(0);
            let lhs = func_data
                .dfg_mut()
                .new_value()
                .binary(BinaryOp::NotEq, lval, zero);
            let rval = gen_land_exp(func_data, land);
            let zero = func_data.dfg_mut().new_value().integer(0);
            let rhs = func_data
                .dfg_mut()
                .new_value()
                .binary(BinaryOp::NotEq, rval, zero);
            let _ = func_data
                .layout_mut()
                .bb_mut(entry)
                .insts_mut()
                .push_key_back(lhs);
            let _ = func_data
                .layout_mut()
                .bb_mut(entry)
                .insts_mut()
                .push_key_back(rhs);

            let result = func_data
                .dfg_mut()
                .new_value()
                .binary(BinaryOp::Or, lhs, rhs);
            let _ = func_data
                .layout_mut()
                .bb_mut(entry)
                .insts_mut()
                .push_key_back(result);
            result
        }
    }
}

fn gen_land_exp(func_data: &mut FunctionData, land_exp: &LAndExp) -> Value {
    match land_exp {
        LAndExp::Eq(eq) => gen_eq_exp(func_data, eq),
        LAndExp::LAnd { land, eq } => {
            let entry = func_data.layout().bbs().front_key().copied().unwrap();
            let lval = gen_land_exp(func_data, land);
            let zero = func_data.dfg_mut().new_value().integer(0);
            let lhs = func_data
                .dfg_mut()
                .new_value()
                .binary(BinaryOp::NotEq, lval, zero);
            let rval = gen_eq_exp(func_data, eq);
            let zero = func_data.dfg_mut().new_value().integer(0);
            let rhs = func_data
                .dfg_mut()
                .new_value()
                .binary(BinaryOp::NotEq, rval, zero);
            let _ = func_data
                .layout_mut()
                .bb_mut(entry)
                .insts_mut()
                .push_key_back(lhs);
            let _ = func_data
                .layout_mut()
                .bb_mut(entry)
                .insts_mut()
                .push_key_back(rhs);

            let result = func_data
                .dfg_mut()
                .new_value()
                .binary(BinaryOp::And, lhs, rhs);
            let _ = func_data
                .layout_mut()
                .bb_mut(entry)
                .insts_mut()
                .push_key_back(result);
            result
        }
    }
}

fn gen_eq_exp(func_data: &mut FunctionData, eq_exp: &EqExp) -> Value {
    match eq_exp {
        EqExp::Rel(rel) => gen_rel_exp(func_data, rel),
        EqExp::Eq { eq, op, rel } => {
            let lhs = gen_eq_exp(func_data, eq);
            let rhs = gen_rel_exp(func_data, rel);
            let entry = func_data.layout().bbs().front_key().copied().unwrap();
            match op {
                EqOp::Eq => {
                    let result = func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Eq, lhs, rhs);
                    let _ = func_data
                        .layout_mut()
                        .bb_mut(entry)
                        .insts_mut()
                        .push_key_back(result);
                    result
                }
                EqOp::Neq => {
                    let result = func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::NotEq, lhs, rhs);
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

fn gen_rel_exp(func_data: &mut FunctionData, rel_exp: &RelExp) -> Value {
    match rel_exp {
        RelExp::Add(add) => gen_add_exp(func_data, add),
        RelExp::Rel { rel, op, add } => {
            let lhs = gen_rel_exp(func_data, rel);
            let rhs = gen_add_exp(func_data, add);
            let entry = func_data.layout().bbs().front_key().copied().unwrap();
            match op {
                RelOp::Lt => {
                    let result = func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Lt, lhs, rhs);
                    let _ = func_data
                        .layout_mut()
                        .bb_mut(entry)
                        .insts_mut()
                        .push_key_back(result);
                    result
                }
                RelOp::Gt => {
                    let result = func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Gt, lhs, rhs);
                    let _ = func_data
                        .layout_mut()
                        .bb_mut(entry)
                        .insts_mut()
                        .push_key_back(result);
                    result
                }
                RelOp::Le => {
                    let result = func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Le, lhs, rhs);
                    let _ = func_data
                        .layout_mut()
                        .bb_mut(entry)
                        .insts_mut()
                        .push_key_back(result);
                    result
                }
                RelOp::Ge => {
                    let result = func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Ge, lhs, rhs);
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
        PrimaryExp::LVal(_lval) => {
            unreachable!("常量引用应该在语义分析阶段被替换为字面量")
        }
        PrimaryExp::Number(num) => func_data.dfg_mut().new_value().integer(*num),
    }
}
