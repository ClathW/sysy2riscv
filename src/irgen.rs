use std::collections::HashMap;

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
    let mut map = HashMap::new();

    gen_block(func_data, &func.block, &mut map);
}

fn gen_block(func_data: &mut FunctionData, block: &Block, map: &mut HashMap<String, Value>) {
    let entry = func_data
        .dfg_mut()
        .new_bb()
        .basic_block(Some("%entry".into()));

    func_data.layout_mut().bbs_mut().extend([entry]);
    for item in &block.block_items {
        match item {
            BlockItem::Stmt(stmt) => gen_stmt(func_data, stmt, map),
            BlockItem::Decl(decl) => gen_decl(func_data, decl, map),
        }
    }
}

fn gen_decl(func_data: &mut FunctionData, decl: &Decl, map: &mut HashMap<String, Value>) {
    match decl {
        Decl::Const(..) => {
            unreachable!("常量声明应该在语义分析阶段被删除")
        }
        Decl::Var(var_decl) => {
            gen_var_decl(func_data, var_decl, map);
        }
    }
}

fn gen_var_decl(
    func_data: &mut FunctionData,
    var_decl: &VarDecl,
    map: &mut HashMap<String, Value>,
) {
    for var_def in &var_decl.vardefs {
        gen_var_def(func_data, var_def, map);
    }
}

fn gen_var_def(func_data: &mut FunctionData, var_def: &VarDef, map: &mut HashMap<String, Value>) {
    let entry = func_data.layout().bbs().front_key().copied().unwrap();
    let alloc = func_data.dfg_mut().new_value().alloc(Type::get_i32());
    match var_def {
        VarDef::Ident(ident) => {
            map.insert(ident.clone(), alloc);
            let _ = func_data
                .layout_mut()
                .bb_mut(entry)
                .insts_mut()
                .push_key_back(alloc);
        }
        VarDef::Assign { ident, val } => {
            map.insert(ident.clone(), alloc);
            let init_val = gen_init_val(func_data, val, map);
            let store = func_data.dfg_mut().new_value().store(init_val, alloc);
            func_data
                .layout_mut()
                .bb_mut(entry)
                .insts_mut()
                .extend([alloc, store]);
        }
    }
}

fn gen_init_val(
    func_data: &mut FunctionData,
    init_val: &InitVal,
    map: &mut HashMap<String, Value>,
) -> Value {
    gen_exp(func_data, &init_val.exp, map)
}

fn gen_stmt(func_data: &mut FunctionData, stmt: &Stmt, map: &mut HashMap<String, Value>) {
    let entry = func_data.layout().bbs().front_key().copied().unwrap();

    match stmt {
        Stmt::Assign { lval, exp } => {
            let val = gen_exp(func_data, exp, map);
            if let Some(alloc) = map.get(&lval.ident) {
                let store = func_data.dfg_mut().new_value().store(val, *alloc);
                let _ = func_data
                    .layout_mut()
                    .bb_mut(entry)
                    .insts_mut()
                    .push_key_back(store);
            } else {
                panic!("Identifier {} not found", lval.ident);
            }
        }
        Stmt::Block(block) => {
            for item in &block.block_items {
                match item {
                    BlockItem::Stmt(stmt) => gen_stmt(func_data, stmt, map),
                    BlockItem::Decl(decl) => gen_decl(func_data, decl, map),
                }
            }
        }
        Stmt::Exp(Some(exp)) => {
            let _ = gen_exp(func_data, exp, map);
        }
        Stmt::Exp(None) => {}
        Stmt::Ret(exp) => {
            let ret = match exp {
                Some(exp) => {
                    let val = gen_exp(func_data, exp, map);
                    func_data.dfg_mut().new_value().ret(Some(val))
                }
                None => func_data.dfg_mut().new_value().ret(None),
            };
            let _ = func_data
                .layout_mut()
                .bb_mut(entry)
                .insts_mut()
                .push_key_back(ret);
        }
    }
}

fn gen_exp(func_data: &mut FunctionData, exp: &Exp, map: &mut HashMap<String, Value>) -> Value {
    gen_lor_exp(func_data, &exp.lor_exp, map)
}

fn gen_lor_exp(
    func_data: &mut FunctionData,
    lor_exp: &LOrExp,
    map: &mut HashMap<String, Value>,
) -> Value {
    match lor_exp {
        LOrExp::LAnd(land) => gen_land_exp(func_data, land, map),
        LOrExp::LOr { lor, land } => {
            let entry = func_data.layout().bbs().front_key().copied().unwrap();
            let lval = gen_lor_exp(func_data, lor, map);
            let zero = func_data.dfg_mut().new_value().integer(0);
            let lhs = func_data
                .dfg_mut()
                .new_value()
                .binary(BinaryOp::NotEq, lval, zero);
            let rval = gen_land_exp(func_data, land, map);
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

fn gen_land_exp(
    func_data: &mut FunctionData,
    land_exp: &LAndExp,
    map: &mut HashMap<String, Value>,
) -> Value {
    match land_exp {
        LAndExp::Eq(eq) => gen_eq_exp(func_data, eq, map),
        LAndExp::LAnd { land, eq } => {
            let entry = func_data.layout().bbs().front_key().copied().unwrap();
            let lval = gen_land_exp(func_data, land, map);
            let zero = func_data.dfg_mut().new_value().integer(0);
            let lhs = func_data
                .dfg_mut()
                .new_value()
                .binary(BinaryOp::NotEq, lval, zero);
            let rval = gen_eq_exp(func_data, eq, map);
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

fn gen_eq_exp(
    func_data: &mut FunctionData,
    eq_exp: &EqExp,
    map: &mut HashMap<String, Value>,
) -> Value {
    match eq_exp {
        EqExp::Rel(rel) => gen_rel_exp(func_data, rel, map),
        EqExp::Eq { eq, op, rel } => {
            let lhs = gen_eq_exp(func_data, eq, map);
            let rhs = gen_rel_exp(func_data, rel, map);
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

fn gen_rel_exp(
    func_data: &mut FunctionData,
    rel_exp: &RelExp,
    map: &mut HashMap<String, Value>,
) -> Value {
    match rel_exp {
        RelExp::Add(add) => gen_add_exp(func_data, add, map),
        RelExp::Rel { rel, op, add } => {
            let lhs = gen_rel_exp(func_data, rel, map);
            let rhs = gen_add_exp(func_data, add, map);
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

fn gen_add_exp(
    func_data: &mut FunctionData,
    add_exp: &AddExp,
    map: &mut HashMap<String, Value>,
) -> Value {
    match add_exp {
        AddExp::Mul(mul) => gen_mul_exp(func_data, mul, map),
        AddExp::Add { add, op, mul } => {
            let lhs = gen_add_exp(func_data, add, map);
            let rhs = gen_mul_exp(func_data, mul, map);
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

fn gen_mul_exp(
    func_data: &mut FunctionData,
    mul_exp: &MulExp,
    map: &mut HashMap<String, Value>,
) -> Value {
    match mul_exp {
        MulExp::Unary(unary) => gen_unary_exp(func_data, unary, map),
        MulExp::Mul { mul, op, unary } => {
            let lhs = gen_mul_exp(func_data, mul, map);
            let rhs = gen_unary_exp(func_data, unary, map);
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

fn gen_unary_exp(
    func_data: &mut FunctionData,
    unaryexp: &UnaryExp,
    map: &mut HashMap<String, Value>,
) -> Value {
    match unaryexp {
        UnaryExp::PrimaryExp(pri_exp) => gen_prime_exp(func_data, pri_exp, map),
        UnaryExp::Unary { op, exp } => {
            let val = gen_unary_exp(func_data, exp, map);
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

fn gen_prime_exp(
    func_data: &mut FunctionData,
    pri_exp: &PrimaryExp,
    map: &mut HashMap<String, Value>,
) -> Value {
    match pri_exp {
        PrimaryExp::Exp(exp) => gen_exp(func_data, exp, map),
        PrimaryExp::LVal(lval) => {
            if let Some(alloc) = map.get(&lval.ident) {
                let load = func_data.dfg_mut().new_value().load(*alloc);
                let entry = func_data.layout().bbs().front_key().copied().unwrap();
                let _ = func_data
                    .layout_mut()
                    .bb_mut(entry)
                    .insts_mut()
                    .push_key_back(load);
                load
            } else {
                panic!("Identifier {} not found", lval.ident);
            }
        }
        PrimaryExp::Number(num) => func_data.dfg_mut().new_value().integer(*num),
    }
}
