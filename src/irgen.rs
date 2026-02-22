use std::collections::HashMap;

use koopa::ir::{builder_traits::*, *};

use crate::ast::*;

pub fn gen_program(ast: &CompUnit) -> Program {
    let mut program = Program::new();
    gen_func(&mut program, &ast.func_def);
    program
}

fn gen_func(program: &mut Program, func: &FuncDef) {
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
    let entry_bb = func_data
        .dfg_mut()
        .new_bb()
        .basic_block(Some("%entry".into()));
    func_data.layout_mut().bbs_mut().extend([entry_bb]);

    let mut cur_bb = entry_bb;
    for item in &block.block_items {
        if bb_is_terminated(func_data, cur_bb) {
            break;
        }
        match item {
            BlockItem::Stmt(stmt) => gen_stmt(func_data, stmt, map, entry_bb, &mut cur_bb),
            BlockItem::Decl(decl) => gen_decl(func_data, decl, map, entry_bb, &mut cur_bb),
        }
    }
}

fn bb_is_terminated(func_data: &FunctionData, bb: BasicBlock) -> bool {
    let Some(last_inst) = func_data
        .layout()
        .bbs()
        .node(&bb)
        .expect("`bb` does not exist")
        .insts()
        .back_key()
    else {
        return false;
    };
    matches!(
        func_data.dfg().value(*last_inst).kind(),
        ValueKind::Jump(_) | ValueKind::Branch(_) | ValueKind::Return(_)
    )
}

fn gen_decl(
    func_data: &mut FunctionData,
    decl: &Decl,
    map: &mut HashMap<String, Value>,
    entry_bb: BasicBlock,
    cur_bb: &mut BasicBlock,
) {
    match decl {
        Decl::Const(..) => {
            unreachable!("常量声明应该在语义分析阶段被删除")
        }
        Decl::Var(var_decl) => {
            gen_var_decl(func_data, var_decl, map, entry_bb, cur_bb);
        }
    }
}

fn gen_var_decl(
    func_data: &mut FunctionData,
    var_decl: &VarDecl,
    map: &mut HashMap<String, Value>,
    entry_bb: BasicBlock,
    cur_bb: &mut BasicBlock,
) {
    for var_def in &var_decl.var_defs {
        gen_var_def(func_data, var_def, map, entry_bb, cur_bb);
    }
}

fn gen_var_def(
    func_data: &mut FunctionData,
    var_def: &VarDef,
    map: &mut HashMap<String, Value>,
    entry_bb: BasicBlock,
    cur_bb: &mut BasicBlock,
) {
    let alloc = func_data.dfg_mut().new_value().alloc(Type::get_i32());
    let _ = func_data
        .layout_mut()
        .bb_mut(entry_bb)
        .insts_mut()
        .push_key_front(alloc);
    match var_def {
        VarDef::Ident(ident) => {
            map.insert(ident.clone(), alloc);
        }
        VarDef::Assign { ident, val } => {
            map.insert(ident.clone(), alloc);
            let init_val = gen_init_val(func_data, val, map, cur_bb);
            let store = func_data.dfg_mut().new_value().store(init_val, alloc);
            let _ = func_data
                .layout_mut()
                .bb_mut(*cur_bb)
                .insts_mut()
                .push_key_back(store);
        }
    }
}

fn gen_init_val(
    func_data: &mut FunctionData,
    init_val: &InitVal,
    map: &mut HashMap<String, Value>,
    cur_bb: &mut BasicBlock,
) -> Value {
    gen_exp(func_data, &init_val.exp, map, *cur_bb)
}

fn gen_stmt(
    func_data: &mut FunctionData,
    stmt: &Stmt,
    map: &mut HashMap<String, Value>,
    entry_bb: BasicBlock,
    cur_bb: &mut BasicBlock,
) {
    if bb_is_terminated(func_data, *cur_bb) {
        return;
    }

    match stmt {
        Stmt::Assign { lval, exp } => {
            let val = gen_exp(func_data, exp, map, *cur_bb);
            if let Some(alloc) = map.get(&lval.ident) {
                let store = func_data.dfg_mut().new_value().store(val, *alloc);
                let _ = func_data
                    .layout_mut()
                    .bb_mut(*cur_bb)
                    .insts_mut()
                    .push_key_back(store);
            } else {
                panic!("Identifier {} not found", lval.ident);
            }
        }
        Stmt::Block(block) => {
            let mut local_map = map.clone();
            for item in &block.block_items {
                if bb_is_terminated(func_data, *cur_bb) {
                    break;
                }
                match item {
                    BlockItem::Stmt(stmt) => {
                        gen_stmt(func_data, stmt, &mut local_map, entry_bb, cur_bb)
                    }
                    BlockItem::Decl(decl) => {
                        gen_decl(func_data, decl, &mut local_map, entry_bb, cur_bb)
                    }
                }
            }
        }
        Stmt::Exp(Some(exp)) => {
            let _ = gen_exp(func_data, exp, map, *cur_bb);
        }
        Stmt::Exp(None) => {}
        Stmt::If { cond, then, else_ } => {
            let cond_val = gen_exp(func_data, cond, map, *cur_bb);

            let end_bb = func_data.dfg_mut().new_bb().basic_block(None);
            func_data.layout_mut().bbs_mut().extend([end_bb]);

            let then_entry_bb = func_data.dfg_mut().new_bb().basic_block(None);
            func_data.layout_mut().bbs_mut().extend([then_entry_bb]);
            let mut then_exit_bb = then_entry_bb;
            let mut then_map = map.clone();
            gen_stmt(func_data, then, &mut then_map, entry_bb, &mut then_exit_bb);
            if !bb_is_terminated(func_data, then_exit_bb) {
                let jmp_to_end = func_data.dfg_mut().new_value().jump(end_bb);
                let _ = func_data
                    .layout_mut()
                    .bb_mut(then_exit_bb)
                    .insts_mut()
                    .push_key_back(jmp_to_end);
            }

            if let Some(else_) = else_ {
                let else_entry_bb = func_data.dfg_mut().new_bb().basic_block(None);
                func_data.layout_mut().bbs_mut().extend([else_entry_bb]);
                let mut else_exit_bb = else_entry_bb;
                let mut else_map = map.clone();
                gen_stmt(func_data, else_, &mut else_map, entry_bb, &mut else_exit_bb);
                if !bb_is_terminated(func_data, else_exit_bb) {
                    let jmp_to_end = func_data.dfg_mut().new_value().jump(end_bb);
                    let _ = func_data
                        .layout_mut()
                        .bb_mut(else_exit_bb)
                        .insts_mut()
                        .push_key_back(jmp_to_end);
                }
                let br_val =
                    func_data
                        .dfg_mut()
                        .new_value()
                        .branch(cond_val, then_entry_bb, else_entry_bb);
                let _ = func_data
                    .layout_mut()
                    .bb_mut(*cur_bb)
                    .insts_mut()
                    .push_key_back(br_val);
            } else {
                let br_val =
                    func_data
                        .dfg_mut()
                        .new_value()
                        .branch(cond_val, then_entry_bb, end_bb);
                let _ = func_data
                    .layout_mut()
                    .bb_mut(*cur_bb)
                    .insts_mut()
                    .push_key_back(br_val);
            }
            *cur_bb = end_bb;
        }
        Stmt::Ret(exp) => {
            let ret = match exp {
                Some(exp) => {
                    let val = gen_exp(func_data, exp, map, *cur_bb);
                    func_data.dfg_mut().new_value().ret(Some(val))
                }
                None => func_data.dfg_mut().new_value().ret(None),
            };
            let _ = func_data
                .layout_mut()
                .bb_mut(*cur_bb)
                .insts_mut()
                .push_key_back(ret);
        }
    }
}

fn gen_exp(
    func_data: &mut FunctionData,
    exp: &Exp,
    map: &mut HashMap<String, Value>,
    cur_bb: BasicBlock,
) -> Value {
    gen_lor_exp(func_data, &exp.lor_exp, map, cur_bb)
}

fn gen_lor_exp(
    func_data: &mut FunctionData,
    lor_exp: &LOrExp,
    map: &mut HashMap<String, Value>,
    cur_bb: BasicBlock,
) -> Value {
    match lor_exp {
        LOrExp::LAnd(land) => gen_land_exp(func_data, land, map, cur_bb),
        LOrExp::LOr { lor, land } => {
            let lval = gen_lor_exp(func_data, lor, map, cur_bb);
            let zero = func_data.dfg_mut().new_value().integer(0);
            let lhs = func_data
                .dfg_mut()
                .new_value()
                .binary(BinaryOp::NotEq, lval, zero);
            let rval = gen_land_exp(func_data, land, map, cur_bb);
            let zero = func_data.dfg_mut().new_value().integer(0);
            let rhs = func_data
                .dfg_mut()
                .new_value()
                .binary(BinaryOp::NotEq, rval, zero);
            let _ = func_data
                .layout_mut()
                .bb_mut(cur_bb)
                .insts_mut()
                .push_key_back(lhs);
            let _ = func_data
                .layout_mut()
                .bb_mut(cur_bb)
                .insts_mut()
                .push_key_back(rhs);

            let result = func_data
                .dfg_mut()
                .new_value()
                .binary(BinaryOp::Or, lhs, rhs);
            let _ = func_data
                .layout_mut()
                .bb_mut(cur_bb)
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
    cur_bb: BasicBlock,
) -> Value {
    match land_exp {
        LAndExp::Eq(eq) => gen_eq_exp(func_data, eq, map, cur_bb),
        LAndExp::LAnd { land, eq } => {
            let lval = gen_land_exp(func_data, land, map, cur_bb);
            let zero = func_data.dfg_mut().new_value().integer(0);
            let lhs = func_data
                .dfg_mut()
                .new_value()
                .binary(BinaryOp::NotEq, lval, zero);
            let rval = gen_eq_exp(func_data, eq, map, cur_bb);
            let zero = func_data.dfg_mut().new_value().integer(0);
            let rhs = func_data
                .dfg_mut()
                .new_value()
                .binary(BinaryOp::NotEq, rval, zero);
            let _ = func_data
                .layout_mut()
                .bb_mut(cur_bb)
                .insts_mut()
                .push_key_back(lhs);
            let _ = func_data
                .layout_mut()
                .bb_mut(cur_bb)
                .insts_mut()
                .push_key_back(rhs);

            let result = func_data
                .dfg_mut()
                .new_value()
                .binary(BinaryOp::And, lhs, rhs);
            let _ = func_data
                .layout_mut()
                .bb_mut(cur_bb)
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
    cur_bb: BasicBlock,
) -> Value {
    match eq_exp {
        EqExp::Rel(rel) => gen_rel_exp(func_data, rel, map, cur_bb),
        EqExp::Eq { eq, op, rel } => {
            let lhs = gen_eq_exp(func_data, eq, map, cur_bb);
            let rhs = gen_rel_exp(func_data, rel, map, cur_bb);
            match op {
                EqOp::Eq => {
                    let result = func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Eq, lhs, rhs);
                    let _ = func_data
                        .layout_mut()
                        .bb_mut(cur_bb)
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
                        .bb_mut(cur_bb)
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
    cur_bb: BasicBlock,
) -> Value {
    match rel_exp {
        RelExp::Add(add) => gen_add_exp(func_data, add, map, cur_bb),
        RelExp::Rel { rel, op, add } => {
            let lhs = gen_rel_exp(func_data, rel, map, cur_bb);
            let rhs = gen_add_exp(func_data, add, map, cur_bb);
            match op {
                RelOp::Lt => {
                    let result = func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Lt, lhs, rhs);
                    let _ = func_data
                        .layout_mut()
                        .bb_mut(cur_bb)
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
                        .bb_mut(cur_bb)
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
                        .bb_mut(cur_bb)
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
                        .bb_mut(cur_bb)
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
    cur_bb: BasicBlock,
) -> Value {
    match add_exp {
        AddExp::Mul(mul) => gen_mul_exp(func_data, mul, map, cur_bb),
        AddExp::Add { add, op, mul } => {
            let lhs = gen_add_exp(func_data, add, map, cur_bb);
            let rhs = gen_mul_exp(func_data, mul, map, cur_bb);
            match op {
                AddOp::Minus => {
                    let result = func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Sub, lhs, rhs);
                    let _ = func_data
                        .layout_mut()
                        .bb_mut(cur_bb)
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
                        .bb_mut(cur_bb)
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
    cur_bb: BasicBlock,
) -> Value {
    match mul_exp {
        MulExp::Unary(unary) => gen_unary_exp(func_data, unary, map, cur_bb),
        MulExp::Mul { mul, op, unary } => {
            let lhs = gen_mul_exp(func_data, mul, map, cur_bb);
            let rhs = gen_unary_exp(func_data, unary, map, cur_bb);
            match op {
                MulOp::Div => {
                    let result = func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Div, lhs, rhs);
                    let _ = func_data
                        .layout_mut()
                        .bb_mut(cur_bb)
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
                        .bb_mut(cur_bb)
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
                        .bb_mut(cur_bb)
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
    unary_exp: &UnaryExp,
    map: &mut HashMap<String, Value>,
    cur_bb: BasicBlock,
) -> Value {
    match unary_exp {
        UnaryExp::PrimaryExp(primary_exp) => gen_primary_exp(func_data, primary_exp, map, cur_bb),
        UnaryExp::Unary { op, exp } => {
            let val = gen_unary_exp(func_data, exp, map, cur_bb);

            match op {
                UnaryOp::Minus => {
                    let zero = func_data.dfg_mut().new_value().integer(0);
                    let result = func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Sub, zero, val);
                    let _ = func_data
                        .layout_mut()
                        .bb_mut(cur_bb)
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
                        .bb_mut(cur_bb)
                        .insts_mut()
                        .push_key_back(result);
                    result
                }
            }
        }
    }
}

fn gen_primary_exp(
    func_data: &mut FunctionData,
    primary_exp: &PrimaryExp,
    map: &mut HashMap<String, Value>,
    cur_bb: BasicBlock,
) -> Value {
    match primary_exp {
        PrimaryExp::Exp(exp) => gen_exp(func_data, exp, map, cur_bb),
        PrimaryExp::LVal(lval) => {
            if let Some(alloc) = map.get(&lval.ident) {
                let load = func_data.dfg_mut().new_value().load(*alloc);
                let _ = func_data
                    .layout_mut()
                    .bb_mut(cur_bb)
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
