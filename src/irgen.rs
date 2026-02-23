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

fn push_inst(func_data: &mut FunctionData, bb: BasicBlock, inst: Value) {
    let _ = func_data
        .layout_mut()
        .bb_mut(bb)
        .insts_mut()
        .push_key_back(inst);
}

fn gen_bool_value(func_data: &mut FunctionData, val: Value, bb: BasicBlock) -> Value {
    let zero = func_data.dfg_mut().new_value().integer(0);
    let bool_val = func_data
        .dfg_mut()
        .new_value()
        .binary(BinaryOp::NotEq, val, zero);
    push_inst(func_data, bb, bool_val);
    bool_val
}

fn new_temp_i32_alloc(func_data: &mut FunctionData) -> Value {
    let alloc = func_data.dfg_mut().new_value().alloc(Type::get_i32());
    let entry_bb = func_data
        .layout()
        .entry_bb()
        .expect("function must have entry basic block");
    let _ = func_data
        .layout_mut()
        .bb_mut(entry_bb)
        .insts_mut()
        .push_key_front(alloc);
    alloc
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
            push_inst(func_data, *cur_bb, store);
        }
    }
}

fn gen_init_val(
    func_data: &mut FunctionData,
    init_val: &InitVal,
    map: &mut HashMap<String, Value>,
    cur_bb: &mut BasicBlock,
) -> Value {
    gen_exp(func_data, &init_val.exp, map, cur_bb)
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
            let val = gen_exp(func_data, exp, map, cur_bb);
            if let Some(alloc) = map.get(&lval.ident) {
                let store = func_data.dfg_mut().new_value().store(val, *alloc);
                push_inst(func_data, *cur_bb, store);
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
            let _ = gen_exp(func_data, exp, map, cur_bb);
        }
        Stmt::Exp(None) => {}
        Stmt::If { cond, then, else_ } => {
            let cond_raw = gen_exp(func_data, cond, map, cur_bb);
            let cond_val = gen_bool_value(func_data, cond_raw, *cur_bb);

            let end_bb = func_data
                .dfg_mut()
                .new_bb()
                .basic_block(Some("%end".into()));
            func_data.layout_mut().bbs_mut().extend([end_bb]);

            let then_entry_bb = func_data
                .dfg_mut()
                .new_bb()
                .basic_block(Some("%then".into()));
            func_data.layout_mut().bbs_mut().extend([then_entry_bb]);
            let mut then_exit_bb = then_entry_bb;
            let mut then_map = map.clone();
            gen_stmt(func_data, then, &mut then_map, entry_bb, &mut then_exit_bb);
            if !bb_is_terminated(func_data, then_exit_bb) {
                let jmp_to_end = func_data.dfg_mut().new_value().jump(end_bb);
                push_inst(func_data, then_exit_bb, jmp_to_end);
            }

            if let Some(else_) = else_ {
                let else_entry_bb = func_data
                    .dfg_mut()
                    .new_bb()
                    .basic_block(Some("%else".into()));
                func_data.layout_mut().bbs_mut().extend([else_entry_bb]);
                let mut else_exit_bb = else_entry_bb;
                let mut else_map = map.clone();
                gen_stmt(func_data, else_, &mut else_map, entry_bb, &mut else_exit_bb);
                if !bb_is_terminated(func_data, else_exit_bb) {
                    let jmp_to_end = func_data.dfg_mut().new_value().jump(end_bb);
                    push_inst(func_data, else_exit_bb, jmp_to_end);
                }
                let br_val =
                    func_data
                        .dfg_mut()
                        .new_value()
                        .branch(cond_val, then_entry_bb, else_entry_bb);
                push_inst(func_data, *cur_bb, br_val);
            } else {
                let br_val =
                    func_data
                        .dfg_mut()
                        .new_value()
                        .branch(cond_val, then_entry_bb, end_bb);
                push_inst(func_data, *cur_bb, br_val);
            }
            *cur_bb = end_bb;
        }
        Stmt::Ret(exp) => {
            let ret = match exp {
                Some(exp) => {
                    let val = gen_exp(func_data, exp, map, cur_bb);
                    func_data.dfg_mut().new_value().ret(Some(val))
                }
                None => func_data.dfg_mut().new_value().ret(None),
            };
            push_inst(func_data, *cur_bb, ret);
        }
    }
}

fn gen_exp(
    func_data: &mut FunctionData,
    exp: &Exp,
    map: &mut HashMap<String, Value>,
    cur_bb: &mut BasicBlock,
) -> Value {
    gen_lor_exp(func_data, &exp.lor_exp, map, cur_bb)
}

fn gen_lor_exp(
    func_data: &mut FunctionData,
    lor_exp: &LOrExp,
    map: &mut HashMap<String, Value>,
    cur_bb: &mut BasicBlock,
) -> Value {
    match lor_exp {
        LOrExp::LAnd(land) => gen_land_exp(func_data, land, map, cur_bb),
        LOrExp::LOr { lor, land } => {
            let left_raw = gen_lor_exp(func_data, lor, map, cur_bb);
            let left_bool = gen_bool_value(func_data, left_raw, *cur_bb);

            let rhs_bb = func_data
                .dfg_mut()
                .new_bb()
                .basic_block(Some("%lor.rhs".into()));
            let end_bb = func_data
                .dfg_mut()
                .new_bb()
                .basic_block(Some("%lor.end".into()));
            func_data.layout_mut().bbs_mut().extend([rhs_bb, end_bb]);

            let result_alloc = new_temp_i32_alloc(func_data);

            let one = func_data.dfg_mut().new_value().integer(1);
            let store_default_true = func_data.dfg_mut().new_value().store(one, result_alloc);
            push_inst(func_data, *cur_bb, store_default_true);

            let br = func_data
                .dfg_mut()
                .new_value()
                .branch(left_bool, end_bb, rhs_bb);
            push_inst(func_data, *cur_bb, br);

            let mut rhs_exit_bb = rhs_bb;
            let right_raw = gen_land_exp(func_data, land, map, &mut rhs_exit_bb);
            let right_bool = gen_bool_value(func_data, right_raw, rhs_exit_bb);
            let store_rhs = func_data
                .dfg_mut()
                .new_value()
                .store(right_bool, result_alloc);
            push_inst(func_data, rhs_exit_bb, store_rhs);
            if !bb_is_terminated(func_data, rhs_exit_bb) {
                let jmp_from_rhs = func_data.dfg_mut().new_value().jump(end_bb);
                push_inst(func_data, rhs_exit_bb, jmp_from_rhs);
            }

            *cur_bb = end_bb;
            let load_result = func_data.dfg_mut().new_value().load(result_alloc);
            push_inst(func_data, *cur_bb, load_result);
            load_result
        }
    }
}

fn gen_land_exp(
    func_data: &mut FunctionData,
    land_exp: &LAndExp,
    map: &mut HashMap<String, Value>,
    cur_bb: &mut BasicBlock,
) -> Value {
    match land_exp {
        LAndExp::Eq(eq) => gen_eq_exp(func_data, eq, map, cur_bb),
        LAndExp::LAnd { land, eq } => {
            let left_raw = gen_land_exp(func_data, land, map, cur_bb);
            let left_bool = gen_bool_value(func_data, left_raw, *cur_bb);

            let rhs_bb = func_data
                .dfg_mut()
                .new_bb()
                .basic_block(Some("%land.rhs".into()));
            let end_bb = func_data
                .dfg_mut()
                .new_bb()
                .basic_block(Some("%land.end".into()));
            func_data.layout_mut().bbs_mut().extend([rhs_bb, end_bb]);

            let result_alloc = new_temp_i32_alloc(func_data);

            let zero = func_data.dfg_mut().new_value().integer(0);
            let store_default_false = func_data.dfg_mut().new_value().store(zero, result_alloc);
            push_inst(func_data, *cur_bb, store_default_false);

            let br = func_data
                .dfg_mut()
                .new_value()
                .branch(left_bool, rhs_bb, end_bb);
            push_inst(func_data, *cur_bb, br);

            let mut rhs_exit_bb = rhs_bb;
            let right_raw = gen_eq_exp(func_data, eq, map, &mut rhs_exit_bb);
            let right_bool = gen_bool_value(func_data, right_raw, rhs_exit_bb);
            let store_rhs = func_data
                .dfg_mut()
                .new_value()
                .store(right_bool, result_alloc);
            push_inst(func_data, rhs_exit_bb, store_rhs);
            if !bb_is_terminated(func_data, rhs_exit_bb) {
                let jmp_from_rhs = func_data.dfg_mut().new_value().jump(end_bb);
                push_inst(func_data, rhs_exit_bb, jmp_from_rhs);
            }

            *cur_bb = end_bb;
            let load_result = func_data.dfg_mut().new_value().load(result_alloc);
            push_inst(func_data, *cur_bb, load_result);
            load_result
        }
    }
}

fn gen_eq_exp(
    func_data: &mut FunctionData,
    eq_exp: &EqExp,
    map: &mut HashMap<String, Value>,
    cur_bb: &mut BasicBlock,
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
                    push_inst(func_data, *cur_bb, result);
                    result
                }
                EqOp::Neq => {
                    let result = func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::NotEq, lhs, rhs);
                    push_inst(func_data, *cur_bb, result);
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
    cur_bb: &mut BasicBlock,
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
                    push_inst(func_data, *cur_bb, result);
                    result
                }
                RelOp::Gt => {
                    let result = func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Gt, lhs, rhs);
                    push_inst(func_data, *cur_bb, result);
                    result
                }
                RelOp::Le => {
                    let result = func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Le, lhs, rhs);
                    push_inst(func_data, *cur_bb, result);
                    result
                }
                RelOp::Ge => {
                    let result = func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Ge, lhs, rhs);
                    push_inst(func_data, *cur_bb, result);
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
    cur_bb: &mut BasicBlock,
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
                    push_inst(func_data, *cur_bb, result);
                    result
                }
                AddOp::Plus => {
                    let result = func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Add, lhs, rhs);
                    push_inst(func_data, *cur_bb, result);
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
    cur_bb: &mut BasicBlock,
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
                    push_inst(func_data, *cur_bb, result);
                    result
                }
                MulOp::Mod => {
                    let result = func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Mod, lhs, rhs);
                    push_inst(func_data, *cur_bb, result);
                    result
                }
                MulOp::Mul => {
                    let result = func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Mul, lhs, rhs);
                    push_inst(func_data, *cur_bb, result);
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
    cur_bb: &mut BasicBlock,
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
                    push_inst(func_data, *cur_bb, result);
                    result
                }
                UnaryOp::Plus => val,
                UnaryOp::Not => {
                    let zero = func_data.dfg_mut().new_value().integer(0);
                    let result = func_data
                        .dfg_mut()
                        .new_value()
                        .binary(BinaryOp::Eq, zero, val);
                    push_inst(func_data, *cur_bb, result);
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
    cur_bb: &mut BasicBlock,
) -> Value {
    match primary_exp {
        PrimaryExp::Exp(exp) => gen_exp(func_data, exp, map, cur_bb),
        PrimaryExp::LVal(lval) => {
            if let Some(alloc) = map.get(&lval.ident) {
                let load = func_data.dfg_mut().new_value().load(*alloc);
                push_inst(func_data, *cur_bb, load);
                load
            } else {
                panic!("Identifier {} not found", lval.ident);
            }
        }
        PrimaryExp::Number(num) => func_data.dfg_mut().new_value().integer(*num),
    }
}
