use crate::ast::*;
use std::collections::HashMap;

pub fn constant_eval(ast: &mut CompUnit) {
    let mut map: HashMap<String, i32> = HashMap::new();

    collect_constants(&mut ast.func_def, &mut map);

    replace_constants(&mut ast.func_def, &map);
}

fn collect_constants(func: &mut FuncDef, map: &mut HashMap<String, i32>) {
    collect_block(&mut func.block, map);
}

fn collect_block(block: &mut Block, map: &mut HashMap<String, i32>) {
    for item in &mut block.block_items {
        if let BlockItem::Decl(decl) = item {
            eval_decl(decl, map)
        }
    }
}

fn replace_constants(func: &mut FuncDef, map: &HashMap<String, i32>) {
    replace_block(&mut func.block, map);
}

fn replace_block(block: &mut Block, map: &HashMap<String, i32>) {
    block
        .block_items
        .retain(|item| !matches!(item, BlockItem::Decl(_)));

    for item in &mut block.block_items {
        if let BlockItem::Stmt(stmt) = item {
            replace_exp(&mut stmt.exp, map);
        }
    }
}

fn replace_exp(exp: &mut Exp, map: &HashMap<String, i32>) {
    replace_lor_exp(&mut exp.lor_exp, map);
}

fn replace_lor_exp(lor_exp: &mut LOrExp, map: &HashMap<String, i32>) {
    match lor_exp {
        LOrExp::LAnd(land) => replace_land_exp(land, map),
        LOrExp::LOr { lor, land } => {
            replace_lor_exp(lor, map);
            replace_land_exp(land, map);
        }
    }
}

fn replace_land_exp(land_exp: &mut LAndExp, map: &HashMap<String, i32>) {
    match land_exp {
        LAndExp::Eq(eq) => replace_eq_exp(eq, map),
        LAndExp::LAnd { land, eq } => {
            replace_land_exp(land, map);
            replace_eq_exp(eq, map);
        }
    }
}

fn replace_eq_exp(eq_exp: &mut EqExp, map: &HashMap<String, i32>) {
    match eq_exp {
        EqExp::Rel(rel) => replace_rel_exp(rel, map),
        EqExp::Eq { eq, rel, .. } => {
            replace_eq_exp(eq, map);
            replace_rel_exp(rel, map);
        }
    }
}

fn replace_rel_exp(rel_exp: &mut RelExp, map: &HashMap<String, i32>) {
    match rel_exp {
        RelExp::Add(add) => replace_add_exp(add, map),
        RelExp::Rel { rel, add, .. } => {
            replace_rel_exp(rel, map);
            replace_add_exp(add, map);
        }
    }
}

fn replace_add_exp(add_exp: &mut AddExp, map: &HashMap<String, i32>) {
    match add_exp {
        AddExp::Mul(mul) => replace_mul_exp(mul, map),
        AddExp::Add { add, mul, .. } => {
            replace_add_exp(add, map);
            replace_mul_exp(mul, map);
        }
    }
}

fn replace_mul_exp(mul_exp: &mut MulExp, map: &HashMap<String, i32>) {
    match mul_exp {
        MulExp::Unary(unary) => replace_unary_exp(unary, map),
        MulExp::Mul { mul, unary, .. } => {
            replace_mul_exp(mul, map);
            replace_unary_exp(unary, map);
        }
    }
}

fn replace_unary_exp(unary_exp: &mut UnaryExp, map: &HashMap<String, i32>) {
    match unary_exp {
        UnaryExp::PrimaryExp(pri_exp) => replace_primary_exp(pri_exp, map),
        UnaryExp::Unary { exp, .. } => {
            replace_unary_exp(exp, map);
        }
    }
}

fn replace_primary_exp(pri_exp: &mut PrimaryExp, map: &HashMap<String, i32>) {
    match pri_exp {
        PrimaryExp::Exp(exp) => replace_exp(exp, map),
        PrimaryExp::LVal(lval) => {
            if let Some(&value) = map.get(&lval.ident) {
                *pri_exp = PrimaryExp::Number(value);
            }
        }
        PrimaryExp::Number(_) => {}
    }
}

fn eval_decl(decl: &mut Decl, map: &mut HashMap<String, i32>) {
    eval_const_decl(&mut decl.const_decl, map)
}

fn eval_const_decl(const_decl: &mut ConstDecl, map: &mut HashMap<String, i32>) {
    for const_def in &mut const_decl.const_defs {
        eval_const_def(const_def, map)
    }
}

fn eval_const_def(const_def: &mut ConstDef, map: &mut HashMap<String, i32>) {
    let ident = &const_def.ident;
    let const_init_val = &const_def.const_init_val;
    if map.contains_key(ident) {
        panic!(
            "The const value {ident} is already exist, which's value is {}",
            map.get(ident).unwrap()
        );
    } else {
        let val = eval_const_init_val(const_init_val, map);
        map.insert(ident.to_string(), val);
    }
}

fn eval_const_init_val(const_init_val: &ConstInitVal, map: &HashMap<String, i32>) -> i32 {
    eval_const_exp(&const_init_val.const_exp, map)
}

fn eval_const_exp(const_exp: &ConstExp, map: &HashMap<String, i32>) -> i32 {
    eval_exp(&const_exp.exp, map)
}

fn eval_exp(exp: &Exp, map: &HashMap<String, i32>) -> i32 {
    eval_lor_exp(&exp.lor_exp, map)
}

fn eval_lor_exp(lor_exp: &LOrExp, map: &HashMap<String, i32>) -> i32 {
    match lor_exp {
        LOrExp::LAnd(land) => eval_land_exp(land, map),
        LOrExp::LOr { lor, land } => {
            let lval = eval_lor_exp(lor, map);
            let lhs = if lval != 0 { 1 } else { 0 };
            let rval = eval_land_exp(land, map);
            let rhs = if rval != 0 { 1 } else { 0 };
            if lhs != 0 || rhs != 0 { 1 } else { 0 }
        }
    }
}

fn eval_land_exp(land_exp: &LAndExp, map: &HashMap<String, i32>) -> i32 {
    match land_exp {
        LAndExp::Eq(eq) => eval_eq_exp(eq, map),
        LAndExp::LAnd { land, eq } => {
            let lval = eval_land_exp(land, map);
            let lhs = if lval != 0 { 1 } else { 0 };
            let rval = eval_eq_exp(eq, map);
            let rhs = if rval != 0 { 1 } else { 0 };
            if lhs != 0 && rhs != 0 { 1 } else { 0 }
        }
    }
}

fn eval_eq_exp(eq_exp: &EqExp, map: &HashMap<String, i32>) -> i32 {
    match eq_exp {
        EqExp::Rel(rel) => eval_rel_exp(rel, map),
        EqExp::Eq { eq, op, rel } => {
            let lhs = eval_eq_exp(eq, map);
            let rhs = eval_rel_exp(rel, map);
            match op {
                EqOp::Eq => {
                    if lhs == rhs {
                        1
                    } else {
                        0
                    }
                }
                EqOp::Neq => {
                    if lhs != rhs {
                        1
                    } else {
                        0
                    }
                }
            }
        }
    }
}

fn eval_rel_exp(rel_exp: &RelExp, map: &HashMap<String, i32>) -> i32 {
    match rel_exp {
        RelExp::Add(add) => eval_add_exp(add, map),
        RelExp::Rel { rel, op, add } => {
            let lhs = eval_rel_exp(rel, map);
            let rhs = eval_add_exp(add, map);
            match op {
                RelOp::Lt => {
                    if lhs < rhs {
                        1
                    } else {
                        0
                    }
                }
                RelOp::Gt => {
                    if lhs > rhs {
                        1
                    } else {
                        0
                    }
                }
                RelOp::Le => {
                    if lhs <= rhs {
                        1
                    } else {
                        0
                    }
                }
                RelOp::Ge => {
                    if lhs >= rhs {
                        1
                    } else {
                        0
                    }
                }
            }
        }
    }
}

fn eval_add_exp(add_exp: &AddExp, map: &HashMap<String, i32>) -> i32 {
    match add_exp {
        AddExp::Mul(mul) => eval_mul_exp(mul, map),
        AddExp::Add { add, op, mul } => {
            let lhs = eval_add_exp(add, map);
            let rhs = eval_mul_exp(mul, map);
            match op {
                AddOp::Minus => lhs - rhs,
                AddOp::Plus => lhs + rhs,
            }
        }
    }
}

fn eval_mul_exp(mul_exp: &MulExp, map: &HashMap<String, i32>) -> i32 {
    match mul_exp {
        MulExp::Unary(unary) => eval_unary_exp(unary, map),
        MulExp::Mul { mul, op, unary } => {
            let lhs = eval_mul_exp(mul, map);
            let rhs = eval_unary_exp(unary, map);
            match op {
                MulOp::Div => lhs / rhs,
                MulOp::Mod => lhs % rhs,
                MulOp::Mul => lhs * rhs,
            }
        }
    }
}

fn eval_unary_exp(unary_exp: &UnaryExp, map: &HashMap<String, i32>) -> i32 {
    match unary_exp {
        UnaryExp::PrimaryExp(pri_exp) => eval_primary_exp(pri_exp, map),
        UnaryExp::Unary { op, exp } => {
            let val = eval_unary_exp(exp, map);
            match op {
                UnaryOp::Minus => -val,
                UnaryOp::Plus => val,
                UnaryOp::Not => {
                    if val == 0 {
                        1
                    } else {
                        0
                    }
                }
            }
        }
    }
}

fn eval_primary_exp(pri_exp: &PrimaryExp, map: &HashMap<String, i32>) -> i32 {
    match pri_exp {
        PrimaryExp::Exp(exp) => eval_exp(exp, map),
        PrimaryExp::LVal(lval) => *map
            .get(&lval.ident)
            .unwrap_or_else(|| panic!("Undefined constant: {}", lval.ident)),
        PrimaryExp::Number(num) => *num,
    }
}
