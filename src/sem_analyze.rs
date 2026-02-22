use crate::ast::*;
use std::collections::HashMap;

#[derive(Clone, Debug)]
pub enum Symbol {
    Const(i32),
    Var(String),
}

type Scope = HashMap<String, Symbol>;

pub(crate) struct SymbolTable {
    scopes: Vec<Scope>,
}

impl SymbolTable {
    pub(crate) fn new() -> Self {
        SymbolTable {
            scopes: vec![HashMap::new()],
        }
    }

    pub(crate) fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    pub(crate) fn exit_scope(&mut self) {
        if self.scopes.len() > 1 {
            self.scopes.pop();
        }
    }

    pub(crate) fn insert(&mut self, name: String, symbol: Symbol) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name, symbol);
        }
    }

    pub(crate) fn contains_key_in_current_scope(&self, name: &str) -> bool {
        self.scopes
            .last()
            .is_some_and(|scope| scope.contains_key(name))
    }

    pub(crate) fn get(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Some(symbol);
            }
        }
        None
    }
}

pub(crate) fn constant_eval(ast: &mut CompUnit) {
    let mut map = SymbolTable::new();
    let mut next_var_id = 0usize;

    process_block(&mut ast.func_def.block, &mut map, &mut next_var_id);
}

fn process_block(block: &mut Block, map: &mut SymbolTable, next_var_id: &mut usize) {
    map.enter_scope();
    let mut new_items = Vec::with_capacity(block.block_items.len());

    for item in std::mem::take(&mut block.block_items) {
        match item {
            BlockItem::Decl(Decl::Const(mut const_decl)) => {
                eval_const_decl(&mut const_decl, map);
            }
            BlockItem::Decl(Decl::Var(mut var_decl)) => {
                process_var_decl(&mut var_decl, map, next_var_id);
                new_items.push(BlockItem::Decl(Decl::Var(var_decl)));
            }
            BlockItem::Stmt(mut stmt) => {
                process_stmt(&mut stmt, map, next_var_id);
                new_items.push(BlockItem::Stmt(stmt));
            }
        }
    }

    block.block_items = new_items;
    map.exit_scope();
}

fn process_stmt(stmt: &mut Stmt, map: &mut SymbolTable, next_var_id: &mut usize) {
    match stmt {
        Stmt::Assign { lval, exp } => {
            replace_exp(exp, map);
            let symbol = map
                .get(&lval.ident)
                .unwrap_or_else(|| panic!("Undefined variable: {}", lval.ident));
            match symbol {
                Symbol::Const(_) => panic!("Cannot assign to const: {}", lval.ident),
                Symbol::Var(unique_ident) => {
                    lval.ident = unique_ident.clone();
                }
            }
        }
        Stmt::Block(block) => process_block(block, map, next_var_id),
        Stmt::Exp(Some(exp)) => replace_exp(exp, map),
        Stmt::Ret(Some(exp)) => replace_exp(exp, map),
        Stmt::Exp(None) | Stmt::Ret(None) => {}
    }
}

fn process_var_decl(var_decl: &mut VarDecl, map: &mut SymbolTable, next_var_id: &mut usize) {
    for var_def in &mut var_decl.var_defs {
        process_var_def(var_def, map, next_var_id);
    }
}

fn process_var_def(var_def: &mut VarDef, map: &mut SymbolTable, next_var_id: &mut usize) {
    match var_def {
        VarDef::Ident(ident) => {
            if map.contains_key_in_current_scope(ident) {
                panic!();
            }
            let original_ident = ident.clone();
            let unique_ident = format!("{}#{}", original_ident, *next_var_id);
            *next_var_id += 1;
            *ident = unique_ident.clone();
            map.insert(original_ident, Symbol::Var(unique_ident));
        }
        VarDef::Assign { ident, val } => {
            if map.contains_key_in_current_scope(ident) {
                panic!();
            }
            replace_exp(&mut val.exp, map);
            let original_ident = ident.clone();
            let unique_ident = format!("{}#{}", original_ident, *next_var_id);
            *next_var_id += 1;
            *ident = unique_ident.clone();
            map.insert(original_ident, Symbol::Var(unique_ident));
        }
    }
}

fn replace_exp(exp: &mut Exp, map: &SymbolTable) {
    replace_lor_exp(&mut exp.lor_exp, map);
}

fn replace_lor_exp(lor_exp: &mut LOrExp, map: &SymbolTable) {
    match lor_exp {
        LOrExp::LAnd(land) => replace_land_exp(land, map),
        LOrExp::LOr { lor, land } => {
            replace_lor_exp(lor, map);
            replace_land_exp(land, map);
        }
    }
}

fn replace_land_exp(land_exp: &mut LAndExp, map: &SymbolTable) {
    match land_exp {
        LAndExp::Eq(eq) => replace_eq_exp(eq, map),
        LAndExp::LAnd { land, eq } => {
            replace_land_exp(land, map);
            replace_eq_exp(eq, map);
        }
    }
}

fn replace_eq_exp(eq_exp: &mut EqExp, map: &SymbolTable) {
    match eq_exp {
        EqExp::Rel(rel) => replace_rel_exp(rel, map),
        EqExp::Eq { eq, rel, .. } => {
            replace_eq_exp(eq, map);
            replace_rel_exp(rel, map);
        }
    }
}

fn replace_rel_exp(rel_exp: &mut RelExp, map: &SymbolTable) {
    match rel_exp {
        RelExp::Add(add) => replace_add_exp(add, map),
        RelExp::Rel { rel, add, .. } => {
            replace_rel_exp(rel, map);
            replace_add_exp(add, map);
        }
    }
}

fn replace_add_exp(add_exp: &mut AddExp, map: &SymbolTable) {
    match add_exp {
        AddExp::Mul(mul) => replace_mul_exp(mul, map),
        AddExp::Add { add, mul, .. } => {
            replace_add_exp(add, map);
            replace_mul_exp(mul, map);
        }
    }
}

fn replace_mul_exp(mul_exp: &mut MulExp, map: &SymbolTable) {
    match mul_exp {
        MulExp::Unary(unary) => replace_unary_exp(unary, map),
        MulExp::Mul { mul, unary, .. } => {
            replace_mul_exp(mul, map);
            replace_unary_exp(unary, map);
        }
    }
}

fn replace_unary_exp(unary_exp: &mut UnaryExp, map: &SymbolTable) {
    match unary_exp {
        UnaryExp::PrimaryExp(primary_exp) => replace_primary_exp(primary_exp, map),
        UnaryExp::Unary { exp, .. } => {
            replace_unary_exp(exp, map);
        }
    }
}

fn replace_primary_exp(primary_exp: &mut PrimaryExp, map: &SymbolTable) {
    match primary_exp {
        PrimaryExp::Exp(exp) => replace_exp(exp, map),
        PrimaryExp::LVal(lval) => {
            if let Some(value) = map.get(&lval.ident) {
                match value {
                    Symbol::Const(num) => {
                        *primary_exp = PrimaryExp::Number(*num);
                    }
                    Symbol::Var(unique_ident) => {
                        lval.ident = unique_ident.clone();
                    }
                }
            }
        }
        PrimaryExp::Number(_) => {}
    }
}

fn eval_const_decl(const_decl: &mut ConstDecl, map: &mut SymbolTable) {
    for const_def in &mut const_decl.const_defs {
        eval_const_def(const_def, map)
    }
}

fn eval_const_def(const_def: &mut ConstDef, map: &mut SymbolTable) {
    let ident = &const_def.ident;
    let const_init_val = &const_def.const_init_val;
    if map.contains_key_in_current_scope(ident) {
        panic!("The const value {ident} is already exist");
    } else {
        let val = eval_const_init_val(const_init_val, map);
        map.insert(ident.to_string(), Symbol::Const(val));
    }
}

fn eval_const_init_val(const_init_val: &ConstInitVal, map: &mut SymbolTable) -> i32 {
    eval_const_exp(&const_init_val.const_exp, map)
}

fn eval_const_exp(const_exp: &ConstExp, map: &mut SymbolTable) -> i32 {
    eval_exp(&const_exp.exp, map)
}

fn eval_exp(exp: &Exp, map: &mut SymbolTable) -> i32 {
    eval_lor_exp(&exp.lor_exp, map)
}

fn eval_lor_exp(lor_exp: &LOrExp, map: &mut SymbolTable) -> i32 {
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

fn eval_land_exp(land_exp: &LAndExp, map: &mut SymbolTable) -> i32 {
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

fn eval_eq_exp(eq_exp: &EqExp, map: &mut SymbolTable) -> i32 {
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

fn eval_rel_exp(rel_exp: &RelExp, map: &mut SymbolTable) -> i32 {
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

fn eval_add_exp(add_exp: &AddExp, map: &mut SymbolTable) -> i32 {
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

fn eval_mul_exp(mul_exp: &MulExp, map: &mut SymbolTable) -> i32 {
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

fn eval_unary_exp(unary_exp: &UnaryExp, map: &mut SymbolTable) -> i32 {
    match unary_exp {
        UnaryExp::PrimaryExp(primary_exp) => eval_primary_exp(primary_exp, map),
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

fn eval_primary_exp(primary_exp: &PrimaryExp, map: &mut SymbolTable) -> i32 {
    match primary_exp {
        PrimaryExp::Exp(exp) => eval_exp(exp, map),
        PrimaryExp::LVal(lval) => {
            let symbol = map
                .get(&lval.ident)
                .unwrap_or_else(|| panic!("Undefined constant: {}", lval.ident));
            match symbol {
                Symbol::Const(num) => *num,
                Symbol::Var(_) => {
                    panic!("Expected constant, found variable: {}", lval.ident);
                }
            }
        }
        PrimaryExp::Number(num) => *num,
    }
}
