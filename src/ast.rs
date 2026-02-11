#[derive(Debug)]
pub struct CompUnit {
    pub func_def: FuncDef,
}

#[derive(Debug)]
#[allow(dead_code)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub block: Block,
}

#[derive(Debug)]
pub enum FuncType {
    Int,
}

#[derive(Debug)]
pub struct Block {
    pub stmt: Stmt,
}

#[derive(Debug)]
pub struct Stmt {
    pub exp: Exp,
}

#[derive(Debug)]
pub struct Exp {
    pub lor_exp: LOrExp,
}

#[derive(Debug)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
}

#[derive(Debug)]
pub enum UnaryExp {
    PrimaryExp(Box<PrimaryExp>),
    Unary { op: UnaryOp, exp: Box<UnaryExp> },
}

#[derive(Debug)]
pub enum PrimaryExp {
    Exp(Box<Exp>),
    Number(i32),
}

#[derive(Debug)]
pub enum AddExp {
    Mul(MulExp),
    Add {
        add: Box<AddExp>,
        op: AddOp,
        mul: MulExp,
    },
}

#[derive(Debug)]
pub enum MulExp {
    Unary(UnaryExp),
    Mul {
        mul: Box<MulExp>,
        op: MulOp,
        unary: UnaryExp,
    },
}

#[derive(Debug)]
pub enum AddOp {
    Plus,
    Minus,
}

#[derive(Debug)]
pub enum MulOp {
    Mul,
    Div,
    Mod,
}

#[derive(Debug)]
pub enum LOrExp {
    LAnd(LAndExp),
    LOr { lor: Box<LOrExp>, land: LAndExp },
}

#[derive(Debug)]
pub enum LAndExp {
    Eq(EqExp),
    LAnd { land: Box<LAndExp>, eq: EqExp },
}

#[derive(Debug)]
pub enum EqExp {
    Rel(RelExp),
    Eq { eq: Box<EqExp>, op: EqOp, rel: RelExp },
}

#[derive(Debug)]
pub enum RelExp {
    Add(AddExp),
    Rel { rel: Box<RelExp>, op: RelOp, add: AddExp },
}

#[derive(Debug)]
pub enum RelOp {
    Lt,
    Gt,
    Le,
    Ge,
}

#[derive(Debug)]
pub enum EqOp {
    Eq,
    Neq,
}