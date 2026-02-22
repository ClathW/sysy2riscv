use koopa::ir::{values::*, *};
use std::collections::HashMap;

enum LoweredValue {
    Integer(i32),
    Else(Value),
}

// 根据内存形式 Koopa IR 生成汇编
pub trait GenerateAsm {
    fn generate(&self) -> String;
}

impl GenerateAsm for Program {
    fn generate(&self) -> String {
        let mut buf = String::new();
        buf.push_str(".text\n");
        for &func in self.func_layout() {
            let funcdata = self.func(func);
            let raw = funcdata.name();
            let name = raw.trim_start_matches(['@', '%']);

            buf.push_str(&format!(".global {name}\n{name}:\n"));
            buf.push_str(&funcdata.generate());
        }
        buf
    }
}

impl GenerateAsm for FunctionData {
    fn generate(&self) -> String {
        let mut buf = String::new();
        let mut value_regs: HashMap<Value, i32> = HashMap::new();
        let alloc_offsets = build_alloc_offsets(self);
        let stack_size = stack_size(self);
        if stack_size > 0 {
            buf.push_str(&format!("addi sp, sp, -{stack_size}\n"));
        }
        for (_, node) in self.layout().bbs() {
            let mut num = 0;
            for &inst in node.insts().keys() {
                if let Some(asm) = inst_to_asm(
                    self,
                    inst,
                    &mut num,
                    &mut value_regs,
                    &alloc_offsets,
                    stack_size,
                ) {
                    buf.push_str(&asm);
                }
            }
        }
        buf
    }
}

fn build_alloc_offsets(func: &FunctionData) -> HashMap<Value, i32> {
    let mut map = HashMap::new();
    let mut offset = 0i32;
    for (_, node) in func.layout().bbs() {
        for &inst in node.insts().keys() {
            let value_data = func.dfg().value(inst);
            if matches!(value_data.kind(), ValueKind::Alloc(_)) {
                map.insert(inst, offset);
                offset += 4;
            }
        }
    }
    map
}

fn stack_size(func: &FunctionData) -> i32 {
    let mut count = 0i32;
    for (_, node) in func.layout().bbs() {
        for &inst in node.insts().keys() {
            let value_data = func.dfg().value(inst);
            if matches!(value_data.kind(), ValueKind::Alloc(_)) {
                count += 1;
            }
        }
    }
    ((count * 4 + 15) / 16) * 16
}

fn inst_to_asm(
    func: &FunctionData,
    inst: Value,
    dest: &mut i32,
    value_regs: &mut HashMap<Value, i32>,
    alloc_offsets: &HashMap<Value, i32>,
    stack_size: i32,
) -> Option<String> {
    let value = func.dfg().value(inst);

    match value.kind() {
        ValueKind::Alloc(_) => None, 
        ValueKind::Load(load) => gen_load(func, load, inst, dest, value_regs, alloc_offsets),
        ValueKind::Store(store) => gen_store(func, store, dest, value_regs, alloc_offsets),
        ValueKind::Binary(bin) => gen_binary(func, bin, inst, dest, value_regs),
        ValueKind::Return(ret) => gen_return(func, ret, dest, value_regs, stack_size),
        _ => None,
    }
}

fn gen_load(
    _func: &FunctionData,
    load: &Load,
    inst: Value,
    dest: &mut i32,
    value_regs: &mut HashMap<Value, i32>,
    alloc_offsets: &HashMap<Value, i32>,
) -> Option<String> {
    let ptr = load.src();
    let offset = alloc_offsets
        .get(&ptr)
        .expect("load: alloc not found in offset map");
    let reg_num = *dest % 7;
    let reg = format!("t{}", reg_num);
    value_regs.insert(inst, reg_num);
    *dest += 1;
    Some(format!("lw {}, {}(sp)\n", reg, offset))
}

fn gen_store(
    func: &FunctionData,
    store: &Store,
    dest: &mut i32,
    value_regs: &HashMap<Value, i32>,
    alloc_offsets: &HashMap<Value, i32>,
) -> Option<String> {
    let val = store.value();
    let ptr = store.dest();
    let offset = alloc_offsets
        .get(&ptr)
        .expect("store: alloc not found in offset map");

    let mut buf = String::new();
    let src_reg = match lower_value(func, val) {
        LoweredValue::Integer(int) => {
            let reg = format!("t{}", *dest % 7);
            buf.push_str(&format!("li {}, {}\n", reg, int));
            *dest += 1;
            reg
        }
        LoweredValue::Else(v) => {
            let reg_num = value_regs
                .get(&v)
                .expect("store: source value not in register map");
            format!("t{}", reg_num)
        }
    };
    buf.push_str(&format!("sw {}, {}(sp)\n", src_reg, offset));
    Some(buf)
}

fn gen_return(
    func: &FunctionData,
    ret: &Return,
    dest: &mut i32,
    value_regs: &HashMap<Value, i32>,
    stack_size: i32,
) -> Option<String> {
    let mut buf = String::new();

    if let Some(val) = ret.value() {
        match lower_value(func, val) {
            LoweredValue::Integer(int) => {
                buf.push_str(&format!("li a0, {int}\n"));
            }
            LoweredValue::Else(v) => {
                let reg_num = value_regs
                    .get(&v)
                    .copied()
                    .unwrap_or_else(|| (*dest - 1).rem_euclid(7));
                buf.push_str(&format!("mv a0, t{}\n", reg_num));
            }
        }
    }
    if stack_size > 0 {
        buf.push_str(&format!("addi sp, sp, {stack_size}\n"));
    }
    buf.push_str("ret\n");
    Some(buf)
}

fn gen_binary(
    func: &FunctionData,
    bin: &Binary,
    inst: Value,
    dest: &mut i32,
    value_regs: &mut HashMap<Value, i32>,
) -> Option<String> {
    let mut buf = String::new();

    let lhs = lower_value(func, bin.lhs());
    let rhs = lower_value(func, bin.rhs());

    let lhs_reg = get_operand_reg(lhs, dest, value_regs, &mut buf);
    let rhs_reg = get_operand_reg(rhs, dest, value_regs, &mut buf);
    let result_reg = format!("t{}", (*dest - 1).rem_euclid(7));

    // 记录当前指令的结果寄存器
    value_regs.insert(inst, (*dest - 1).rem_euclid(7));

    match bin.op() {
        BinaryOp::Add => {
            buf.push_str(&format!("add {}, {}, {}\n", result_reg, lhs_reg, rhs_reg));
        }
        BinaryOp::And => {
            buf.push_str(&format!("and {}, {}, {}\n", result_reg, lhs_reg, rhs_reg));
        }
        BinaryOp::Div => {
            buf.push_str(&format!("div {}, {}, {}\n", result_reg, lhs_reg, rhs_reg));
        }
        BinaryOp::Eq => {
            buf.push_str(&format!("xor {}, {}, {}\n", result_reg, lhs_reg, rhs_reg));
            buf.push_str(&format!("seqz {}, {}\n", result_reg, result_reg));
        }
        BinaryOp::Ge => {
            buf.push_str(&format!("slt {}, {}, {}\n", result_reg, lhs_reg, rhs_reg));
            buf.push_str(&format!("xori {}, {}, 1\n", result_reg, result_reg));
        }
        BinaryOp::Gt => {
            buf.push_str(&format!("slt {}, {}, {}\n", result_reg, rhs_reg, lhs_reg));
        }
        BinaryOp::Le => {
            buf.push_str(&format!("slt {}, {}, {}\n", result_reg, rhs_reg, lhs_reg));
            buf.push_str(&format!("xori {}, {}, 1\n", result_reg, result_reg));
        }
        BinaryOp::Lt => {
            buf.push_str(&format!("slt {}, {}, {}\n", result_reg, lhs_reg, rhs_reg));
        }
        BinaryOp::Mul => {
            buf.push_str(&format!("mul {}, {}, {}\n", result_reg, lhs_reg, rhs_reg));
        }
        BinaryOp::Mod => {
            buf.push_str(&format!("rem {}, {}, {}\n", result_reg, lhs_reg, rhs_reg));
        }
        BinaryOp::NotEq => {
            buf.push_str(&format!("xor {}, {}, {}\n", result_reg, lhs_reg, rhs_reg));
            buf.push_str(&format!("snez {}, {}\n", result_reg, result_reg));
        }
        BinaryOp::Or => {
            buf.push_str(&format!("or {}, {}, {}\n", result_reg, lhs_reg, rhs_reg));
            buf.push_str(&format!("snez {}, {}\n", result_reg, result_reg));
        }
        BinaryOp::Sub => {
            buf.push_str(&format!("sub {}, {}, {}\n", result_reg, lhs_reg, rhs_reg));
        }
        _ => return None,
    }

    Some(buf)
}

fn get_operand_reg(
    val: LoweredValue,
    dest: &mut i32,
    value_regs: &HashMap<Value, i32>,
    buf: &mut String,
) -> String {
    match val {
        LoweredValue::Integer(int) => {
            let reg = format!("t{}", *dest % 7);
            buf.push_str(&format!("li {}, {}\n", reg, int));
            *dest += 1;
            reg
        }
        LoweredValue::Else(v) => {
            let reg_num = value_regs.get(&v).expect("Value not found in register map");
            format!("t{}", reg_num)
        }
    }
}

fn lower_value(func: &FunctionData, val: Value) -> LoweredValue {
    let value_data = func.dfg().value(val);

    match value_data.kind() {
        ValueKind::Integer(int) => LoweredValue::Integer(int.value()),
        _ => LoweredValue::Else(val),
    }
}
