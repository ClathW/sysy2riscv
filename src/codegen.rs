use koopa::ir::{values::*, *};
use std::collections::{HashMap, HashSet};

const IMM12_MIN: i32 = -2048;
const IMM12_MAX: i32 = 2047;

struct FunctionLayoutInfo {
    stack_size: i32,
    slot_offsets: HashMap<Value, i32>,
    bb_labels: HashMap<BasicBlock, String>,
    referenced_bbs: HashSet<BasicBlock>,
}

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

            buf.push_str(&format!(".globl {name}\n{name}:\n"));
            buf.push_str(&funcdata.generate());
        }
        buf
    }
}

impl GenerateAsm for FunctionData {
    fn generate(&self) -> String {
        let mut buf = String::new();
        let info = build_layout_info(self);

        emit_adjust_sp(&mut buf, -info.stack_size);

        for (idx, (bb, node)) in self.layout().bbs().iter().enumerate() {
            if (idx != 0 || info.referenced_bbs.contains(bb)) && node.insts().front_key().is_some()
            {
                let label = info.bb_labels.get(bb).expect("basic block label not found");
                buf.push_str(&format!("{label}:\n"));
            }
            for &inst in node.insts().keys() {
                if let Some(asm) = inst_to_asm(
                    self,
                    inst,
                    &info.slot_offsets,
                    &info.bb_labels,
                    info.stack_size,
                ) {
                    buf.push_str(&asm);
                }
            }
        }
        buf
    }
}

fn build_layout_info(func: &FunctionData) -> FunctionLayoutInfo {
    let mut slot_offsets = HashMap::new();
    let mut current_offset = 0i32;

    for (_, node) in func.layout().bbs() {
        for &inst in node.insts().keys() {
            let value_data = func.dfg().value(inst);
            if !value_data.ty().is_unit() {
                slot_offsets.insert(inst, current_offset);
                current_offset += 4;
            }
        }
    }

    let mut bb_labels = HashMap::new();
    let mut referenced_bbs = HashSet::new();
    let func_name = func.name().trim_start_matches(['@', '%']);
    for (idx, (&bb, _)) in func.layout().bbs().iter().enumerate() {
        bb_labels.insert(bb, format!(".L{func_name}_bb{idx}"));
    }

    for (_, node) in func.layout().bbs() {
        for &inst in node.insts().keys() {
            match func.dfg().value(inst).kind() {
                ValueKind::Jump(jump) => {
                    referenced_bbs.insert(jump.target());
                }
                ValueKind::Branch(branch) => {
                    referenced_bbs.insert(branch.true_bb());
                    referenced_bbs.insert(branch.false_bb());
                }
                _ => {}
            }
        }
    }

    FunctionLayoutInfo {
        stack_size: align16(current_offset),
        slot_offsets,
        bb_labels,
        referenced_bbs,
    }
}

fn inst_to_asm(
    func: &FunctionData,
    inst: Value,
    slot_offsets: &HashMap<Value, i32>,
    bb_labels: &HashMap<BasicBlock, String>,
    stack_size: i32,
) -> Option<String> {
    let value = func.dfg().value(inst);

    match value.kind() {
        ValueKind::Alloc(_) => None,
        ValueKind::Binary(bin) => gen_binary(func, bin, inst, slot_offsets),
        ValueKind::Branch(branch) => gen_branch(func, branch, slot_offsets, bb_labels),
        ValueKind::Jump(jump) => gen_jump(jump, bb_labels),
        ValueKind::Load(load) => gen_load(load, inst, slot_offsets),
        ValueKind::Return(ret) => gen_return(func, ret, slot_offsets, stack_size),
        ValueKind::Store(store) => gen_store(func, store, slot_offsets),
        _ => None,
    }
}

fn gen_branch(
    func: &FunctionData,
    branch: &Branch,
    slot_offsets: &HashMap<Value, i32>,
    bb_labels: &HashMap<BasicBlock, String>,
) -> Option<String> {
    let mut buf = String::new();
    emit_value_to_reg(func, branch.cond(), "t0", slot_offsets, &mut buf);
    let true_label = bb_labels
        .get(&branch.true_bb())
        .expect("branch: true target label not found");
    let false_label = bb_labels
        .get(&branch.false_bb())
        .expect("branch: false target label not found");
    buf.push_str(&format!("bnez t0, {true_label}\n"));
    buf.push_str(&format!("j {false_label}\n"));
    Some(buf)
}

fn gen_jump(jump: &Jump, bb_labels: &HashMap<BasicBlock, String>) -> Option<String> {
    let mut buf = String::new();
    let jump_label = bb_labels
        .get(&jump.target())
        .expect("jump: target label not found");
    buf.push_str(&format!("j {jump_label}\n"));
    Some(buf)
}

fn gen_load(load: &Load, inst: Value, slot_offsets: &HashMap<Value, i32>) -> Option<String> {
    let ptr = load.src();
    let ptr_offset = slot_offsets
        .get(&ptr)
        .expect("load: alloc not found in offset map");
    let dst_offset = slot_offsets
        .get(&inst)
        .expect("load: result slot not found");

    let mut buf = String::new();
    emit_lw_from_stack("t0", *ptr_offset, &mut buf);
    emit_sw_to_stack("t0", *dst_offset, &mut buf);
    Some(buf)
}

fn gen_store(
    func: &FunctionData,
    store: &Store,
    slot_offsets: &HashMap<Value, i32>,
) -> Option<String> {
    let ptr = store.dest();
    let offset = slot_offsets
        .get(&ptr)
        .expect("store: alloc not found in offset map");

    let mut buf = String::new();
    emit_value_to_reg(func, store.value(), "t0", slot_offsets, &mut buf);
    emit_sw_to_stack("t0", *offset, &mut buf);
    Some(buf)
}

fn gen_return(
    func: &FunctionData,
    ret: &Return,
    slot_offsets: &HashMap<Value, i32>,
    stack_size: i32,
) -> Option<String> {
    let mut buf = String::new();

    if let Some(val) = ret.value() {
        emit_value_to_reg(func, val, "a0", slot_offsets, &mut buf);
    }
    emit_adjust_sp(&mut buf, stack_size);
    buf.push_str("ret\n");
    Some(buf)
}

fn gen_binary(
    func: &FunctionData,
    bin: &Binary,
    inst: Value,
    slot_offsets: &HashMap<Value, i32>,
) -> Option<String> {
    let mut buf = String::new();
    emit_value_to_reg(func, bin.lhs(), "t0", slot_offsets, &mut buf);
    emit_value_to_reg(func, bin.rhs(), "t1", slot_offsets, &mut buf);

    match bin.op() {
        BinaryOp::Add => {
            buf.push_str("add t2, t0, t1\n");
        }
        BinaryOp::And => {
            buf.push_str("and t2, t0, t1\n");
        }
        BinaryOp::Div => {
            buf.push_str("div t2, t0, t1\n");
        }
        BinaryOp::Eq => {
            buf.push_str("xor t2, t0, t1\n");
            buf.push_str("seqz t2, t2\n");
        }
        BinaryOp::Ge => {
            buf.push_str("slt t2, t0, t1\n");
            buf.push_str("xori t2, t2, 1\n");
        }
        BinaryOp::Gt => {
            buf.push_str("slt t2, t1, t0\n");
        }
        BinaryOp::Le => {
            buf.push_str("slt t2, t1, t0\n");
            buf.push_str("xori t2, t2, 1\n");
        }
        BinaryOp::Lt => {
            buf.push_str("slt t2, t0, t1\n");
        }
        BinaryOp::Mul => {
            buf.push_str("mul t2, t0, t1\n");
        }
        BinaryOp::Mod => {
            buf.push_str("rem t2, t0, t1\n");
        }
        BinaryOp::NotEq => {
            buf.push_str("xor t2, t0, t1\n");
            buf.push_str("snez t2, t2\n");
        }
        BinaryOp::Or => {
            buf.push_str("or t2, t0, t1\n");
            buf.push_str("snez t2, t2\n");
        }
        BinaryOp::Sub => {
            buf.push_str("sub t2, t0, t1\n");
        }
        _ => return None,
    }

    let result_offset = slot_offsets
        .get(&inst)
        .expect("binary: result slot not found");
    emit_sw_to_stack("t2", *result_offset, &mut buf);

    Some(buf)
}

fn emit_value_to_reg(
    func: &FunctionData,
    val: Value,
    reg: &str,
    slot_offsets: &HashMap<Value, i32>,
    buf: &mut String,
) {
    match func.dfg().value(val).kind() {
        ValueKind::Integer(int) => {
            buf.push_str(&format!("li {reg}, {}\n", int.value()));
        }
        _ => {
            let offset = slot_offsets
                .get(&val)
                .expect("value slot not found for non-integer value");
            emit_lw_from_stack(reg, *offset, buf);
        }
    }
}

fn emit_lw_from_stack(rd: &str, offset: i32, buf: &mut String) {
    if fits_imm12(offset) {
        buf.push_str(&format!("lw {rd}, {offset}(sp)\n"));
    } else {
        buf.push_str(&format!("li t3, {offset}\n"));
        buf.push_str("add t3, sp, t3\n");
        buf.push_str(&format!("lw {rd}, 0(t3)\n"));
    }
}

fn emit_sw_to_stack(rs: &str, offset: i32, buf: &mut String) {
    if fits_imm12(offset) {
        buf.push_str(&format!("sw {rs}, {offset}(sp)\n"));
    } else {
        buf.push_str(&format!("li t3, {offset}\n"));
        buf.push_str("add t3, sp, t3\n");
        buf.push_str(&format!("sw {rs}, 0(t3)\n"));
    }
}

fn emit_adjust_sp(buf: &mut String, delta: i32) {
    if delta == 0 {
        return;
    }
    if fits_imm12(delta) {
        buf.push_str(&format!("addi sp, sp, {delta}\n"));
    } else {
        buf.push_str(&format!("li t0, {delta}\n"));
        buf.push_str("add sp, sp, t0\n");
    }
}

fn align16(size: i32) -> i32 {
    ((size + 15) / 16) * 16
}

fn fits_imm12(imm: i32) -> bool {
    (IMM12_MIN..=IMM12_MAX).contains(&imm)
}
