use koopa::ir::{values::*, *};

#[allow(dead_code)]
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
        for (_, node) in self.layout().bbs() {
            let mut num = 0;
            for &inst in node.insts().keys() {
                if let Some(asm) = inst_to_asm(self, inst, &mut num) {
                    buf.push_str(&asm);
                }
                num += 1;
            }
        }
        buf
    }
}

fn inst_to_asm(func: &FunctionData, inst: Value, dest: &mut i32) -> Option<String> {
    let value = func.dfg().value(inst);

    match value.kind() {
        ValueKind::Binary(bin) => gen_binary(func, bin, dest),
        ValueKind::Return(ret) => gen_return(func, ret, dest),
        _ => None,
    }
}

fn gen_return(func: &FunctionData, ret: &Return, dest: &mut i32) -> Option<String> {
    let mut buf = String::new();

    if let Some(val) = ret.value() {
        let val = lower_value(func, val);
        match val {
            LoweredValue::Integer(int) => {
                buf.push_str(&format!("li a0, {int}\n"));
            }
            LoweredValue::Else(_) => {
                buf.push_str(&format!("mv a0, t{}\n", *dest - 1));
            }
        }
    }
    buf.push_str("ret\n");
    Some(buf)
}

fn gen_binary(func: &FunctionData, bin: &Binary, dest: &mut i32) -> Option<String> {
    let mut buf = String::new();

    let lhs = lower_value(func, bin.lhs());
    let rhs = lower_value(func, bin.rhs());

    match bin.op() {
        BinaryOp::Add => {
            let lhs = match lhs {
                LoweredValue::Integer(int) => {
                    buf.push_str(&format!("li t{dest}, {int}\n"));
                    &format!("t{dest}")
                }
                _ => &format!("t{}", *dest - 1),
            };
            *dest += 1;
            let rhs = match rhs {
                LoweredValue::Integer(int) => {
                    buf.push_str(&format!("li t{dest}, {int}\n"));
                    &format!("t{dest}")
                }
                _ => &format!("t{}", *dest - 2),
            };
            buf.push_str(&format!("add t{dest}, {lhs}, {rhs}\n"));
        }
        BinaryOp::Div => {
            let lhs = match lhs {
                LoweredValue::Integer(int) => {
                    buf.push_str(&format!("li t{dest}, {int}\n"));
                    &format!("t{dest}")
                }
                _ => &format!("t{}", *dest - 1),
            };
            *dest += 1;
            let rhs = match rhs {
                LoweredValue::Integer(int) => {
                    buf.push_str(&format!("li t{dest}, {int}\n"));
                    &format!("t{dest}")
                }
                _ => &format!("t{}", *dest - 2),
            };
            buf.push_str(&format!("div t{dest}, {lhs}, {rhs}\n"));
        }
        BinaryOp::Eq => {
            let lhs = match lhs {
                LoweredValue::Integer(int) => &int.to_string(),
                _ => &format!("t{}", *dest - 1),
            };

            let rhs = match rhs {
                LoweredValue::Integer(int) => &int.to_string(),
                _ => &format!("t{}", *dest - 1),
            };
            buf.push_str(&format!("li t{dest}, {lhs}\n"));
            buf.push_str(&format!("xor t{dest}, t{dest}, {rhs}\n"));
            buf.push_str(&format!("seqz t{dest}, t{dest}\n"));
        }
        BinaryOp::Mul => {
            let lhs = match lhs {
                LoweredValue::Integer(int) => {
                    buf.push_str(&format!("li t{dest}, {int}\n"));
                    &format!("t{dest}")
                }
                _ => &format!("t{}", *dest - 1),
            };
            *dest += 1;
            let rhs = match rhs {
                LoweredValue::Integer(int) => {
                    buf.push_str(&format!("li t{dest}, {int}\n"));
                    &format!("t{dest}")
                }
                _ => &format!("t{}", *dest - 2),
            };
            buf.push_str(&format!("mul t{dest}, {lhs}, {rhs}\n"));
        }
        BinaryOp::Mod => {
            let lhs = match lhs {
                LoweredValue::Integer(int) => {
                    buf.push_str(&format!("li t{dest}, {int}\n"));
                    &format!("t{dest}")
                }
                _ => &format!("t{}", *dest - 1),
            };
            *dest += 1;
            let rhs = match rhs {
                LoweredValue::Integer(int) => {
                    buf.push_str(&format!("li t{dest}, {int}\n"));
                    &format!("t{dest}")
                }
                _ => &format!("t{}", *dest - 2),
            };
            buf.push_str(&format!("rem t{dest}, {lhs}, {rhs}\n"));
        }
        BinaryOp::Sub => {
            let lhs = match lhs {
                LoweredValue::Integer(int) => {
                    buf.push_str(&format!("li t{dest}, {int}\n"));
                    &format!("t{dest}")
                }
                _ => &format!("t{}", *dest - 1),
            };
            *dest += 1;
            let rhs = match rhs {
                LoweredValue::Integer(int) => {
                    buf.push_str(&format!("li t{dest}, {int}\n"));
                    &format!("t{dest}")
                }
                _ => &format!("t{}", *dest - 2),
            };
            buf.push_str(&format!("sub t{dest}, {lhs}, {rhs}\n"));
        }
        _ => return None,
    }

    Some(buf)
}

fn lower_value(func: &FunctionData, val: Value) -> LoweredValue {
    let value_data = func.dfg().value(val);

    match value_data.kind() {
        ValueKind::Integer(int) => LoweredValue::Integer(int.value()),
        _ => LoweredValue::Else(val),
    }
}
