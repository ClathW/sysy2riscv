use koopa::ir::{values::*, *};

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
            for &inst in node.insts().keys() {
                if let Some(asm) = inst_to_asm(self, inst) {
                    buf.push_str(&asm);
                }
            }
        }
        buf
    }
}

fn inst_to_asm(func: &FunctionData, inst: Value) -> Option<String> {
    let value = func.dfg().value(inst);

    match value.kind() {
        ValueKind::Integer(int) => gen_interger(int),
        ValueKind::Return(ret) => gen_return(func, ret),
        _ => None,
    }
}

fn gen_return(func: &FunctionData, ret: &Return) -> Option<String> {
    let mut buf = String::new();

    if let Some(val) = ret.value() {
        let src = lower_value(func, val)?;
        buf.push_str(&format!("li a0, {}\n", src));
    }

    buf.push_str("ret\n");
    Some(buf)
}

fn gen_interger(int: &Integer) -> Option<String> {
    Some(format!("{}", int.value()))
}

fn lower_value(func: &FunctionData, val: Value) -> Option<String> {
    let val = func.dfg().value(val);

    match val.kind() {
        ValueKind::Integer(int) => gen_interger(int),
        _ => None,
    }
}
