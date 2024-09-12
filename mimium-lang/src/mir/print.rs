use crate::{compiler::bytecodegen::ByteCodeGenerator, format_vec};

use super::*;

fn display_state_sizes<T: AsRef<[StateSize]>>(x: T) -> String {
    let x_ref = x.as_ref();

    // it's a bit weird to use a function defined for bytecode to display an
    // MIR, but this is for the sake of debugging.
    let x_calculated = ByteCodeGenerator::calc_state_size(x_ref);

    let x_str = x_ref
        .iter()
        .map(|x| {
            if x.size == 1 {
                x.ty.to_type().to_string()
            } else {
                format!("({}*{})", x.ty.to_type(), x.size)
            }
        })
        .collect::<Vec<String>>();

    format!("{x_calculated}(=[{}])", x_str.join(","))
}

impl std::fmt::Display for Mir {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for fun in self.functions.iter() {
            let af = fun
                .args
                .iter()
                .zip(fun.argtypes.clone())
                .map(|(a, t)| format!("{}:{}", *a, t.to_type()))
                .collect::<Vec<_>>()
                .join(",");
            let _ = writeln!(f, "fn {} [{af}]", fun.label);
            let upi = format_vec!(fun.upindexes, ",");
            let _ = write!(f, "upindexes:[{upi}]");
            if let Some(upper_i) = fun.upperfn_i {
                let _ = write!(f, "upper:{upper_i}");
            }
            let _ = write!(f, " state_size: {}", display_state_sizes(&fun.state_sizes));
            for (i, block) in fun.body.iter().enumerate() {
                let _ = write!(f, "\n  block {i}\n");
                for (v, insts) in block.0.iter() {
                    let _ = match v.as_ref() {
                        Value::None => writeln!(f, "  {: <10} {insts}", " "),
                        _ => writeln!(f, "  {:<7} := {insts}", format!("{}", *v)),
                    };
                }
            }
        }
        Ok(())
    }
}

impl std::fmt::Display for Argument {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Argument(label, t) = self;
        write!(f, "arg {}: {}", label.0, t.to_type())
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Global(gv) => write!(f, "global({})", *gv),
            Value::Argument(_, v) => write!(f, "{}", v.0),
            Value::Register(r) => write!(f, "reg({r})"),
            Value::Function(id) => write!(f, "function {id}"),
            Value::ExtFunction(label, t) => write!(f, "extfun {label} {}", t.to_type()),
            Value::State(v) => write!(f, "state({})", *v),
            Value::None => write!(f, "none"),
        }
    }
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Uinteger(u) => write!(f, "uint {u}"),
            Instruction::Integer(i) => write!(f, "int {i}"),
            Instruction::Float(n) => write!(f, "float {n}"),
            Instruction::Alloc(t) => write!(f, "alloc {}", t.to_type()),
            Instruction::Load(src, ty) => write!(f, "load {src}, {}", ty.to_type()),
            Instruction::Store(dst, src, ty) => write!(f, "store {dst}, {src}, {}", ty.to_type()),
            Instruction::GetElement {
                value,
                ty,
                array_idx,
                tuple_offset,
            } => {
                let ty = ty.to_type();
                write!(f, "getelement {value}, {ty}, {tuple_offset}[{array_idx}]")
            }
            Instruction::Call(fptr, args, rty) => {
                write!(
                    f,
                    "call {} [{}] ->{}",
                    *fptr,
                    args.iter()
                        .map(|(a, _t)| a.to_string())
                        .collect::<Vec<_>>()
                        .join(","),
                    rty.to_type()
                )
            }
            Instruction::CallCls(cls, args, rty) => {
                write!(
                    f,
                    "callcls {} [{}] ->{}",
                    *cls,
                    args.iter()
                        .map(|(a, _t)| a.to_string())
                        .collect::<Vec<_>>()
                        .join(","),
                    rty.to_type()
                )
            }
            Instruction::Closure(fun) => {
                if let Value::Function(idx) = fun.as_ref() {
                    // TODO: convert idx to function in order to show more information
                    write!(f, "closure {idx}")
                } else {
                    write!(f, "closure {}", *fun)
                }
            }
            Instruction::ClosureRec => {
                write!(f, "closurerec")
            }
            Instruction::CloseUpValues(cls, ty) => {
                write!(f, "close {} {}", *cls, ty.to_type())
            }
            Instruction::GetUpValue(idx, ty) => write!(f, "getupval {idx} {}", ty.to_type()),
            Instruction::SetUpValue(dst, src, ty) => {
                write!(f, "setupval {dst} {} {}", src, ty.to_type())
            }
            Instruction::GetGlobal(v, ty) => write!(f, "getglobal {} {}", *v, ty.to_type()),
            Instruction::SetGlobal(dst, src, ty) => {
                write!(f, "setglobal {} {} {}", *dst, *src, ty.to_type())
            }
            Instruction::PushStateOffset(v) => {
                write!(f, "pushstateidx {}", display_state_sizes(v))
            }
            Instruction::PopStateOffset(v) => {
                write!(f, "popstateidx  {}", display_state_sizes(v))
            }

            Instruction::GetState(ty) => write!(f, "getstate {}", ty.to_type()),
            Instruction::JmpIf(cond, tbb, ebb) => write!(f, "jmpif {cond} {tbb} {ebb}"),
            Instruction::Jmp(bb) => write!(f, "jmp {bb}"),
            Instruction::Phi(t, e) => write!(f, "phi {t} {e}"),
            Instruction::Return(a, rty) => write!(f, "ret {} {}", *a, rty.to_type()),
            Instruction::ReturnFeed(v, rty) => write!(f, "retfeed {} {}", *v, rty.to_type()),
            Instruction::Delay(max, a, b) => write!(f, "delay {max} {} {}", *a, *b),
            Instruction::Mem(a) => write!(f, "mem {}", *a),
            Instruction::AddF(a, b) => write!(f, "addf {} {}", *a, *b),
            Instruction::SubF(a, b) => write!(f, "subf {} {}", *a, *b),
            Instruction::MulF(a, b) => write!(f, "mulf {} {}", *a, *b),
            Instruction::DivF(a, b) => write!(f, "divf {} {}", *a, *b),
            Instruction::ModF(a, b) => write!(f, "modf {} {}", *a, *b),
            Instruction::NegF(a) => write!(f, "negf {}", *a),
            Instruction::AbsF(a) => write!(f, "absf {}", *a),
            Instruction::SinF(a) => write!(f, "sinf {}", *a),
            Instruction::CosF(a) => write!(f, "sinf {}", *a),
            Instruction::SqrtF(a) => write!(f, "sqrtf {}", *a),
            Instruction::PowF(a, b) => write!(f, "powf {} {}", *a, *b),
            Instruction::LogF(a, b) => write!(f, "logf {} {}", *a, *b),
            Instruction::AddI(a, b) => write!(f, "addi {} {}", *a, *b),
            Instruction::SubI(a, b) => write!(f, "subi {} {}", *a, *b),
            Instruction::MulI(a, b) => write!(f, "muli {} {}", *a, *b),
            Instruction::DivI(a, b) => write!(f, "divi {} {}", *a, *b),
            Instruction::ModI(a, b) => write!(f, "modi {} {}", *a, *b),
            Instruction::NegI(a) => write!(f, "negi {}", *a),
            Instruction::AbsI(a) => write!(f, "absi {}", *a),
            Instruction::PowI(a) => write!(f, "powi {}", *a),
            Instruction::LogI(_, _) => todo!(),
            Instruction::Not(_) => todo!(),
            Instruction::Eq(a, b) => write!(f, "eq {} {}", *a, *b),
            Instruction::Ne(a, b) => write!(f, "ne {} {}", *a, *b),
            Instruction::Gt(a, b) => write!(f, "gt {} {}", *a, *b),
            Instruction::Ge(a, b) => write!(f, "ge {} {}", *a, *b),
            Instruction::Lt(a, b) => write!(f, "lt {} {}", *a, *b),
            Instruction::Le(a, b) => write!(f, "le {} {}", *a, *b),
            Instruction::And(a, b) => write!(f, "and {} {}", *a, *b),
            Instruction::Or(a, b) => write!(f, "or {} {}", *a, *b),
            Instruction::CastFtoI(_) => todo!(),
            Instruction::CastItoF(_) => todo!(),
            Instruction::CastItoB(_) => todo!(),
        }
    }
}
