use std::collections::BTreeSet;

use crate::mir::{BlockAnnotation, FunctionExecutionMode, Instruction, Mir, Value};

fn collect_direct_callees(mir: &Mir, function_index: usize) -> Vec<usize> {
    mir.functions
        .get(function_index)
        .map(|function| {
            let mut function_registers = std::collections::BTreeMap::new();
            function
                .body
                .iter()
                .flat_map(|block| block.0.iter())
                .filter_map(|(value, instruction)| match instruction {
                    Instruction::Uinteger(index) => match value.as_ref() {
                        Value::Register(register) => {
                            function_registers.insert(*register, *index as usize);
                            None
                        }
                        _ => None,
                    },
                    Instruction::Call(callee, _, _) => match callee.as_ref() {
                        Value::Function(index) => Some(*index),
                        Value::Register(register) => function_registers.get(register).copied(),
                        _ => None,
                    },
                    Instruction::CallIndirect(callee, _, _) => match callee.as_ref() {
                        Value::Function(index) => Some(*index),
                        Value::Register(register) => function_registers.get(register).copied(),
                        _ => None,
                    },
                    _ => None,
                })
                .collect()
        })
        .unwrap_or_default()
}

pub fn annotate_dsp_block_graph(mir: &mut Mir, requested_block_size: u32) {
    let Some(dsp_index) = mir
        .functions
        .iter()
        .position(|function| function.label.as_str() == "dsp")
    else {
        return;
    };

    let annotation = BlockAnnotation::new(requested_block_size);
    let mut visited = BTreeSet::new();
    let mut stack = vec![dsp_index];

    while let Some(function_index) = stack.pop() {
        if !visited.insert(function_index) {
            continue;
        }

        if let Some(function) = mir.functions.get_mut(function_index) {
            function.execution_mode = FunctionExecutionMode::Block(annotation);
        }

        collect_direct_callees(mir, function_index)
            .into_iter()
            .filter(|callee_index| !visited.contains(callee_index))
            .for_each(|callee_index| stack.push(callee_index));
    }

    lower_block_primitives(mir);
}

fn rewrite_block_instruction(instruction: &mut Instruction) {
    *instruction = match instruction.clone() {
        Instruction::AddF(lhs, rhs) => Instruction::VAddF(lhs, rhs),
        Instruction::SubF(lhs, rhs) => Instruction::VSubF(lhs, rhs),
        Instruction::MulF(lhs, rhs) => Instruction::VMulF(lhs, rhs),
        Instruction::DivF(lhs, rhs) => Instruction::VDivF(lhs, rhs),
        other => other,
    };
}

pub fn lower_block_primitives(mir: &mut Mir) {
    mir.functions
        .iter_mut()
        .filter(|function| matches!(function.execution_mode, FunctionExecutionMode::Block(_)))
        .for_each(|function| {
            function.body.iter_mut().for_each(|block| {
                block
                    .0
                    .iter_mut()
                    .for_each(|(_, instruction)| rewrite_block_instruction(instruction));
            });
        });
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use crate::{
        interner::{ToSymbol, TypeNodeId},
        mir::{Argument, Function, Instruction, Mir, Value},
        numeric,
    };

    use super::annotate_dsp_block_graph;

    fn numeric_type() -> TypeNodeId {
        numeric!()
    }

    fn arg(name: &str) -> Argument {
        Argument(name.to_symbol(), numeric_type())
    }

    #[test]
    fn annotate_dsp_reaches_direct_call_graph() {
        let mut dsp = Function::new(0, "dsp".to_symbol(), &[arg("input")], vec![], None);
        dsp.body[0].0.push((
            Arc::new(Value::Register(0)),
            Instruction::AddF(Arc::new(Value::Argument(0)), Arc::new(Value::Argument(0))),
        ));
        dsp.body[0].0.push((
            Arc::new(Value::Register(1)),
            Instruction::Call(
                Arc::new(Value::Function(1)),
                vec![(Arc::new(Value::Argument(0)), numeric_type())],
                numeric_type(),
            ),
        ));

        let mut helper = Function::new(1, "helper".to_symbol(), &[arg("x")], vec![], None);
        helper.body[0].0.push((
            Arc::new(Value::Register(1)),
            Instruction::MulF(Arc::new(Value::Argument(0)), Arc::new(Value::Argument(0))),
        ));
        helper.body[0].0.push((
            Arc::new(Value::Register(2)),
            Instruction::Call(
                Arc::new(Value::Function(2)),
                vec![(Arc::new(Value::Argument(0)), numeric_type())],
                numeric_type(),
            ),
        ));

        let leaf = Function::new(2, "leaf".to_symbol(), &[arg("x")], vec![], None);
        let unrelated = Function::new(3, "unrelated".to_symbol(), &[arg("x")], vec![], None);

        let mut mir = Mir::new(None);
        mir.functions = vec![dsp, helper, leaf, unrelated];

        annotate_dsp_block_graph(&mut mir, 256);

        assert_eq!(
            mir.get_dsp_function()
                .and_then(|function| function.block_annotation())
                .map(|annotation| annotation.requested_block_size),
            Some(256)
        );
        assert_eq!(
            mir.get_function("helper")
                .and_then(|function| function.block_annotation())
                .map(|annotation| annotation.requested_block_size),
            Some(256)
        );
        assert_eq!(
            mir.get_function("leaf")
                .and_then(|function| function.block_annotation())
                .map(|annotation| annotation.requested_block_size),
            Some(256)
        );
        assert_eq!(
            mir.get_function("unrelated")
                .and_then(|function| function.block_annotation()),
            None
        );
        assert!(matches!(
            &mir.get_dsp_function().unwrap().body[0].0[0].1,
            Instruction::VAddF(_, _)
        ));
        assert!(matches!(
            &mir.get_function("helper").unwrap().body[0].0[0].1,
            Instruction::VMulF(_, _)
        ));
        assert!(!matches!(
            &mir.get_function("unrelated").unwrap().body[0].0.first().map(|(_, inst)| inst),
            Some(Instruction::VAddF(_, _) | Instruction::VSubF(_, _) | Instruction::VMulF(_, _) | Instruction::VDivF(_, _))
        ));
    }
}
