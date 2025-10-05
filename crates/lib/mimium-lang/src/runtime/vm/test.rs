use super::*;
use crate::{
    interner::ToSymbol,
    mir::OpenUpValue,
    plugin::{self, ExtFunTypeInfo},
    types::Type,
    utils::half_float::HFloat,
};

#[test]
fn ensure_closurekey_size() {
    assert_eq!(size_of::<ClosureIdx>(), size_of::<RawVal>());
}

#[test]
fn stack_set_vec_test1() {
    //less
    let mut testvec = vec![0u64, 1, 2, 3, 4, 5];
    set_vec_range(&mut testvec, 2, &[6, 7]);
    assert_eq!(testvec, vec![0u64, 1, 6, 7, 4, 5])
}
#[test]
fn stack_set_vec_test2() {
    //greater
    let mut testvec = vec![0u64, 1, 2, 3, 4, 5];
    set_vec_range(&mut testvec, 6, &[6, 7]);
    assert_eq!(testvec, vec![0u64, 1, 2, 3, 4, 5, 6, 7])
}

#[test]
fn stack_set_vec_test3() {
    //equal
    let mut testvec = vec![0u64, 1, 2, 3, 4, 5];
    set_vec_range(&mut testvec, 5, &[6, 7]);
    assert_eq!(testvec, vec![0u64, 1, 2, 3, 4, 6, 7])
}
#[test]
fn stack_set_vec_test4() {
    //even_greater
    let mut testvec = vec![0u64, 1, 2, 3, 4, 5];
    set_vec_range(&mut testvec, 7, &[6, 7]);
    assert_eq!(testvec, vec![0u64, 1, 2, 3, 4, 5, 0, 6, 7])
}

#[test]
fn size_of_intern_func() {
    let s = std::mem::size_of::<std::rc::Rc<FuncProto>>();
    assert_eq!(s, 8);
}
#[test]
fn size_of_extern_func() {
    let s = std::mem::size_of::<ExtFunType>();
    assert_eq!(s, 8);
}

//single print function
fn lib_printi(state: &mut Machine) -> i64 {
    let v = state.get_top_n(1)[0];
    let i = Machine::get_as::<i64>(v);
    println!("{i}");
    1
}

#[test]
fn closuretest() {
    //fn makeCounter(beg,inc){
    // let n = beg+1;
    // return |x| { //local 0:x
    //              n = n+inc+x;
    //              n
    //             }
    //}
    //fn main(){
    //  let c = makeCounter(13,7);
    //  print(c(0)); //print 21
    //  print(c(0)); // print 28
    //}

    let inner_insts = vec![
        Instruction::GetUpValue(0, 0, 1), //load n
        Instruction::GetUpValue(1, 1, 1), //load inc
        Instruction::AddI(0, 0, 1),       // store n+inc in new n
        Instruction::Move(1, 0),          //load x
        Instruction::AddI(0, 0, 1),       // store n+inc+x in new n
        Instruction::SetUpValue(0, 0, 1), //store new n in upvalue index 0
        Instruction::Return(0, 1),        // return single value at 1
    ];
    let inner_f = FuncProto {
        nparam: 0,
        nret: 1,
        upindexes: vec![
            OpenUpValue {
                pos: 1,
                size: 1,
                is_closure: false,
            },
            OpenUpValue {
                pos: 2,
                size: 1,
                is_closure: false,
            },
        ],
        bytecodes: inner_insts,
        constants: vec![], //no constants in the inner function
        ..Default::default()
    };
    let inner_insts2 = vec![
        // reg0:beg, reg1: inc
        Instruction::MoveConst(2, 0), //load 1 in reg2
        Instruction::AddI(3, 0, 2),   // beg+1, n is in reg0
        Instruction::MoveConst(4, 1), //load posf in reg4
        Instruction::Closure(5, 4), // make closure of constant table 1(which is the function table 0)
        Instruction::Close(5),      // convert n(at 0) and inc(at 1) into closed value
        Instruction::Return(5, 1),  // return 1 value
    ];
    let makecounter_f = FuncProto {
        nparam: 2,
        nret: 1,
        bytecodes: inner_insts2,
        constants: vec![1u64, 2], // 1, position of inner in global table
        ..Default::default()
    };
    let main_inst = vec![
        // no stack in the entry
        Instruction::MoveConst(0, 2), //load makecounter
        Instruction::MoveConst(1, 0), //load 2
        Instruction::MoveConst(2, 1), //load 3 [makecounter, 2, 3]
        Instruction::Call(0, 2, 1), // [(closure)]  call makecounter on register 2 with 2 arguments and 1 return value.return value (inner closure)is on reg 0
        //print(c())
        Instruction::Move(1, 0),          // move closure 0 to 1
        Instruction::MoveConst(2, 3),     //load 0
        Instruction::CallCls(1, 0, 1), // call inner closure with 0 args and 1 return value.(result is in 0)
        Instruction::Move(2, 1),       // load result to reg 2
        Instruction::MoveConst(1, 3),  //set print into reg1
        Instruction::CallExtFun(1, 1, 1), //print result
        //repeat precvous 4 step : print(c())
        Instruction::Move(1, 0),      // move closure 0 to 1
        Instruction::MoveConst(2, 3), //load 0
        Instruction::CallCls(1, 0, 1),
        Instruction::Move(2, 1),
        Instruction::MoveConst(1, 3),
        Instruction::CallExtFun(1, 1, 1),
        Instruction::Return0,
    ];
    let main_f = FuncProto {
        nparam: 0,
        nret: 1,
        bytecodes: main_inst,
        constants: vec![13u64, 7u64, 1, 0], //13,7, makecounter, print_f
        ..Default::default()
    };
    let global_fn_table = vec![
        ("main".to_string(), main_f),
        ("makecounter".to_string(), makecounter_f),
        ("inner".to_string(), inner_f),
    ];

    // machine.install_extern_fn("lib_printi".to_string(), lib_printi);
    let default_plugin = plugin::get_builtin_fns_as_plugins();
    let builtins = default_plugin.get_ext_closures().into_iter();
    let ext_fun_table = plugin::get_extfun_types(&[default_plugin])
        .filter(|ExtFunTypeInfo { stage, .. }| stage.is_available_in_vm())
        .map(|ExtFunTypeInfo { name, ty, .. }| (name.to_string(), ty))
        .collect();
    let prog = Program {
        global_fn_table,
        ext_fun_table,
        ..Default::default()
    };
    let mut machine = Machine::new(prog, [].into_iter(), builtins);
    let res = machine.execute_main();
    assert_eq!(res, 0);
}

#[test]
fn rust_closure_test() {
    //fn main()->int{
    // return rust_closure(4)
    //}
    let inner_insts = vec![
        Instruction::MoveConst(0, 0),     //load closure
        Instruction::MoveConst(1, 1),     //load const int 4
        Instruction::CallExtFun(0, 1, 1), //call closure, 7 should be set at reg 0
        Instruction::Return0,             // return single value at 1
    ];
    let main_f = FuncProto {
        nparam: 0,
        nret: 1,
        bytecodes: inner_insts,
        constants: vec![0u64, 4u64], //cls, int 4
        ..Default::default()
    };
    let fns = vec![main_f];
    let fnames = vec!["main".to_string()];
    let global_fn_table = fnames.into_iter().zip(fns).collect::<Vec<_>>();
    // let mut count = 0;
    let cls = Rc::new(RefCell::new(|m: &mut Machine| -> ReturnCode {
        let v = m.get_stack(1);
        let i = Machine::get_as::<u64>(v) + 3;
        println!("Call from closure: {i}");
        //?????
        m.set_stack(-1, Machine::to_value(i));
        1
    }));
    let unknownt = Type::Unknown.into_id();

    let prog = Program {
        global_fn_table,
        ext_fun_table: vec![
            ("lib_printi".to_string(), unknownt),
            ("rustclosure".to_string(), unknownt),
        ],
        ..Default::default()
    };
    let mut machine = Machine::new(
        prog,
        [ExtFunInfo::new(
            "lib_printi".to_symbol(),
            unknownt,
            lib_printi as ExtFunType,
        )]
        .into_iter(),
        [Box::new(ExtClsInfo::new(
            "rustclosure".to_symbol(),
            unknownt,
            cls.clone() as ExtClsType,
        )) as Box<dyn MachineFunction>]
        .into_iter(),
    );
    let res = machine.execute_main();
    assert_eq!(res, 0);
}

fn prep_closure_gc_program(is_closed: bool) -> Machine {
    let inner_insts = vec![
        Instruction::MoveConst(0, 0),
        Instruction::Return(0, 1), // return just 0
    ];
    let cls_f = FuncProto {
        nparam: 0,
        nret: 1,
        bytecodes: inner_insts,
        constants: vec![0], //13,7, makecounter, print_f
        ..Default::default()
    };
    let mut inner_insts = vec![
        Instruction::MoveConst(0, 0), //load closure
        Instruction::Closure(0, 0),
        Instruction::CallCls(0, 0, 1),
        Instruction::Return0, // return single value at 1
    ];
    if is_closed {
        inner_insts.insert(2, Instruction::Close(0))
    }
    let main_f = FuncProto {
        nparam: 0,
        nret: 1,
        bytecodes: inner_insts,
        constants: vec![1], //13,7, makecounter, print_f
        ..Default::default()
    };
    let global_fn_table = vec![("main".to_string(), main_f), ("cls".to_string(), cls_f)];
    let default_plugin = plugin::get_builtin_fns_as_plugins();
    let builtins = default_plugin.get_ext_closures().into_iter();
    let ext_fun_table = plugin::get_extfun_types(&[default_plugin])
        .filter(|ExtFunTypeInfo { stage, .. }| stage.is_available_in_vm())
        .map(|ExtFunTypeInfo { name, ty, .. }| (name.to_string(), ty))
        .collect();
    let prog = Program {
        global_fn_table,
        ext_fun_table,
        ..Default::default()
    };
    Machine::new(prog, [].into_iter(), builtins.into_iter())
}

// closure gc is disabled until correct implementation comes.

#[test]
fn closure_gc_open() {
    let mut machine = prep_closure_gc_program(false);

    machine.execute_main();
    //open closure should be released.
    assert_eq!(machine.closures.len(), 0);
}
#[test]
fn closure_gc_closed() {
    let mut machine = prep_closure_gc_program(true);
    machine.execute_main();
    //closed closure should be kept.
    assert_eq!(machine.closures.len(), 1);
}

fn prep_array_program() -> Program {
    //the main function create array of [10,20,30] and just returns the second element.
    let inner_insts = vec![
        Instruction::AllocArray(0, 3, 1), // allocate array of 3 elements
        Instruction::MoveImmF(1, HFloat::try_from(0.0).unwrap()), // set first element
        Instruction::MoveImmF(2, HFloat::try_from(10.0).unwrap()),
        Instruction::SetArrayElem(0, 1, 2),
        Instruction::MoveImmF(1, HFloat::try_from(1.0).unwrap()), // set first element
        Instruction::MoveImmF(2, HFloat::try_from(20.0).unwrap()),
        Instruction::SetArrayElem(0, 1, 2),
        Instruction::MoveImmF(1, HFloat::try_from(2.0).unwrap()), // set first element
        Instruction::MoveImmF(2, HFloat::try_from(40.0).unwrap()),
        Instruction::SetArrayElem(0, 1, 2),
        Instruction::MoveImmF(3, HFloat::try_from(1.0).unwrap()),
        Instruction::GetArrayElem(4, 0, 3), // return array at 1
        Instruction::Return(4, 1),          // return just 0
    ];
    let main_f = FuncProto {
        nparam: 0,
        nret: 1,
        bytecodes: inner_insts,
        constants: vec![],
        ..Default::default()
    };
    let global_fn_table = vec![("main".to_string(), main_f)];
    Program {
        global_fn_table,
        ext_fun_table: vec![],
        ..Default::default()
    }
}

#[test]
fn array_init() {
    let prog = prep_array_program();
    let mut machine = Machine::new(prog, [].into_iter(), [].into_iter());
    machine.execute_main();
    let res = machine.get_top_n(1)[0];
    //open closure should be released.
    assert_eq!(Machine::get_as::<f64>(res), 20.0);
}
