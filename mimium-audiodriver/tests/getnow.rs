use mimium_audiodriver::{
    backends::local_buffer::LocalBufferDriver,
    driver::{Driver, SampleRate},
    runtime_fn,
};
use mimium_lang::{
    function,
    interner::ToSymbol as _,
    numeric,
    runtime::vm::{self, FuncProto, Instruction, Program},
    types::{PType, Type},
};

#[test]
fn getnow_test() {
    // fn dsp(){
    //   now
    // }
    let inner_insts_main = vec![Instruction::Return0];
    let main_f = FuncProto {
        bytecodes: inner_insts_main,
        constants: vec![], //cls, int 4
        ..Default::default()
    };
    let inner_insts = vec![
        Instruction::MoveConst(0, 0),     //load constant 0 for closure index
        Instruction::CallExtCls(0, 0, 1), //call getnow
        Instruction::Return(0, 1),        // return single value at 0
    ];
    let dsp_f = FuncProto {
        nparam: 0,
        nret: 1,
        bytecodes: inner_insts,
        constants: vec![0], //cls,
        ..Default::default()
    };
    let fns = vec![("main".to_symbol(), main_f), ("dsp".to_symbol(), dsp_f)];

    let prog = Program {
        global_fn_table: fns,
        ext_cls_table: vec![("_mimium_getnow".to_symbol(), function!(vec![], numeric!()))],
        ..Default::default()
    };

    let times = 10;
    let mut driver = LocalBufferDriver::new(times);
    let (fname, getnowfn, _type) = runtime_fn::gen_getnowfn(driver.count.clone());
    driver.init(
        vm::Machine::new(
            None,
            prog,
            &[],
            &[(fname, getnowfn, function!(vec![], numeric!()))],
        ),
        Some(SampleRate(48000)),
    );
    driver.play();

    let res = driver.get_generated_samples();
    let answer = vec![0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0];
    assert_eq!(res, answer);
}
