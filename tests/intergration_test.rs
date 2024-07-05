extern crate mimium_rs;
use std::path::PathBuf;

use mimium_rs::{compiler,runtime::{self, vm}, utils::{error::ReportableError, fileloader}};


fn run_multiple(
    src:&str,
    times: u64,
) -> Result<Vec<f64>, Vec<Box<dyn ReportableError>>> {
    let bytecode = compiler::emit_bytecode(src)?;
    let mut machine = vm::Machine::new();
    let mut ret = 0.0;
    let mut res:Vec<f64> = vec![];
    machine.link_functions(&bytecode);
    for _i in 0..times {
        ret = runtime::run_bytecode_test(&mut machine, &bytecode)?;
        res.push(ret);
    }
    Ok(res)
}

fn run_multiple_file(path:&str,times:u64)->Result<Vec<f64>,Box<dyn std::error::Error>>{
    let file: PathBuf = [env!("CARGO_MANIFEST_DIR"), "tests/mmm", path].iter().collect();
    println!("{}",file.to_str().unwrap());
    let (src,_path) = fileloader::load(file.to_string_lossy().to_string())?;
    let res = run_multiple(&src, times).unwrap();
    Ok(res)
}

#[test]
fn counter(){
    let res = run_multiple_file("counter.mmm", 10).unwrap();
    let ans = vec![1.0f64,2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,10.0];
    assert_eq!(res,ans);
}