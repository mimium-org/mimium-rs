use crate::{
    // ast_interpreter::{self, PValue, Value},
    compiler::{self, interpret_top},
    interner::ToSymbol,
    utils::{error, miniprint::MiniPrint},
};
use std::io::{stdin, stdout, Write};

pub enum ReplMode {
    Eval,
    EvalMulti(u64),
    ShowAST,
}
pub struct ReplAppData {
    line_count: u64,
    global_ctx: ast_interpreter::Context,
    mode: ReplMode,
}
impl Default for ReplAppData {
    fn default() -> Self {
        Self {
            line_count: 0,
            global_ctx: ast_interpreter::Context::default(),
            mode: ReplMode::Eval,
        }
    }
}

fn process_command(mode_str: &str) -> Option<ReplMode> {
    match mode_str {
        ":e" => {
            println!("Mode:Eval");
            Some(ReplMode::Eval)
        }
        ":m" => {
            println!("Mode:EvalMulti");
            Some(ReplMode::EvalMulti(3))
        }
        ":a" => {
            println!("Mode:AST");
            Some(ReplMode::ShowAST)
        }

        _ => None,
    }
}

fn repl(data: &mut ReplAppData) -> ! {
    let mut src = String::new();
    loop {
        print!("> ");
        let _ = stdout().flush();
        let mut src_tmp = String::new();
        let _size = stdin().read_line(&mut src_tmp).expect("stdin read error.");
        src = format!("{}{}", src, src_tmp);
        if let Some(mode) = process_command(&src[0..2]) {
            data.mode = mode;
            src.clear();
        } else {
            let _ = src.pop(); //remove last linebreak
            if !src.as_str().ends_with('\\') {
                match data.mode {
                    ReplMode::Eval => match interpret_top(src.clone(), &mut data.global_ctx) {
                        Ok(v) => {
                            println!("{:?}", v);
                        }
                        Err(e) => error::report(&src, "".to_symbol(), &e),
                    },
                    ReplMode::EvalMulti(n) => {
                        let mut res = Ok(Value::Primitive(PValue::Numeric(0.0)));
                        for _i in 0..n {
                            res = interpret_top(src.clone(), &mut data.global_ctx);
                            data.global_ctx.history.0 = 0;
                        }
                        match res {
                            Ok(v) => {
                                println!("{:?}", v);
                            }
                            Err(e) => error::report(&src, "".to_symbol(), &e),
                        }
                    }
                    ReplMode::ShowAST => match compiler::emit_ast(&src, None) {
                        Ok(ast) => {
                            println!("{}", ast.pretty_print());
                        }
                        Err(e) => error::report(&src, "".to_symbol(), &e),
                    },
                }
                src.clear();
            } else {
                src.pop(); //remove last backslash
            }

            data.line_count += 1;
        }
    }
}

pub fn run_repl() {
    repl(&mut ReplAppData::default())
}
