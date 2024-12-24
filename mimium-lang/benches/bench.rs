// If you want to run benchmark, you need to run with nightly channel. Run with `cargo +nightly bench`.
#![feature(test)]
extern crate test;

fn main() {
    // 省略
}

#[cfg(test)]
mod tests {

    mod runtime {
        use mimium_lang::compiler::{self, Config};
        use mimium_lang::interner::ToSymbol;
        use mimium_lang::runtime::vm::Machine;
        use test::Bencher;

        fn make_multiosc_src(n: usize) -> String {
            format!(
                "let pi = 3.14159265359
let sr = 44100.0
fn phasor(freq){{
  (self + freq/sr)%1.0
}}
fn osc(freq){{
  sin(phasor(freq)*pi*2.0)
}}
fn amosc(freq,rate){{
  osc(freq + osc(rate)*100.0)
}}

fn replicate(n:float,gen:()->(float,float)->float){{
    if (n>0.0){{
        let c = replicate(n - 1.0,gen)
        let g = gen()
        |x,rate| {{g(x,rate) + c(x+100.0,rate+0.5)}}
    }}else{{
        |x,rate| {{ 0.0 }}
    }}
}}
let mycounter = replicate({n}.0,| |amosc);
fn dsp(){{
    mycounter(500.0,0.5)*0.1
}}"
            )
        }

        fn bench_runtime(b: &mut Bencher, content: &str, times: usize) {
            let compiler = compiler::Context::new([], None, Config::default());
            let program = compiler.emit_bytecode(content).expect("ok");
            let idx = program.get_fun_index(&"dsp".to_symbol()).expect("ok");
            let mut machine = Machine::new(program, [].into_iter(), [].into_iter());
            machine.execute_main();
            b.iter(move || {
                for _i in 0..times {
                    let _ = machine.execute_idx(idx);
                }
            });
        }
        #[bench]
        fn bench_multiosc5(b: &mut Bencher) {
            bench_runtime(b, &make_multiosc_src(5), 1);
        }
        #[bench]
        fn bench_multiosc7(b: &mut Bencher) {
            bench_runtime(b, &make_multiosc_src(7), 1);
        }
        #[bench]
        fn bench_multiosc10(b: &mut Bencher) {
            bench_runtime(b, &make_multiosc_src(9), 1);
        }
        #[bench]
        fn bench_multiosc15(b: &mut Bencher) {
            bench_runtime(b, &make_multiosc_src(15), 1);
        }
        fn make_partialapp_src_from_template(c: &str) -> String {
            format!(
                "let pi = 3.14159265359
let sr = 44100.0
fn phasor(freq,phase){{
  (self + freq/sr + phase)%1.0
}}
fn osc(freq,phase){{
  sin(phasor(freq,phase)*pi*2.0)
}}
fn dsp(){{
    {c}
}}"
            )
        }
        fn make_partialapp_src() -> String {
            make_partialapp_src_from_template("440 |> osc(_,0.0)")
        }
        fn make_no_partialapp_src() -> String {
            make_partialapp_src_from_template("osc(440,0.0)")
        }
        //test the performance degradation when open closure is made on `dsp` function with partial application.
        #[bench]
        fn bench_partialapp(b: &mut Bencher) {
            bench_runtime(b, &make_partialapp_src(), 10);
        }
        #[bench]
        fn bench_partialapp_no(b: &mut Bencher) {
            bench_runtime(b, &make_no_partialapp_src(), 10);
        }
    }
    mod parse {
        use mimium_lang::compiler::{self, Config};
        use test::Bencher;

        fn gen_fn(fn_name: &str, n: usize) -> String {
            let vars = (0..n).map(|i| format!("x{n}_{i}")).collect::<Vec<_>>();
            // duplicate variables meaninglessly
            let let_statements = vars
                .iter()
                .map(|v| format!("  let {v} = x{n}"))
                .collect::<Vec<_>>()
                .join("\n");
            let calc = vars
                .iter()
                .fold(format!("x{n}"), |acc, e| format!("({acc} * 0.1 + {e})"));
            format!(
                "
fn {fn_name}(x{n}:float) {{
{let_statements}
  {calc}
}}
"
            )
        }

        fn make_many_symbols_src(n: usize) -> String {
            let fns = (0..n).map(|i| format!("f{i}")).collect::<Vec<_>>();
            let fn_declarations = fns
                .iter()
                .map(|fn_name| gen_fn(fn_name, n))
                .collect::<Vec<_>>()
                .join("\n\n");
            let fn_sum = fns
                .iter()
                .map(|fn_name| format!("{fn_name}(1.0)"))
                .collect::<Vec<_>>()
                .join(" + ");
            format!(
                "
{fn_declarations}

fn dsp() {{
  {fn_sum}
}}
"
            )
        }

        fn bench_many_symbols(b: &mut Bencher, n: usize) {
            let content = make_many_symbols_src(n);
            let compiler = compiler::Context::new([], None, Config::default());
            b.iter(move || {
                let _mir = compiler.emit_mir(&content);
            });
        }

        #[bench]
        fn bench_many_symbols3(b: &mut Bencher) {
            bench_many_symbols(b, 3);
        }

        #[bench]
        fn bench_many_symbols5(b: &mut Bencher) {
            bench_many_symbols(b, 5);
        }

        #[bench]
        fn bench_many_symbols10(b: &mut Bencher) {
            bench_many_symbols(b, 10);
        }
    }
}
