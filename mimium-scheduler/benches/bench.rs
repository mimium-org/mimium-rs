// If you want to run benchmark, you need to run with nightly channel. Run with `cargo +nightly bench`.
#![feature(test)]
extern crate test;

fn main() {}
#[cfg(test)]
mod tests {
    use mimium_test::run_source_with_scheduler;
    use test::Bencher;
    #[bench]
    fn scheduler_counter(b: &mut Bencher) {
        let src = r"fn makecounter(){
    let x = 0.0
    letrec gen = | |{
        x = x+1.0
       gen@(now+1.0)
    }
    gen@1.0
    let getter = | | {x}
    getter
}
let x_getter = makecounter();
fn dsp(){
    x_getter()
}";
        b.iter(move || {
            let _res = run_source_with_scheduler(src, 10);
        })
    }
}
