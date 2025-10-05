use mimium_audiodriver::backends::local_buffer::LocalBufferDriver;
use mimium_audiodriver::driver::{Driver, RuntimeData};
use mimium_lang::plugin::SystemPlugin;
use mimium_lang::utils::error::report;
use mimium_lang::{Config, ExecContext};
use mimium_symphonia::SamplerPlugin;
use mimium_test::*;

fn run_file_with_symphonia(path: &'static str, times: u64) -> Option<Vec<f64>> {
    run_file_with_sys_plugin(path, times, SamplerPlugin::default())
}

fn run_file_with_sys_plugin<T: SystemPlugin + 'static>(
    path: &'static str,
    times: u64,
    plugin: T,
) -> Option<Vec<f64>> {
    let (file, src) = load_src(path);
    let mut driver = LocalBufferDriver::new(times as _);
    let audiodriverplug: Box<dyn mimium_lang::plugin::Plugin> = Box::new(driver.get_as_plugin());
    let mut ctx = ExecContext::new(
        [audiodriverplug].into_iter(),
        Some(file.clone()),
        Config::default(),
    );
    ctx.add_system_plugin(plugin);

    match ctx.prepare_machine(&src) {
        Ok(_) => {}
        Err(errs) => {
            report(&src, file, &errs);
            return None;
        }
    }

    let _ = ctx.run_main();
    let runtimedata = {
        let ctxmut: &mut ExecContext = &mut ctx;
        RuntimeData::try_from(ctxmut).unwrap()
    };
    driver.init(runtimedata, None);
    driver.play();
    Some(driver.get_generated_samples().to_vec())
}

//loadwav reads wave file `count_100_by_0_01_f32_48000Hz.wav` that is sequence of 0, 0.01, 0.02...

#[test]
fn test_readwav() {
    let res = run_file_with_symphonia("loadwav.mmm", 101).expect("failed to evaluate");
    let res_int = res
        .iter()
        .map(|f| (*f * 100.0).round() as u32)
        .collect::<Vec<_>>();
    let mut ans = (0u32..100).collect::<Vec<_>>();
    ans.push(0); //0 should be returned when the index exceeds the boundary
    assert_eq!(res_int, ans);
}

#[test]
fn test_readwav_interp() {
    //res should be 0.005, 0.0015,
    let res = run_file_with_symphonia("loadwav_interp.mmm", 101).expect("failed to evaluate");
    let res_int = res
        .iter()
        .map(|f| (*f * 1000.0).round() as u32)
        .collect::<Vec<_>>();
    let mut ans = (0u32..100)
        .map(|x| (x * 10 + 5).min(990))
        .collect::<Vec<_>>();
    ans.push(0); //0 should be returned when the index exceeds the boundary
    assert_eq!(res_int, ans);
}
