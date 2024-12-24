use mimium_lang::plugin::Plugin;
use mimium_symphonia::SamplerPlugin;
use mimium_test::*;

fn run_file_with_symphonia(path: &'static str, times: u64) -> Option<Vec<f64>> {
    let plugins: [Box<dyn Plugin>; 1] = [Box::new(SamplerPlugin)];
    run_file_with_plugins(path, times, plugins.into_iter(), false)
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
