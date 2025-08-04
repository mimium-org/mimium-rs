use mimium_fmt::pretty_print;
#[test]
fn basic_80() {
    let src = include_str!("dirty.mmm");
    let ans = include_str!("pretty_80.mmm");
    let res = pretty_print(src, &None, 80).expect("failed to pretty print");
    assert_eq!(res, ans)
}
#[test]
fn basic_50() {
    let src = include_str!("dirty.mmm");
    let ans = include_str!("pretty_50.mmm");
    let res = pretty_print(src, &None, 50).expect("failed to pretty print");
    assert_eq!(res, ans)
}
#[test]
fn basic_20() {
    let src = include_str!("dirty.mmm");
    let ans = include_str!("pretty_20.mmm");
    let res = pretty_print(src, &None, 20).expect("failed to pretty print");
    assert_eq!(res, ans)
}
