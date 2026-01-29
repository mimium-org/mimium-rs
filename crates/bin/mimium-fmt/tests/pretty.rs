// CST-based pretty print tests (now default)
// To regenerate expected files:
//   cargo run -p mimium-fmt -- tests/dirty.mmm --width 80 > tests/pretty_80.mmm
//   cargo run -p mimium-fmt -- tests/dirty.mmm --width 50 > tests/pretty_50.mmm
//   cargo run -p mimium-fmt -- tests/dirty.mmm --width 20 > tests/pretty_20.mmm
mod main {

    use mimium_fmt::pretty_print_cst;
    #[test]
    fn basic_80() {
        let src = include_str!("dirty.mmm").replace("\r\n", "\n");
        let ans = include_str!("pretty_80.mmm").replace("\r\n", "\n");
        let res = pretty_print_cst(&src, &Some("tests/dirty.mmm".into()), 80)
            .expect("failed to pretty print");
        assert_eq!(res, ans); // format success
        let res2 = pretty_print_cst(res.as_str(), &Some("tests/pretty_80.mmm".into()), 80);
        assert!(res2.is_ok()); // formatted document does not contain syntax error
        assert_eq!(res, res2.unwrap()) // pretty printed document is identical
    }
    #[test]
    fn basic_50() {
        let src = include_str!("dirty.mmm").replace("\r\n", "\n");
        let ans = include_str!("pretty_50.mmm").replace("\r\n", "\n");
        let res = pretty_print_cst(&src, &Some("tests/dirty.mmm".into()), 50)
            .expect("failed to pretty print");
        assert_eq!(res, ans);
        let res2 = pretty_print_cst(res.as_str(), &Some("tests/pretty_50.mmm".into()), 50);
        assert!(res2.is_ok()); // formatted document does not contain syntax error
        assert_eq!(res, res2.unwrap()) // pretty printed document is identical
    }
    #[test]
    fn basic_20() {
        let src = include_str!("dirty.mmm").replace("\r\n", "\n");
        let ans = include_str!("pretty_20.mmm").replace("\r\n", "\n");
        let res = pretty_print_cst(&src, &Some("tests/dirty.mmm".into()), 20)
            .expect("failed to pretty print");
        assert_eq!(res, ans);

        let res2 = pretty_print_cst(res.as_str(), &Some("tests/pretty_20.mmm".into()), 20);
        assert!(res2.is_ok()); // formatted document does not contain syntax error
        assert_eq!(res, res2.unwrap()) // pretty printed document is identical
    }
}
