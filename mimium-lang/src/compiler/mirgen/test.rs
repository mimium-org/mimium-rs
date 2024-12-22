use crate::{app, assign, ast::builder::*, lambda, let_, number, then, var};

use super::*;

#[test]

fn closure_test() {
    //fn makeCounter(beg,inc){
    // let n = beg+1; // local- 0:beg, 1:inc 2: 1 3:n
    // return | | { //upvalue: n:3 inc:1
    //              n = n+inc;
    //              n
    //             }
    //}
    //fn main(){
    //  let c = makeCounter(13,7);
    //  print(c()); //print 21
    //  print(c()); // print 28
    //}
    let expr = let_!(
        "makeCounter",
        lambda!(
            vec!["beg", "inc"],
            let_!(
                "n",
                app!(var!("add"), vec![var!("beg"), number!(1)]),
                lambda!(
                    vec![],
                    then!(
                        assign!("n", app!(var!("add"), vec![var!("n"), var!("inc")])),
                        var!("n")
                    )
                )
            )
        ),
        let_!(
            "main",
            lambda!(
                vec![],
                let_!(
                    "c",
                    app!(var!("makeCounter"), vec![number!(13), number!(7)]),
                    then!(
                        app!(var!("print"), vec![app!(var!("c"), vec![])]),
                        app!(var!("print"), vec![app!(var!("c"), vec![])])
                    )
                )
            )
        )
    );

    let prog = compile(expr, &[], None).unwrap();
    let mirstr = format!("{:?}", prog);
    println!("{}", mirstr);
}
