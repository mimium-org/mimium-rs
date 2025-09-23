pub trait MiniPrint {
    fn simple_print(&self) -> String;

    fn pretty_print(&self) -> String {
        let src = self.simple_print();
        let mut level = 0;
        let mut res = String::new();
        let indent = "   ";
        for c in src.chars() {
            match c {
                '(' => {
                    level += 1;
                }
                ')' => {
                    level -= 1;
                }
                ' ' => {
                    res.push('\n');
                    for _i in 0..level {
                        res.push_str(indent);
                    }
                }
                _ => {}
            }

            res.push(c);
        }
        res
    }
}
