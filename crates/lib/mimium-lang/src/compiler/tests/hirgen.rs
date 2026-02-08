#[cfg(test)]

mod test {
    use mimium_hirgen::typing::{infer_type, InferContext};
    use mmmtype::*;
    use ast::*;
    use utils::metadata::WithMeta;
    use std::path::PathBuf;
    #[test]
    pub fn unify_prim() {
        let ast = WithMeta(Expr::Literal(Literal::Float("10.2".to_string())),0..0);
        let mut ctx = InferContext::new(&[], PathBuf::from("test"), None, None, None);
        let res = infer_type(ast.0,&mut ctx).unwrap();
        assert_eq!(res, Type::Numeric);
    }
}
