use hir::expr::*;
use utils::metadata::WithMeta;

pub struct EvalContext {
    pub stage: u64,
}

pub fn eval_stage_hir(expr: WithMeta<Expr>, ctx: &mut EvalContext) -> WithMeta<Expr> {
    let WithMeta(e, span) = expr.clone();

    match e {
        Expr::Var(var, _time) if ctx.stage == 0 => {
            let r = var.0.v.borrow().as_ref().unwrap().clone();
            WithMeta(r, span.clone())
        }
        Expr::Apply(box WithMeta(Expr::Lambda(p, body), _fspan), box WithMeta(callee, _cspan))
            if ctx.stage == 0 =>
        {
            let param = &p[0].0;
            let mut myp = param.v.borrow_mut();
            *myp = Some(callee);
            eval_stage_hir(*body, ctx)
        }
        Expr::Apply(fun, box callee) if fun.0.is_value() => {
            let newcallee = eval_stage_hir(callee, ctx);
            let res = WithMeta(Expr::Apply(fun, Box::new(newcallee)), span.clone());
            eval_stage_hir(res, ctx)
        }
        Expr::Apply(box fun, callee) => {
            let res = WithMeta(
                Expr::Apply(Box::new(eval_stage_hir(fun, ctx)), callee.clone()),
                span.clone(),
            );
            eval_stage_hir(res, ctx)
        }
        Expr::If(box cond, box then, opt_else) => {
            let cond = eval_stage_hir(cond, ctx);
            let then = eval_stage_hir(then, ctx);
            let opt_else = opt_else.map(|e| Box::new(eval_stage_hir(*e, ctx)));
            if ctx.stage == 0 {
                if cond.0.eval_condition() {
                    then
                } else {
                    *opt_else.unwrap()
                }
            } else {
                WithMeta(
                    Expr::If(Box::new(cond), Box::new(then), opt_else),
                    span.clone(),
                )
            }
        }
        Expr::Bracket(b) => {
            ctx.stage += 1;
            eval_stage_hir(*b, ctx)
        }
        Expr::Escape(b) => {
            ctx.stage -= 1;
            eval_stage_hir(*b, ctx)
        }
        Expr::Lambda(params, body) if ctx.stage == 0 => WithMeta(
            Expr::Lambda(params, Box::new(eval_stage_hir(*body, ctx))),
            span.clone(),
        ),
        _ if ctx.stage == 0 && e.is_value() => expr.clone(),
        _ => expr.walk(|x| eval_stage_hir(x, ctx)),
    }
}
