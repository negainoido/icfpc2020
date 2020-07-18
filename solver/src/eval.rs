use std::collections::HashMap;

use crate::typing::*;
use std::cell::RefCell;
use typed_arena::Arena;
use crate::expr::Expr;

#[derive(Debug)]
pub enum EvalError<'a> {
    NumberIsExpected(ExprNode<'a>),
    ListIsExpected(ExprNode<'a>),
    UndefinedVariable(i128),
    Todo,
}

pub struct Evaluator<'a> {
    exprs: Arena<TypedExpr<'a>>,
}

impl<'a> Evaluator<'a> {
    pub fn new() -> Self {
        Evaluator {
            exprs: Arena::new(),
        }
    }

    fn app(&'a self, expr1: ExprNode<'a>, expr2: ExprNode<'a>) -> ExprNode<'a> {
        self.exprs.alloc(TypedExpr::Apply(expr1, expr2))
    }

    fn val(&'a self, symbol: TypedSymbol<'a>) -> ExprNode<'a> {
        self.exprs.alloc(TypedExpr::Val(symbol))
    }

    pub fn eval(
        &'a self,
        expr: ExprNode<'a>,
        env: &HashMap<i128, ExprNode<'a>>,
    ) -> Result<ExprNode, EvalError> {
        use EvalError::*;
        use TypedExpr::*;
        use TypedSymbol::*;

        match expr {
            Val(Variable(i)) => {
                let v = env
                    .get(&i)
                    .map(|v| v.clone())
                    .ok_or(UndefinedVariable(*i))?;
                self.eval(v, env)
            }
            Val(_) => Ok(expr),
            Apply(f, x) => {
                let f = self.eval(f, env)?;
                match f {
                    // Car
                    Val(Car) => {
                        // ap car x   =   ap x t
                        let v = self.app(x.clone(), self.val(True(vec![])));
                        self.eval(v, env)
                    }
                    // Cdr
                    Val(Cdr) => {
                        // ap cdr x2   =   ap x2 f
                        let v = self.app(x.clone(), self.val(False(vec![])));
                        eprintln!("cdr v = {:?}", &v);
                        self.eval(v, env)
                    }
                    // Cons
                    Val(Cons(xs)) if xs.len() == 2 => {
                        // ap ap ap cons x0 x1 x2   =   ap ap x2 x0 x1
                        let v = self.app(self.app(x.clone(), xs[0]), xs[1]);
                        self.eval(v, env)
                    }
                    Val(Cons(xs)) => {
                        let mut args = xs.clone();
                        args.push(x.clone());
                        Ok(self.val(Cons(args)))
                    }
                    // B-Combinator
                    Val(BComb(xs)) if xs.len() == 2 => {
                        // ap ap ap b x0 x1 x2   =   ap x0 ap x1 x2
                        let v = self.app(xs[0], self.app(xs[1], x.clone()));
                        self.eval(v, env)
                    }
                    Val(BComb(xs)) => {
                        assert!(xs.len() < 2);
                        let mut args = xs.clone();
                        args.push(x.clone());
                        Ok(self.val(BComb(args)))
                    }
                    // C-Combinator
                    Val(CComb(xs)) if xs.len() == 2 => {
                        // ap ap ap c x0 x1 x2   =   ap ap x0 x2 x1
                        let v = self.app(self.app(xs[0], x.clone()), xs[1]);
                        self.eval(v, env)
                    }
                    Val(CComb(xs)) => {
                        assert!(xs.len() < 2);
                        let mut args = xs.clone();
                        args.push(x.clone());
                        Ok(self.val(CComb(args)))
                    }
                    // S-Combinator
                    Val(SComb(xs)) if xs.len() == 2 => {
                        // ap ap ap s x0 x1 x2   =   ap ap x0 x2 ap x1 x2
                        let v = self.app(
                            self.app(xs[0], x.clone()),
                            self.app(xs[1], x.clone()),
                        );
                        self.eval(v, env)
                    }
                    Val(SComb(xs)) => {
                        assert!(xs.len() < 2);
                        let mut args = xs.clone();
                        args.push(x.clone());
                        Ok(self.val(SComb(args)))
                    }
                    // I-Combinator
                    Val(IComb) => {
                        // ap i x0   =   x0
                        self.eval(x.clone(), env)
                    }
                    // True
                    Val(True(xs)) if xs.len() == 1 => {
                        // ap ap t x0 x1   =   x0
                        let x0 = xs[0];
                        self.eval(x0, env)
                    }
                    Val(True(xs)) => {
                        assert_eq!(xs.len(), 0);
                        let mut args = xs.clone();
                        args.push(x.clone());
                        Ok(self.val(True(args)))
                    }
                    // False
                    Val(False(xs)) if xs.len() == 1 => {
                        // ap ap f x0 x1   =   x1
                        self.eval(x.clone(), env)
                    }
                    Val(False(xs)) => {
                        assert_eq!(xs.len(), 0);
                        let mut args = xs.clone();
                        args.push(x.clone());
                        Ok(self.val(False(args)))
                    }
                    // Sum (Add)
                    Val(Sum(xs)) if xs.len() == 1 => {
                        let x0 = self
                            .eval(xs[0], env)?
                            .get_number()
                            .unwrap();
                        let x1 = self.eval(x.clone(), env)?.get_number().unwrap();
                        Ok(self.val(Number(x0 + x1)))
                    }
                    Val(Sum(xs)) => {
                        assert_eq!(xs.len(), 0);
                        let args = vec![x.clone()];
                        Ok(self.val(Sum(args)))
                    }
                    // Product
                    Val(Prod(xs)) if xs.len() == 1 => {
                        let x0 = self
                            .eval(xs[0], env)?
                            .get_number()
                            .unwrap();
                        let x1 = self
                            .eval(x.clone(), env)?
                            .get_number()
                            .unwrap();
                        Ok(self.val(Number(x0 * x1)))
                    }
                    Val(Prod(xs)) => {
                        assert_eq!(xs.len(), 0);
                        let args = vec![x.clone()];
                        Ok(self.val(Prod(args)))
                    }
                    Val(Neg) => {
                        let x = self.eval(x.clone(), env)?;
                        let x = x.get_number().unwrap();
                        Ok(self.val(Number(-x)))
                    }
                    // Div
                    Val(Div(xs)) if xs.len() == 1 => {
                        let x_num = self
                            .eval(xs[0], env)?
                            .get_number()
                            .unwrap();
                        let x_den = self
                            .eval(x.clone(), env)?
                            .get_number()
                            .unwrap();
                        Ok(self.val(Number(x_num / x_den)))
                    }
                    Val(Div(xs)) => {
                        assert_eq!(xs.len(), 0);
                        let args = vec![x.clone()];
                        Ok(self.val(Div(args)))
                    }
                    Val(Nil) => Ok(self.val(True(vec![]))),
                    Val(IsNil) => {
                        let e = self.eval(x.clone(), env)?;
                        match e {
                            Val(Nil) => Ok(self.val(True(vec![]))),
                            Val(Cons(_)) => Ok(self.val(False(vec![]))),
                            _ => Err(ListIsExpected(e)),
                        }
                    }
                    // Less
                    Val(Less(xs)) if xs.len() == 1 => {
                        let x0 = self
                            .eval(xs[0], env)?
                            .get_number()
                            .unwrap();
                        let x1 = self
                            .eval(x.clone(), env)?
                            .get_number()
                            .unwrap();

                        if x0 < x1 {
                            Ok(self.val(True(vec![])))
                        } else {
                            Ok(self.val(False(vec![])))
                        }
                    }
                    Val(Less(xs)) => {
                        assert_eq!(xs.len(), 0);
                        let args = vec![x.clone()];
                        Ok(self.val(Less(args)))
                    }
                    // BigEq
                    Val(BigEq(xs)) if xs.len() == 1 => {
                        let x0 = self
                            .eval(xs[0], env)?
                            .get_number()
                            .unwrap();
                        let x1 = self
                            .eval(x.clone(), env)?
                            .get_number()
                            .unwrap();

                        if x0 == x1 {
                            Ok(self.val(True(vec![])))
                        } else {
                            Ok(self.val(False(vec![])))
                        }
                    }
                    Val(BigEq(xs)) => {
                        assert_eq!(xs.len(), 0);
                        let args = vec![x.clone()];
                        Ok(self.val(BigEq(args)))
                    }
                    _ => {
                        eprintln!("Applying f={:?} to x={:?}", &f, &x);
                        Err(Todo)
                    }
                }
            }
        }
    }

    pub fn typing(&'a self, expr: &Expr) -> Option<ExprNode<'a>> {

        match expr {
            Expr::Val(sym) => TypedSymbol::typing(sym).map(|s| self.val(s)),
            Expr::Apply(e1, e2) => match (self.typing(e1), self.typing(e2)) {
                (Some(t1), Some(t2)) => Some(self.app(t1, t2)),
                _ => None,
            },
        }
    }
}

pub fn eval<'a>(
    _expr: &'a TypedExpr<'a>,
    _env: &'a HashMap<i128, ExprNode<'a>>,
) -> Result<TypedExpr<'a>, EvalError<'a>> {
    unimplemented!();
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::typing::raku::*;
    use crate::typing::TypedExpr::*;
    use crate::typing::TypedSymbol::*;

    fn empty_env<'a>() -> HashMap<i128, ExprNode<'a>> {
        HashMap::new()
    }

    #[test]
    fn test_neg() {
        let exp = neg(&number(5));
        let e = Evaluator::new().eval(&exp, &empty_env()).unwrap();
        assert_eq!(e, &number(-5))
    }

    #[test]
    fn test_div() {
        let exp = div(&number(5), &number(2));
        let e = Evaluator::new().eval(&exp, &empty_env()).unwrap();
        assert_eq!(e, &number(2))
    }

    #[test]
    fn test_div_numerator_minus() {
        // ap ap div -5 3   =   -1
        let exp = div(&number(-5), &number(3));
        let e = Evaluator::new().eval(&exp, &empty_env()).unwrap();
        assert_eq!(e, &number(-1))
    }

    #[test]
    fn test_div_denominator_minus() {
        // ap ap div 5 -3   =   -1
        let exp = div(&number(5), &number(-3));
        let e = Evaluator::new().eval(&exp, &empty_env()).unwrap();
        assert_eq!(e, &number(-1))
    }

    #[test]
    fn test_div_num_den_minus() {
        // ap ap div -5 -3   =   1
        let exp = div(&number(-5), &number(-3));
        let e = Evaluator::new().eval(&exp, &empty_env()).unwrap();
        assert_eq!(e, &number(1))
    }

    #[test]
    fn test_less() {
        use TypedExpr::Val;

        // ap ap lt 0 -1   =   f
        let exp0 = less(&number(0), &number(-1));
        let e0 = Evaluator::new().eval(&exp0, &empty_env()).unwrap();
        assert_eq!(e0, &Val(TypedSymbol::False(vec![])));

        // ap ap lt 0 0   =   f
        let exp1 = less(&number(0), &number(0));
        let e1 = Evaluator::new().eval(&exp1, &empty_env()).unwrap();
        assert_eq!(e1, &Val(TypedSymbol::False(vec![])));

        // ap ap lt 0 1   =   t
        let exp2 = less(&number(0), &number(1));
        let e2 = Evaluator::new().eval(&exp2, &empty_env()).unwrap();
        assert_eq!(e2, &Val(TypedSymbol::True(vec![])));
    }

    #[test]
    fn test_cons() {
        {
            let pair = cons(&number(1), &NIL);
            let x = app(&CAR, &pair);
            let e = Evaluator::new().eval(&x, &empty_env()).unwrap();
            assert_eq!(e, &number(1));
        }
        {
            let pair = cons(&number(1), &NIL);
            let x = app(&CDR, &pair);
            let e = Evaluator::new().eval(&x, &empty_env()).unwrap();
            assert_eq!(e, &NIL);
        }
        {
            let x = cons(&number(1), &cons(&number(2), &NIL));
            let e = Evaluator::new().eval(&x, &empty_env()).unwrap();
            assert_eq!(e, &val(Cons(vec![&number(1), &cons(&number(2), &NIL)])));
        }
        {
            let x = cons(&number(1), &cons(&number(2), &NIL));
            let x = app(&CDR, &x);
            let e = Evaluator::new().eval(&x, &empty_env()).unwrap();
            assert_eq!(e, &val(Cons(vec![&number(2), &NIL])));
        }
    }

    #[test]
    fn test_cons_galaxy_line1() {
        use TypedSymbol::*;

        // ap ap cons 7 ap ap cons 123 nil
        let x = cons(&number(7), &cons(&number(123), &NIL));

        let expected = val(Cons(vec![
            &val(Number(7)),
            &app(&app(&val(Cons(vec![])), &val(Number(123))), &val(Nil)),
        ]));

        assert_eq!(&expected, Evaluator::new().eval(&x, &empty_env()).unwrap());
    }

    #[test]
    fn test_variable() {
        use TypedExpr::*;
        use TypedSymbol::*;
        // var1 = 123
        // ap ap cons 0 ap ap cons var1 2
        let mut env = HashMap::new();
        let v = number(123);
        env.insert(1, &v);
        let e = cons(&number(0), &cons(&variable(1), &number(2)));
        let expected = Val(Cons(vec![
            &Val(Number(0)),
            &app(&app(&val(Cons(vec![])), &val(Variable(1))), &val(Number(2))),
        ]));

        assert_eq!(&expected, Evaluator::new().eval(&e, &env).unwrap());
    }

    #[test]
    fn test_variable_func() {
        use TypedExpr::*;
        use TypedSymbol::*;
        // var1 = cons
        // ap ap cons 0 ap ap var1 1 2
        let mut env = HashMap::new();
        let v = Val(Cons(vec![]));
        env.insert(1, &v);
        let e = cons(&number(0), &app(&app(&variable(1), &number(1)), &number(2)));
        let v = app(&app(&val(Variable(1)), &val(Number(1))), &val(Number(2)));
        let expected = Val(Cons(vec![ &Val(Number(0)), &v ]));

        assert_eq!(&expected, Evaluator::new().eval(&e, &env).unwrap());
    }

    #[test]
    fn test_variable_func_with_var() {
        use TypedExpr::*;
        use TypedSymbol::*;
        // var1 = cons 1
        // ap ap cons 0 ap var1 2
        let mut env = HashMap::new();
        let v = Val(Cons(vec![&Val(Number(1))]));
        env.insert(1, &v);
        let e = cons(&number(0), &app(&variable(1), &number(2)));
        let expected = Val(Cons(vec![
            &Val(Number(0)),
            &app(&val(Variable(1)), &val(Number(2))),
        ]));

        assert_eq!(&expected, Evaluator::new().eval(&e, &env).unwrap());
    }

    #[test]
    fn test_sum() {
        let env = HashMap::new();
        let expr = sum(&number(1), &number(2));
        assert_eq!(&number(3), Evaluator::new().eval(&expr, &env).unwrap());
    }

    #[test]
    fn test_is_nil() {
        let env = HashMap::new();
        let expr = isnil(&NIL);
        assert_eq!(&T, Evaluator::new().eval(&expr, &env).unwrap());
        let expr = isnil(&cons(&number(1), &NIL));
        assert_eq!(&F, Evaluator::new().eval(&expr, &env).unwrap());
    }

    #[test]
    fn test_big_eq() {
        let env = HashMap::new();
        let expr = big_eq(&number(1), &number(1));
        assert_eq!(&T, Evaluator::new().eval(&expr, &env).unwrap());
        let expr = big_eq(&number(1), &number(0));
        assert_eq!(&F, Evaluator::new().eval(&expr, &env).unwrap());
    }

    #[test]
    fn test_comb_c() {
        // C add 2 3 = add 3 2 = 5
        {
            let expr = app(&app(&app(&CCOMB, &SUM), &number(2)), &number(3));
            let env = HashMap::new();
            assert_eq!(&number(5), Evaluator::new().eval(&expr, &env).unwrap());
        }
        // C cons 2 3 = cons 3 2 = 5
        {
            let expr = app(&app(&app(&CCOMB, &CONS), &number(2)), &number(3));
            let env = HashMap::new();
            assert_eq!(
                &Val(Cons(vec![&number(3), &number(2)])),
                Evaluator::new().eval(&expr, &env).unwrap()
            );
        }
    }

    #[test]
    fn test_comb_b() {
        // B neg neg x = x
        for x in -3..4 {
            let expr = app(&app(&app(&BCOMB, &NEG), &NEG), &number(x));
            let env = HashMap::new();
            assert_eq!(&number(x), Evaluator::new().eval(&expr, &env).unwrap());
        }
    }

    #[test]
    fn test_comb_i() {
        // I x = x
        {
            let expr = app(&ICOMB, &NEG);
            let env = HashMap::new();
            assert_eq!(&NEG, Evaluator::new().eval(&expr, &env).unwrap());
        }
        {
            let expr = app(&ICOMB, &BCOMB);
            let env = HashMap::new();
            assert_eq!(&BCOMB, Evaluator::new().eval(&expr, &env).unwrap());
        }
    }
}
