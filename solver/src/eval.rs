use std::collections::HashMap;

use crate::typing::*;

#[derive(Debug)]
pub enum EvalError {
    NumberIsExpected(TypedExpr),
    ListIsExpected(TypedExpr),
    UndefinedVariable(i128),
    Todo,
}

pub fn app(e1: TypedExpr, e2: TypedExpr) -> TypedExpr {
    TypedExpr::Apply(Box::new(e1), Box::new(e2))
}

pub fn val(sym: TypedSymbol) -> TypedExpr {
    TypedExpr::Val(sym)
}

pub fn nil() -> TypedExpr {
    TypedExpr::Val(TypedSymbol::Nil)
}

pub fn car() -> TypedExpr {
    TypedExpr::Val(TypedSymbol::Car)
}

pub fn cons(e1: TypedExpr, e2: TypedExpr) -> TypedExpr {
    let c = TypedExpr::Val(TypedSymbol::Cons(vec![]));
    app(app(c, e1), e2)
}

pub fn number(x: i128) -> TypedExpr {
    TypedExpr::Val(TypedSymbol::Number(x))
}

pub fn eval(expr: &TypedExpr, env: &HashMap<i128, TypedExpr>) -> Result<TypedExpr, EvalError> {
    use EvalError::*;
    use TypedExpr::*;
    use TypedSymbol::*;

    match expr {
        Val(Variable(i)) => {
            let v = env.get(i).map(|v| v.clone()).ok_or(UndefinedVariable(*i))?;
            eval(&v, env)
        }
        Val(_) => Ok(expr.clone()),
        Apply(f, x) => {
            let f = eval(&f, env)?;
            match f {
                // Car
                Val(Car) => {
                    // ap car x   =   ap x t
                    let v = app(*x.clone(), val(True(vec![])));
                    eval(&v, env)
                }
                // Cdr
                Val(Cdr) => {
                    // ap cdr x2   =   ap x2 f
                    let v = app(*x.clone(), val(False(vec![])));
                    eval(&v, env)
                }
                // Cons
                Val(Cons(xs)) if xs.len() == 2 => {
                    // ap ap ap cons x0 x1 x2   =   ap ap x2 x0 x1
                    let v = app(app(*x.clone(), xs[0].clone()), xs[1].clone());
                    eval(&v, env)
                }
                Val(Cons(xs)) => {
                    let mut args = xs.clone();
                    args.push(*x.clone());
                    Ok(Val(Cons(args)))
                }
                // B-Combinator
                Val(BComb(xs)) if xs.len() == 2 => {
                    // ap ap ap b x0 x1 x2   =   ap x0 ap x1 x2
                    let v = app(xs[0].clone(), app(xs[1].clone(), *x.clone()));
                    eval(&v, env)
                }
                Val(BComb(xs)) => {
                    assert!(xs.len() < 2);
                    let mut args = xs.clone();
                    args.push(*x.clone());
                    Ok(Val(BComb(args)))
                }
                // C-Combinator
                Val(CComb(xs)) if xs.len() == 2 => {
                    // ap ap ap c x0 x1 x2   =   ap ap x0 x2 x1
                    let v = app(app(xs[0].clone(), *x.clone()), xs[1].clone());
                    eval(&v, env)
                }
                Val(CComb(xs)) => {
                    assert!(xs.len() < 2);
                    let mut args = xs.clone();
                    args.push(*x.clone());
                    Ok(Val(CComb(args)))
                }
                // S-Combinator
                Val(SComb(xs)) if xs.len() == 2 => {
                    // ap ap ap s x0 x1 x2   =   ap ap x0 x2 ap x1 x2
                    let v = app(
                        app(xs[0].clone(), *x.clone()),
                        app(xs[1].clone(), *x.clone()),
                    );
                    eval(&v, env)
                }
                Val(SComb(xs)) => {
                    assert!(xs.len() < 2);
                    let mut args = xs.clone();
                    args.push(*x.clone());
                    Ok(Val(SComb(args)))
                }
                // I-Combinator
                Val(IComb) => {
                    // ap i x0   =   x0
                    eval(&x, env)
                }
                // True
                Val(True(xs)) if xs.len() == 1 => {
                    // ap ap t x0 x1   =   x0
                    let x0 = xs[0].clone();
                    Ok(x0)
                }
                Val(True(xs)) => {
                    assert_eq!(xs.len(), 0);
                    let mut args = xs.clone();
                    args.push(*x.clone());
                    Ok(Val(True(args)))
                }
                // False
                Val(False(xs)) if xs.len() == 1 => {
                    // ap ap f x0 x1   =   x1
                    Ok(*x.clone())
                }
                Val(False(xs)) => {
                    assert_eq!(xs.len(), 0);
                    let mut args = xs.clone();
                    args.push(*x.clone());
                    Ok(Val(False(args)))
                }
                // Sum (Add)
                Val(Sum(xs)) if xs.len() == 1 => {
                    let x_num = eval(&xs[0], env)?
                        .get_number()
                        .ok_or_else(|| NumberIsExpected(*x.clone()))?;
                    let x_den = eval(&x, env)?
                        .get_number()
                        .ok_or_else(|| NumberIsExpected(*x.clone()))?;
                    Ok(Val(Number(x_num + x_den)))
                }
                Val(Sum(xs)) => {
                    assert_eq!(xs.len(), 0);
                    let args = vec![*x.clone()];
                    Ok(Val(Sum(args)))
                }
                // Product
                Val(Prod(xs)) if xs.len() == 1 => {
                    let x_num = eval(&xs[0], env)?
                        .get_number()
                        .ok_or_else(|| NumberIsExpected(*x.clone()))?;
                    let x_den = eval(&x, env)?
                        .get_number()
                        .ok_or_else(|| NumberIsExpected(*x.clone()))?;
                    Ok(Val(Number(x_num * x_den)))
                }
                Val(Prod(xs)) => {
                    assert_eq!(xs.len(), 0);
                    let args = vec![*x.clone()];
                    Ok(Val(Prod(args)))
                }
                Val(Neg) => {
                    let x = eval(&x, env)?;
                    let x = x.get_number().ok_or_else(|| NumberIsExpected(x))?;
                    Ok(Val(Number(-x)))
                }
                // Div
                Val(Div(xs)) if xs.len() == 1 => {
                    let x_num = eval(&xs[0], env)?
                        .get_number()
                        .ok_or_else(|| NumberIsExpected(*x.clone()))?;
                    let x_den = eval(&x, env)?
                        .get_number()
                        .ok_or_else(|| NumberIsExpected(*x.clone()))?;
                    Ok(Val(Number(x_num / x_den)))
                }
                Val(Div(xs)) => {
                    assert_eq!(xs.len(), 0);
                    let args = vec![*x.clone()];
                    Ok(Val(Div(args)))
                }
                Val(Nil) => Ok(Val(True(vec![]))),
                Val(IsNil) => {
                    let e = eval(&x, env)?;
                    match e {
                        Val(Nil) => Ok(Val(True(vec![]))),
                        Val(Cons(_)) => Ok(Val(False(vec![]))),
                        _ => Err(ListIsExpected(e)),
                    }
                }
                // Less
                Val(Less(xs)) if xs.len() == 1 => {
                    let x0 = xs[0]
                        .get_number()
                        .ok_or_else(|| NumberIsExpected(xs[0].clone()))?;
                    let x1 = eval(&x, env)?
                        .get_number()
                        .ok_or_else(|| NumberIsExpected(*x.clone()))?;

                    if x0 < x1 {
                        Ok(Val(True(vec![])))
                    } else {
                        Ok(Val(False(vec![])))
                    }
                }
                Val(Less(xs)) => {
                    assert_eq!(xs.len(), 0);
                    let args = vec![*x.clone()];
                    Ok(Val(Less(args)))
                }
                // BigEq
                Val(BigEq(xs)) if xs.len() == 1 => {
                    let x0 = xs[0]
                        .get_number()
                        .ok_or_else(|| NumberIsExpected(xs[0].clone()))?;
                    let x1 = eval(&x, env)?
                        .get_number()
                        .ok_or_else(|| NumberIsExpected(*x.clone()))?;

                    if x0 == x1 {
                        Ok(Val(True(vec![])))
                    } else {
                        Ok(Val(False(vec![])))
                    }
                }
                Val(BigEq(xs)) => {
                    assert_eq!(xs.len(), 0);
                    let args = vec![*x.clone()];
                    Ok(Val(BigEq(args)))
                }
                t => {
                    dbg!(t);
                    Err(Todo)
                }
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    fn empty_env() -> HashMap<i128, TypedExpr> {
        HashMap::new()
    }

    fn neg(e: TypedExpr) -> TypedExpr {
        let n = TypedExpr::Val(TypedSymbol::Neg);
        app(n, e)
    }

    fn div(e1: TypedExpr, e2: TypedExpr) -> TypedExpr {
        let d = TypedExpr::Val(TypedSymbol::Div(vec![]));
        app(app(d, e1), e2)
    }

    fn less(e1: TypedExpr, e2: TypedExpr) -> TypedExpr {
        let l = TypedExpr::Val(TypedSymbol::Less(vec![]));
        app(app(l, e1), e2)
    }

    fn variable(x: i128) -> TypedExpr {
        TypedExpr::Val(TypedSymbol::Variable(x))
    }

    fn sum() -> TypedExpr {
        TypedExpr::Val(TypedSymbol::Sum(vec![]))
    }

    fn big_eq(x1: TypedExpr, x2: TypedExpr) -> TypedExpr {
        let eq = TypedExpr::Val(TypedSymbol::BigEq(vec![]));
        app(app(eq, x1), x2)
    }

    fn t() -> TypedExpr {
        TypedExpr::Val(TypedSymbol::True(vec![]))
    }

    fn f() -> TypedExpr {
        TypedExpr::Val(TypedSymbol::False(vec![]))
    }

    #[test]
    fn test_neg() {
        let exp = neg(number(5));
        let e = eval(&exp, &empty_env()).unwrap();
        assert_eq!(e, number(-5))
    }

    #[test]
    fn test_div() {
        let exp = div(number(5), number(2));
        let e = eval(&exp, &empty_env()).unwrap();
        assert_eq!(e, number(2))
    }

    #[test]
    fn test_div_numerator_minus() {
        // ap ap div -5 3   =   -1
        let exp = div(number(-5), number(3));
        let e = eval(&exp, &empty_env()).unwrap();
        assert_eq!(e, number(-1))
    }

    #[test]
    fn test_div_denominator_minus() {
        // ap ap div 5 -3   =   -1
        let exp = div(number(5), number(-3));
        let e = eval(&exp, &empty_env()).unwrap();
        assert_eq!(e, number(-1))
    }

    #[test]
    fn test_div_num_den_minus() {
        // ap ap div -5 -3   =   1
        let exp = div(number(-5), number(-3));
        let e = eval(&exp, &empty_env()).unwrap();
        assert_eq!(e, number(1))
    }

    #[test]
    fn test_less() {
        use TypedExpr::Val;

        // ap ap lt 0 -1   =   f
        let exp0 = less(number(0), number(-1));
        let e0 = eval(&exp0, &empty_env()).unwrap();
        assert_eq!(e0, Val(TypedSymbol::False(vec![])));

        // ap ap lt 0 0   =   f
        let exp1 = less(number(0), number(0));
        let e1 = eval(&exp1, &empty_env()).unwrap();
        assert_eq!(e1, Val(TypedSymbol::False(vec![])));

        // ap ap lt 0 1   =   t
        let exp2 = less(number(0), number(1));
        let e2 = eval(&exp2, &empty_env()).unwrap();
        assert_eq!(e2, Val(TypedSymbol::True(vec![])));
    }

    #[test]
    fn test_cons() {
        let pair = cons(number(1), nil());
        let x = app(car(), pair);

        let e = eval(&x, &empty_env()).unwrap();
        assert_eq!(e, number(1));
    }

    #[test]
    fn test_cons_galaxy_line1() {
        use TypedSymbol::*;

        // ap ap cons 7 ap ap cons 123 nil
        let x = cons(number(7), cons(number(123), nil()));

        let expected = val(Cons(vec![
            val(Number(7)),
            app(app(val(Cons(vec![])), val(Number(123))), val(Nil)),
        ]));

        assert_eq!(expected, eval(&x, &empty_env()).unwrap());
    }

    #[test]
    fn test_variable() {
        use TypedExpr::*;
        use TypedSymbol::*;
        // var1 = 123
        // ap ap cons 0 ap ap cons var1 2
        let mut env = HashMap::new();
        env.insert(1, number(123));
        let e = cons(number(0), cons(variable(1), number(2)));
        let expected = Val(Cons(vec![
            Val(Number(0)),
            app(app(val(Cons(vec![])), val(Variable(1))), val(Number(2))),
        ]));

        assert_eq!(expected, eval(&e, &env).unwrap());
    }

    #[test]
    fn test_variable_func() {
        use TypedExpr::*;
        use TypedSymbol::*;
        // var1 = cons
        // ap ap cons 0 ap ap var1 1 2
        let mut env = HashMap::new();
        env.insert(1, Val(Cons(vec![])));
        let e = cons(number(0), app(app(variable(1), number(1)), number(2)));
        let expected = Val(Cons(vec![
            Val(Number(0)),
            app(app(val(Variable(1)), val(Number(1))), val(Number(2))),
        ]));

        assert_eq!(expected, eval(&e, &env).unwrap());
    }

    #[test]
    fn test_variable_func_with_var() {
        use TypedExpr::*;
        use TypedSymbol::*;
        // var1 = cons 1
        // ap ap cons 0 ap var1 2
        let mut env = HashMap::new();
        env.insert(1, Val(Cons(vec![Val(Number(1))])));
        let e = cons(number(0), app(variable(1), number(2)));
        let expected = Val(Cons(vec![
            Val(Number(0)),
            app(val(Variable(1)), val(Number(2))),
        ]));

        assert_eq!(expected, eval(&e, &env).unwrap());
    }

    #[test]
    fn test_sum() {
        let env = HashMap::new();
        let expr = app(app(sum(), number(1)), number(2));
        assert_eq!(number(3), eval(&expr, &env).unwrap());
    }

    #[test]
    fn test_is_nil() {
        use TypedExpr::*;
        use TypedSymbol::*;

        let env = HashMap::new();
        let expr = app(Val(IsNil), nil());
        assert_eq!(t(), eval(&expr, &env).unwrap());
        let expr = app(Val(IsNil), cons(number(1), nil()));
        assert_eq!(f(), eval(&expr, &env).unwrap());
    }

    #[test]
    fn test_big_eq() {
        let env = HashMap::new();
        let expr = big_eq(number(1), number(1));
        assert_eq!(t(), eval(&expr, &env).unwrap());
        let expr = big_eq(number(1), number(0));
        assert_eq!(f(), eval(&expr, &env).unwrap());
    }
}
