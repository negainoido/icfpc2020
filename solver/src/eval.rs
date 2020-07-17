#![allow(dead_code)]

use crate::typing::*;
use std::collections::HashMap;

#[derive(Debug)]
pub enum EvalError {
    NumberIsExpected(TypedExpr),
    Todo,
}

fn app(e1: TypedExpr, e2: TypedExpr) -> TypedExpr {
    TypedExpr::Apply(Box::new(e1), Box::new(e2))
}

pub(crate) fn eval(
    expr: &TypedExpr,
    env: &HashMap<i128, TypedExpr>,
) -> Result<TypedExpr, EvalError> {
    use EvalError::*;
    use TypedExpr::*;
    use TypedSymbol::*;

    match expr {
        Val(Variable(i)) => Ok(env.get(i).unwrap().clone()),
        Val(_) => Ok(expr.clone()),
        Apply(f, x) => {
            let f = eval(&f, env)?;
            let x = eval(&x, env)?;
            match f {
                // Car
                Val(Car) => {
                    // ap car x   =   ap x t
                    let v = app(x, Val(True(vec![])));
                    eval(&v, env)
                }
                // Cdr
                Val(Cdr) => {
                    // ap cdr x2   =   ap x2 f
                    let v = app(x, Val(False(vec![])));
                    eval(&v, env)
                }
                // Cons
                Val(Cons(xs)) if xs.len() == 2 => {
                    // ap ap ap cons x0 x1 x2   =   ap ap x2 x0 x1
                    let x0 = xs[0].clone();
                    let x1 = xs[1].clone();
                    let x2 = eval(&x, env)?;

                    let v = app(app(x2, x0), x1);
                    eval(&v, env)
                }
                Val(Cons(xs)) => {
                    let mut args = xs.clone();
                    let e = eval(&x, env)?;
                    args.push(e);
                    Ok(Val(Cons(args)))
                }
                // B-Combinator
                Val(BComb(xs)) if xs.len() == 2 => {
                    // ap ap ap b x0 x1 x2   =   ap x0 ap x1 x2
                    let x0 = xs[0].clone();
                    let x1 = xs[1].clone();
                    let x2 = eval(&x, env)?;

                    let v = app(x0, app(x1, x2));
                    eval(&v, env)
                }
                Val(BComb(xs)) => {
                    assert!(xs.len() < 2);
                    let mut args = xs.clone();
                    let e = eval(&x, env)?;
                    args.push(e);
                    Ok(Val(BComb(args)))
                }
                // C-Combinator
                Val(CComb(xs)) if xs.len() == 2 => {
                    // ap ap ap c x0 x1 x2   =   ap ap x0 x2 x1
                    let x0 = xs[0].clone();
                    let x1 = xs[1].clone();
                    let x2 = eval(&x, env)?;

                    let v = app(app(x0, x2), x1);
                    eval(&v, env)
                }
                Val(CComb(xs)) => {
                    assert!(xs.len() < 2);
                    let mut args = xs.clone();
                    let e = eval(&x, env)?;
                    args.push(e);
                    Ok(Val(CComb(args)))
                }
                // S-Combinator
                Val(SComb(xs)) if xs.len() == 2 => {
                    // ap ap ap s x0 x1 x2   =   ap ap x0 x2 ap x1 x2
                    let x0 = xs[0].clone();
                    let x1 = xs[1].clone();
                    let x2 = eval(&x, env)?;

                    let v = app(app(x0, x2.clone()), app(x1, x2));
                    eval(&v, env)
                }
                Val(SComb(xs)) => {
                    assert!(xs.len() < 2);
                    let mut args = xs.clone();
                    let e = eval(&x, env)?;
                    args.push(e);
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
                    let e = eval(&x, env)?;
                    args.push(e);
                    Ok(Val(True(args)))
                }
                // False
                Val(False(xs)) if xs.len() == 1 => {
                    // ap ap f x0 x1   =   x1
                    let x1 = xs[1].clone();
                    Ok(x1)
                }
                Val(False(xs)) => {
                    assert_eq!(xs.len(), 0);
                    let mut args = xs.clone();
                    let e = eval(&x, env)?;
                    args.push(e);
                    Ok(Val(False(args)))
                }
                // Sum (Add)
                Val(Sum { arity, args }) => {
                    let x = eval(&x, env)?;
                    let n: i128 = x.get_number().ok_or_else(|| NumberIsExpected(x))?;

                    if args.len() + 1 == (arity as usize) {
                        let v = n + args.iter().sum::<i128>();
                        Ok(Val(Number(v)))
                    } else {
                        let mut args = args.clone();
                        args.push(n);
                        Ok(Val(Sum { arity, args }))
                    }
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

    fn number(x: i128) -> TypedExpr {
        TypedExpr::Val(TypedSymbol::Number(x))
    }

    fn nil() -> TypedExpr {
        TypedExpr::Val(TypedSymbol::Nil)
    }

    fn car() -> TypedExpr {
        TypedExpr::Val(TypedSymbol::Car)
    }

    fn cons(e1: TypedExpr, e2: TypedExpr) -> TypedExpr {
        let c = TypedExpr::Val(TypedSymbol::Cons(vec![]));
        app(app(c, e1), e2)
    }

    fn variable(x: i128) -> TypedExpr {
        TypedExpr::Val(TypedSymbol::Variable(x))
    }

    fn sum_n(arity: u32) -> TypedExpr {
        TypedExpr::Val(TypedSymbol::Sum {
            arity,
            args: vec![],
        })
    }

    #[test]
    fn test_cons() {
        let pair = cons(number(1), nil());
        let x = app(car(), pair);

        let e = eval(&x, &HashMap::new()).unwrap();
        assert_eq!(e, number(1));
    }

    #[test]
    fn test_cons_galaxy_line1() {
        use TypedExpr::*;
        use TypedSymbol::*;

        // ap ap cons 7 ap ap cons 123 nil
        let x = cons(number(7), cons(number(123), nil()));

        let expected = Val(Cons(vec![
            Val(Number(7)),
            Val(Cons(vec![Val(Number(123)), Val(Nil)])),
        ]));
        assert_eq!(expected, eval(&x, &HashMap::new()).unwrap());
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
            Val(Cons(vec![Val(Number(123)), Val(Number(2))])),
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
            Val(Cons(vec![Val(Number(1)), Val(Number(2))])),
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
            Val(Cons(vec![Val(Number(1)), Val(Number(2))])),
        ]));

        assert_eq!(expected, eval(&e, &env).unwrap());
    }

    #[test]
    fn test_sum() {
        let env = HashMap::new();
        let expr = app(app(app(sum_n(3), number(1)), number(2)), number(3));
        assert_eq!(number(6), eval(&expr, &env).unwrap());

        let expr = app(app(sum_n(2), number(-100)), number(101));
        assert_eq!(number(1), eval(&expr, &env).unwrap());
    }
}
