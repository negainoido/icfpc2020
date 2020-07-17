#![allow(dead_code)]

use crate::typing::*;

#[derive(Debug)]
pub enum EvalError {
    Todo,
}

fn app(e1: TypedExpr, e2: TypedExpr) -> TypedExpr {
    TypedExpr::Apply(Box::new(e1), Box::new(e2))
}

pub fn eval(expr: &TypedExpr) -> Result<TypedExpr, EvalError> {
    use EvalError::*;
    use TypedExpr::*;
    use TypedSymbol::*;

    match expr {
        Val(_) => Ok(expr.clone()),
        Apply(f, x) => {
            let f = eval(&f)?;
            let x = eval(&x)?;
            match f {
                // Car
                Val(Car) => {
                    // ap car x   =   ap x t
                    let v = app(x, Val(True(vec![])));
                    eval(&v)
                }
                // Cdr
                Val(Cdr) => {
                    // ap cdr x2   =   ap x2 f
                    let v = app(x, Val(False(vec![])));
                    eval(&v)
                }
                // Cons
                Val(Cons(xs)) if xs.len() == 2 => {
                    // ap ap ap cons x0 x1 x2   =   ap ap x2 x0 x1
                    let x0 = xs[0].clone();
                    let x1 = xs[1].clone();
                    let x2 = eval(&x)?;

                    let v = app(app(x2, x0), x1);
                    eval(&v)
                }
                Val(Cons(xs)) => {
                    let mut args = xs.clone();
                    let e = eval(&x)?;
                    args.push(e);
                    Ok(Val(Cons(args)))
                }
                // B-Combinator
                Val(BComb(xs)) if xs.len() == 2 => {
                    // ap ap ap b x0 x1 x2   =   ap x0 ap x1 x2
                    let x0 = xs[0].clone();
                    let x1 = xs[1].clone();
                    let x2 = eval(&x)?;

                    let v = app(x0, app(x1, x2));
                    eval(&v)
                }
                Val(BComb(xs)) => {
                    assert!(xs.len() < 2);
                    let mut args = xs.clone();
                    let e = eval(&x)?;
                    args.push(e);
                    Ok(Val(BComb(args)))
                }
                // C-Combinator
                Val(CComb(xs)) if xs.len() == 2 => {
                    // ap ap ap c x0 x1 x2   =   ap ap x0 x2 x1
                    let x0 = xs[0].clone();
                    let x1 = xs[1].clone();
                    let x2 = eval(&x)?;

                    let v = app(app(x0, x2), x1);
                    eval(&v)
                }
                Val(CComb(xs)) => {
                    assert!(xs.len() < 2);
                    let mut args = xs.clone();
                    let e = eval(&x)?;
                    args.push(e);
                    Ok(Val(CComb(args)))
                }
                // S-Combinator
                Val(SComb(xs)) if xs.len() == 2 => {
                    // ap ap ap s x0 x1 x2   =   ap ap x0 x2 ap x1 x2
                    let x0 = xs[0].clone();
                    let x1 = xs[1].clone();
                    let x2 = eval(&x)?;

                    let v = app(app(x0, x2.clone()), app(x1, x2));
                    eval(&v)
                }
                Val(SComb(xs)) => {
                    assert!(xs.len() < 2);
                    let mut args = xs.clone();
                    let e = eval(&x)?;
                    args.push(e);
                    Ok(Val(SComb(args)))
                }
                // I-Combinator
                Val(IComb) => {
                    // ap i x0   =   x0
                    eval(&x)
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
                    let e = eval(&x)?;
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
                    let e = eval(&x)?;
                    args.push(e);
                    Ok(Val(False(args)))
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

    #[test]
    fn test_cons() {
        let pair = cons(number(1), nil());
        let x = app(car(), pair);

        let e = eval(&x).unwrap();
        assert_eq!(e, number(1));
    }

    #[test]
    fn test_cons_galaxy_line1() {
        use TypedExpr::*;
        use TypedSymbol::*;

        // ap ap cons 7 ap ap cons 123229502148636 nil
        let x = cons(number(7), cons(number(123), nil()));

        let expected = Val(Cons(vec![
            Val(Number(7)),
            Val(Cons(vec![Val(Number(123)), Val(Nil)])),
        ]));
        assert_eq!(expected, eval(&x).unwrap());
    }
}
