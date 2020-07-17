#![allow(dead_code)]
use crate::typing::*;

#[derive(Debug)]
enum EvalError {
    Todo,
}

fn eval(expr: &TypedExpr) -> Result<TypedExpr, EvalError> {
    use EvalError::*;
    use TypedExpr::*;
    use TypedSymbol::*;

    dbg!(&expr);

    match expr {
        Val(_) => Ok(expr.clone()),
        Apply(f, x) => {
            let f = eval(&f)?;
            let x = eval(&x)?;
            match f {
                // Cons
                Val(Cons(xs)) if xs.len() == 2 => {
                    let e = eval(&x)?;
                    let f0 = Apply(Box::new(e), Box::new(xs[0].clone()));
                    let f1 = Apply(Box::new(f0), Box::new(xs[1].clone()));
                    eval(&f1)
                }
                Val(Cons(xs)) => {
                    let mut args = xs.clone();
                    let e = eval(&x)?;
                    args.push(e);
                    Ok(Val(Cons(args)))
                }
                // Car
                Val(Car(xs)) if xs.len() == 1 => Ok(xs[0].clone()),
                Val(Car(xs)) => {
                    assert!(xs.len() == 0);
                    let e = eval(&x)?;
                    Ok(Val(Car(vec![e])))
                }
                // Cdr
                Val(Cdr(xs)) if xs.len() == 1 => Ok(xs[1].clone()),
                Val(Cdr(xs)) => {
                    assert!(xs.len() == 0);
                    let e = eval(&x)?;
                    Ok(Val(Cdr(vec![e])))
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

    fn app(e1: TypedExpr, e2: TypedExpr) -> TypedExpr {
        TypedExpr::Apply(Box::new(e1), Box::new(e2))
    }

    fn number(x: i128) -> TypedExpr {
        TypedExpr::Val(TypedSymbol::Number(x))
    }

    fn nil() -> TypedExpr {
        TypedExpr::Val(TypedSymbol::Nil)
    }

    fn car() -> TypedExpr {
        TypedExpr::Val(TypedSymbol::Car(vec![]))
    }

    fn cons(e1: TypedExpr, e2: TypedExpr) -> TypedExpr {
        let c = TypedExpr::Val(TypedSymbol::Cons(vec![]));
        app(app(c, e1), e2)
    }

    #[test]
    fn test_cons() {
        let pair = cons(number(1), nil());
        let x = app(pair, car());

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
