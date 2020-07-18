use crate::typing::raku::cons;
use crate::typing::{TypedExpr, TypedSymbol};

#[derive(Debug, Eq, PartialEq)]
pub enum List {
    Cons(Box<List>, Box<List>),
    Integer(i128),
    Nil,
}

fn do_demodulate(a: &str) -> (&str, List) {
    let prefix = &a[..2];
    if prefix == "11" {
        let (a, car) = do_demodulate(&a[2..]);
        let (a, cdr) = do_demodulate(a);
        return (a, List::Cons(Box::new(car), Box::new(cdr)));
    } else if prefix == "00" {
        return (&a[2..], List::Nil);
    } else {
        let sign = if prefix == "01" { 1 } else { -1 };
        let a = &a[2..];
        let mut len = 0;
        for c in a.chars() {
            if c == '0' {
                break;
            }
            len += 1;
        }
        let a = &a[len + 1..];
        len *= 4;
        if len == 0 {
            return (a, List::Integer(0));
        }
        let res = i128::from_str_radix(&a[0..len], 2);
        let num = match res {
            Ok(b) => b,
            Err(e) => {
                eprintln!("error while demodulating: {}", &a);
                panic!(e)
            }
        };
        let a = &a[len..];
        return (a, List::Integer(sign * num));
    }
}

fn modulate_number(value: i128) -> String {
    let mut res = String::new();
    if value >= 0 {
        res.push_str("01");
    } else {
        res.push_str("10");
    }
    let value = value.abs();
    let mut width = 0;
    while value >= 1 << width {
        width += 4;
        res.push('1');
    }
    res.push('0');
    for i in (0..width).rev() {
        if value & (1 << i) > 0 {
            res.push('1');
        } else {
            res.push('0');
        }
    }

    res
}

fn do_modulate(expr: &TypedExpr, str: &mut String) {
    match expr {
        TypedExpr::Apply(l, r) => {
            do_modulate(l, str);
            do_modulate(r, str);
        }
        TypedExpr::Val(symbol) => match symbol {
            TypedSymbol::Cons(exprs) => {
                str.push_str("11");
                for e in exprs {
                    do_modulate(e, str);
                }
            }
            TypedSymbol::Number(v) => {
                str.push_str(&modulate_number(*v));
            }
            TypedSymbol::Nil => {
                str.push_str("00");
            }
            _ => {
                unimplemented!();
            }
        },
    }
}

pub fn modulate(expr: &TypedExpr) -> String {
    let mut result = String::new();

    do_modulate(expr, &mut result);
    result
}

fn convert_to_expr(list: &List) -> TypedExpr {
    match list {
        List::Nil => TypedExpr::Val(TypedSymbol::Nil),
        List::Integer(i) => TypedExpr::Val(TypedSymbol::Number(*i)),
        List::Cons(l, r) => {
            let l = convert_to_expr(&l);
            let r = convert_to_expr(&r);
            cons(l, r)
        }
    }
}

pub fn demodulate(a: &str) -> TypedExpr {
    if a == "11" {
        return TypedExpr::Val(TypedSymbol::Nil);
    }
    let (_, l) = do_demodulate(a);
    return convert_to_expr(&l);
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::{eval, EvalError};
    use crate::typing::raku::{number, NIL};
    use std::collections::HashMap;

    #[test]
    fn demodulate_simple() {
        let request = "1101000";
        let expr = demodulate(request);
        let expected_expr = cons(number(0), NIL);
        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn modulate_simple() -> std::result::Result<(), EvalError> {
        let expr = cons(number(0), NIL);
        let result = modulate(&expr);
        let expected = "1101000";
        assert_eq!(result, expected);

        let env = HashMap::new();
        let expr = eval(&expr, &env)?;
        let result = modulate(&expr);
        assert_eq!(result, expected);
        Ok(())
    }

    #[test]
    fn modulate_number() {
        let expected = "01100001";
        assert_eq!(modulate(&number(1)), expected);
        let expected = "10100010";
        assert_eq!(modulate(&number(-2)), expected);
        let expected = "0111000100010";
        assert_eq!(modulate(&number(34)), expected);
    }

    #[test]
    fn examples() {
        let request = "00";
        let expected_expr = TypedExpr::Val(TypedSymbol::Nil);
        assert_eq!(modulate(&expected_expr), request);
        assert_eq!(demodulate(request), expected_expr);

        let request = "110000";
        let expected_expr = cons(NIL, NIL);
        assert_eq!(modulate(&expected_expr), request);
        assert_eq!(demodulate(request), expected_expr);

        let request = "1101100001111101100010110110001100110110010000";
        let expected_expr = cons(
            number(1),
            cons(cons(number(2), cons(number(3), NIL)), cons(number(4), NIL)),
        );
        assert_eq!(modulate(&expected_expr), request);
        assert_eq!(demodulate(request), expected_expr);
    }

    #[test]
    fn demodulate_complex() {
        let request = "110110000111011111100001001010100000110000";
        let expr = demodulate(request);
        let expected = cons(number(1), cons(number(76300), NIL));

        assert_eq!(expr, expected);
    }
}
