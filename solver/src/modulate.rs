use crate::eval::Evaluator;
use crate::typing::{ExprNode, TypedExpr, TypedSymbol};

#[derive(Debug, Eq, PartialEq)]
pub enum List {
    Cons(Box<List>, Box<List>),
    Integer(i128),
    Nil,
}

pub fn cons(car: List, cdr: List) -> List {
    List::Cons(Box::new(car), Box::new(cdr))
}

impl From<Vec<i128>> for List {
    fn from(v: Vec<i128>) -> Self {
        let mut ret = Self::Nil;
        for x in v.iter().rev() {
            ret = cons(Self::Integer(*x), ret);
        }
        ret
    }
}

pub fn modulate_number(n: i128) -> String {
    if n == 0 {
        return "010".to_string();
    }
    let mut ret: String = String::from(if n < 0 { "10" } else { "01" });
    let n = n.abs();
    let mut binary: String = format!("{:b}", n);
    while binary.len() % 4 != 0 {
        binary = "0".to_string() + &binary;
    }
    for _ in 0..binary.len() / 4 {
        ret += "1";
    }
    ret += "0";
    ret + &binary
}

impl List {
    fn do_modulate(expr: &Self, s: &mut String) {
        use List::*;
        match expr {
            Cons(car, cdr) => {
                s.push_str("11");
                Self::do_modulate(&car, s);
                Self::do_modulate(&cdr, s);
            }
            Integer(v) => {
                s.push_str(&modulate_number(*v));
            }
            Nil => {
                s.push_str("00");
            }
        }
    }

    pub fn modulate(&self) -> String {
        let mut result = String::new();
        Self::do_modulate(self, &mut result);
        result
    }

    fn do_demodulate(a: &str) -> (&str, List) {
        let prefix = &a[..2];
        if prefix == "11" {
            let (a, car) = Self::do_demodulate(&a[2..]);
            let (a, cdr) = Self::do_demodulate(a);
            (a, List::Cons(Box::new(car), Box::new(cdr)))
        } else if prefix == "00" {
            (&a[2..], List::Nil)
        } else {
            let sign = if prefix == "01" { 1 } else { -1 };
            let a = &a[2..];
            let len = a.find('0').unwrap();
            let a = &a[len + 1..];
            let len = len * 4;
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
            (a, List::Integer(sign * num))
        }
    }

    pub fn demodulate(s: &str) -> Option<Self> {
        if s == "11" {
            return Some(Self::Nil);
        }
        if let ("", l) = Self::do_demodulate(s) {
            Some(l)
        } else {
            None
        }
    }
}

impl<'a> From<ExprNode<'a>> for List {
    fn from(expr: ExprNode<'a>) -> Self {
        match expr {
            TypedExpr::Apply(TypedExpr::Apply(TypedExpr::Val(TypedSymbol::Cons(l)), car), cdr)
                if l.len() == 0 =>
            {
                let car: List = Self::from(*car);
                let cdr: List = Self::from(*cdr);
                List::Cons(Box::new(car), Box::new(cdr))
            }
            TypedExpr::Val(symbol) => match symbol {
                TypedSymbol::Number(v) => List::Integer(*v),
                TypedSymbol::Nil => List::Nil,
                TypedSymbol::Cons(l) => {
                    assert_eq!(l.len(), 2);
                    let car: List = Self::from(l[0]);
                    let cdr: List = Self::from(l[1]);
                    List::Cons(Box::new(car), Box::new(cdr))
                }
                _ => unimplemented!("{:?}", symbol),
            },
            _ => unimplemented!("{:?}", expr),
        }
    }
}

pub fn modulate(expr: ExprNode) -> String {
    let l = List::from(expr);
    l.modulate()
}

fn convert_to_expr<'a>(list: &List, sim: &'a Evaluator<'a>) -> ExprNode<'a> {
    match list {
        List::Nil => sim.get_val(TypedSymbol::Nil),
        List::Integer(i) => sim.get_val(TypedSymbol::Number(*i)),
        List::Cons(l, r) => {
            let l = convert_to_expr(&l, sim);
            let r = convert_to_expr(&r, sim);
            sim.get_cons(l, r)
        }
    }
}

pub fn demodulate<'a>(a: &str, sim: &'a Evaluator<'a>) -> ExprNode<'a> {
    if a == "11" {
        return sim.get_val(TypedSymbol::Nil);
    }
    let l = List::demodulate(a).expect("failed demodulating");
    convert_to_expr(&l, sim)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::eval::static_expr::*;
    use crate::eval::Evaluator;
    use std::collections::HashMap;

    #[test]
    fn demodulate_simple() {
        let eval = Evaluator::new();
        let request = "1101000";
        let expr = demodulate(request, &eval);
        let expected_expr = eval.get_cons(eval.get_number(0), NIL);
        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn modulate_simple() {
        let eval = Evaluator::new();
        let expr = eval.get_cons(eval.get_number(0), NIL);
        let result = modulate(&expr);
        let expected = "1101000";
        assert_eq!(result, expected);

        let mut env = HashMap::new();
        let expr = eval.eval(expr, &mut env).unwrap();
        let result = modulate(expr);
        assert_eq!(result, expected);
    }

    #[test]
    fn modulate_number() {
        let eval = Evaluator::new();
        let expected = "01100001";
        assert_eq!(modulate(eval.get_number(1)), expected);
        let expected = "10100010";
        assert_eq!(modulate(eval.get_number(-2)), expected);
        let expected = "0111000100010";
        assert_eq!(modulate(eval.get_number(34)), expected);
    }

    #[test]
    fn examples() {
        let eval = Evaluator::new();
        let request = "00";
        let expected_expr = TypedExpr::Val(TypedSymbol::Nil);
        assert_eq!(modulate(&expected_expr), request);
        assert_eq!(demodulate(request, &eval), &expected_expr);

        let request = "110000";
        let expected_expr = eval.get_cons(NIL, NIL);
        assert_eq!(modulate(&expected_expr), request);
        assert_eq!(demodulate(request, &eval), expected_expr);

        let request = "1101100001111101100010110110001100110110010000";
        let expected_expr = eval.get_cons(
            eval.get_number(1),
            eval.get_cons(
                eval.get_cons(eval.get_number(2), eval.get_cons(eval.get_number(3), NIL)),
                eval.get_cons(eval.get_number(4), NIL),
            ),
        );
        assert_eq!(modulate(&expected_expr), request);
        assert_eq!(demodulate(request, &eval), expected_expr);
    }

    #[test]
    fn demodulate_complex() {
        let eval = Evaluator::new();
        let request = "110110000111011111100001001010100000110000";
        let expr = demodulate(request, &eval);
        let expected = eval.get_cons(
            eval.get_number(1),
            eval.get_cons(eval.get_number(76300), NIL),
        );

        assert_eq!(expr, expected);
    }
}
