use crate::expr::Expr;
use crate::symbol::Symbol;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypedSymbol {
    Cons(Vec<TypedExpr>),
    Nil,
    Car(Vec<TypedExpr>),
    Cdr(Vec<TypedExpr>),
    Number(i128),
}

impl TypedSymbol {
    pub fn typing(sym: &Symbol) -> Option<Self> {
        use TypedSymbol::*;

        match sym {
            Symbol::Cons => Some(Cons(vec![])),
            Symbol::Nil => Some(Nil),
            Symbol::Car => Some(Car(vec![])),
            Symbol::Cdr => Some(Cdr(vec![])),
            Symbol::Number(i) => Some(Number(*i)),
            _ => todo!("todo"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypedExpr {
    Apply(Box<TypedExpr>, Box<TypedExpr>),
    Val(TypedSymbol),
}

impl TypedExpr {
    pub fn typing(expr: &Expr) -> Option<Self> {
        use TypedExpr::*;

        match expr {
            Expr::Val(sym) => TypedSymbol::typing(sym).map(|s| Val(s)),
            Expr::Apply(e1, e2) => match (TypedExpr::typing(e1), TypedExpr::typing(e2)) {
                (Some(t1), Some(t2)) => Some(Apply(Box::new(t1), Box::new(t2))),
                _ => None,
            },
        }
    }
}
