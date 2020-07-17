use crate::expr::Expr;
use crate::symbol::Symbol;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypedSymbol {
    Number(i128),
    Nil,
    Cons(Vec<TypedExpr>),
    Car(Vec<TypedExpr>),
    Cdr(Vec<TypedExpr>),
    BComb(Vec<TypedExpr>),
    CComb(Vec<TypedExpr>),
    SComb(Vec<TypedExpr>),
    IComb,
}

impl TypedSymbol {
    pub fn typing(sym: &Symbol) -> Option<Self> {
        use TypedSymbol::*;

        match sym {
            Symbol::Number(i) => Some(Number(*i)),
            Symbol::Nil => Some(Nil),
            Symbol::Cons => Some(Cons(vec![])),
            Symbol::Car => Some(Car(vec![])),
            Symbol::Cdr => Some(Cdr(vec![])),
            Symbol::BComb => Some(BComb(vec![])),
            Symbol::CComb => Some(CComb(vec![])),
            Symbol::SComb => Some(SComb(vec![])),
            Symbol::IComb => Some(IComb),
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
