use crate::expr::Expr;
use crate::symbol::Symbol;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypedSymbol {
    Number(i128),
    Nil,
    Cons(Vec<TypedExpr>),
    Car,
    Cdr,
    BComb(Vec<TypedExpr>),
    CComb(Vec<TypedExpr>),
    SComb(Vec<TypedExpr>),
    IComb,
    True(Vec<TypedExpr>),
    False(Vec<TypedExpr>),
}

impl TypedSymbol {
    pub fn typing(sym: &Symbol) -> Option<Self> {
        use TypedSymbol::*;

        match sym {
            Symbol::Number(i) => Some(Number(*i)),
            Symbol::Nil => Some(Nil),
            Symbol::Cons => Some(Cons(vec![])),
            Symbol::Car => Some(Car),
            Symbol::Cdr => Some(Cdr),
            Symbol::BComb => Some(BComb(vec![])),
            Symbol::CComb => Some(CComb(vec![])),
            Symbol::SComb => Some(SComb(vec![])),
            Symbol::IComb => Some(IComb),
            Symbol::True => Some(True(vec![])),
            Symbol::False => Some(False(vec![])),
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
