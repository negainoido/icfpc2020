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
    Variable(i128),
    Neg,
    Sum(Vec<TypedExpr>),
    Prod(Vec<TypedExpr>),
    Div(Vec<TypedExpr>),
    Less(Vec<TypedExpr>),
    IsNil,
    BigEq(Vec<TypedExpr>),
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
            Symbol::Variable(i) => Some(Variable(*i)),
            Symbol::Sum => Some(Sum(vec![])),
            Symbol::Prod => Some(Prod(vec![])),

            Symbol::Neg => Some(Neg),
            Symbol::Div => Some(Div(vec![])),
            Symbol::Less => Some(Less(vec![])),
            Symbol::IsNil => Some(IsNil),
            Symbol::BigEq => Some(BigEq(vec![])),
            _ => todo!("todo: {:?}", sym),
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

    pub fn get_number(&self) -> Option<i128> {
        match self {
            TypedExpr::Val(TypedSymbol::Number(x)) => Some(*x),
            _ => None,
        }
    }
}

pub mod raku {
    use super::TypedExpr::*;
    use super::TypedSymbol::*;
    use super::*;
    pub fn app(e1: TypedExpr, e2: TypedExpr) -> TypedExpr {
        TypedExpr::Apply(Box::new(e1), Box::new(e2))
    }
    pub fn val(sym: TypedSymbol) -> TypedExpr {
        TypedExpr::Val(sym)
    }
    pub const NIL: TypedExpr = Val(Nil);
    pub const CONS: TypedExpr = Val(Cons(vec![]));
    pub const CAR: TypedExpr = Val(Car);
    pub const CDR: TypedExpr = Val(Cdr);
    pub const BCOMB: TypedExpr = Val(BComb(vec![]));
    pub const CCOMB: TypedExpr = Val(CComb(vec![]));
    pub const ICOMB: TypedExpr = Val(IComb);
    pub const SUM: TypedExpr = Val(Sum(vec![]));
    pub const NEG: TypedExpr = Val(Neg);
    pub fn cons(e1: TypedExpr, e2: TypedExpr) -> TypedExpr {
        app(app(val(Cons(vec![])), e1), e2)
    }
    pub fn isnil(x: TypedExpr) -> TypedExpr {
        app(val(IsNil), x)
    }
    pub fn number(x: i128) -> TypedExpr {
        val(Number(x))
    }
    pub fn neg(e: TypedExpr) -> TypedExpr {
        app(Val(Neg), e)
    }
    pub fn div(e1: TypedExpr, e2: TypedExpr) -> TypedExpr {
        app(app(Val(Div(vec![])), e1), e2)
    }
    pub fn less(e1: TypedExpr, e2: TypedExpr) -> TypedExpr {
        app(app(Val(Less(vec![])), e1), e2)
    }
    pub fn variable(x: i128) -> TypedExpr {
        Val(Variable(x))
    }
    pub fn sum(x: TypedExpr, y: TypedExpr) -> TypedExpr {
        app(app(val(Sum(vec![])), x), y)
    }
    pub fn big_eq(x1: TypedExpr, x2: TypedExpr) -> TypedExpr {
        let eq = Val(BigEq(vec![]));
        app(app(eq, x1), x2)
    }
    pub const T: TypedExpr = Val(True(vec![]));
    pub const F: TypedExpr = Val(False(vec![]));
}
