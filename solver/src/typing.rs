use crate::expr::Expr;
use crate::symbol::Symbol;
use std::cell::RefCell;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypedSymbol<'a> {
    Number(i128),
    Nil,
    Cons(Vec<ExprNode<'a>>),
    Car,
    Cdr,
    BComb(Vec<ExprNode<'a>>),
    CComb(Vec<ExprNode<'a>>),
    SComb(Vec<ExprNode<'a>>),
    IComb,
    True(Vec<ExprNode<'a>>),
    False(Vec<ExprNode<'a>>),
    Variable(i128),
    Neg,
    Sum(Vec<ExprNode<'a>>),
    Prod(Vec<ExprNode<'a>>),
    Div(Vec<ExprNode<'a>>),
    Less(Vec<ExprNode<'a>>),
    IsNil,
    BigEq(Vec<ExprNode<'a>>),
}

impl<'a> TypedSymbol<'a> {
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

pub type ExprNode<'a> = &'a TypedExpr<'a>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypedExpr<'a> {
    Apply(ExprNode<'a>, ExprNode<'a>),
    Val(TypedSymbol<'a>),
}

impl<'a> TypedExpr<'a> {
    pub fn typing(_expr: &Expr) -> Option<Self> {
        unimplemented!();
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
    pub fn app<'a>(e1: ExprNode<'a>, e2: ExprNode<'a>) -> TypedExpr<'a> {
        TypedExpr::Apply(e1, e2)
    }
    pub fn val<'a>(sym: TypedSymbol<'a>) -> TypedExpr<'a> {
        TypedExpr::Val(sym)
    }
    pub const NIL: TypedExpr<'static> = Val(Nil);
    pub const CONS: TypedExpr<'static> = Val(Cons(Vec::new()));
    pub const CAR: TypedExpr<'static> = Val(Car);
    pub const CDR: TypedExpr<'static> = Val(Cdr);
    pub const BCOMB: TypedExpr<'static> = Val(BComb(Vec::new()));
    pub const CCOMB: TypedExpr<'static> = Val(CComb(Vec::new()));
    pub const ICOMB: TypedExpr<'static> = Val(IComb);
    pub const SUM: TypedExpr<'static> = Val(Sum(Vec::new()));
    pub const NEG: TypedExpr<'static> = Val(Neg);
    pub fn cons<'a>(e1: ExprNode<'a>, e2: ExprNode<'a>) -> TypedExpr<'a> {
        app(&app(&val(Cons(vec![])), e1), e2)
    }
    pub fn isnil<'a>(x: ExprNode<'a>) -> TypedExpr<'a> {
        app(&val(IsNil), x)
    }
    pub fn number<'a>(x: i128) -> TypedExpr<'a> {
        val(Number(x))
    }
    pub fn neg<'a>(e: ExprNode<'a>) -> TypedExpr<'a> {
        app(&Val(Neg), e)
    }
    pub fn div<'a>(e1: ExprNode<'a>, e2: ExprNode<'a>) -> TypedExpr<'a> {
        app(&app(&Val(Div(vec![])), e1), e2)
    }
    pub fn less<'a>(e1: ExprNode<'a>, e2: ExprNode<'a>) -> TypedExpr<'a> {
        app(&app(&Val(Less(vec![])), e1), e2)
    }
    pub fn variable<'a>(x: i128) -> TypedExpr<'a> {
        Val(Variable(x))
    }
    pub fn sum<'a>(x: ExprNode<'a>, y: ExprNode<'a>) -> TypedExpr<'a> {
        app(&app(&val(Sum(vec![])), x), y)
    }
    pub fn big_eq<'a>(x1: ExprNode<'a>, x2: ExprNode<'a>) -> TypedExpr<'a> {
        let eq = Val(BigEq(vec![]));
        app(&app(&eq, x1), x2)
    }
    pub const T: TypedExpr<'static> = Val(True(Vec::new()));
    pub const F: TypedExpr<'static> = Val(False(Vec::new()));
}
