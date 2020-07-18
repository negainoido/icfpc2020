use std::collections::HashMap;

use crate::expr::Expr;
use crate::symbol::Symbol;
use crate::typing::{TypedExpr, ExprNode};
use crate::expr;
use crate::eval::Evaluator;
use std::cell::RefCell;
use typed_arena::Arena;
use std::ops::Deref;
use std::fmt::{Debug, Formatter};

pub struct Task<'a> {
    pub variable_to_expr_map: HashMap<i128, Expr>,
    pub target: Expr,
    evaluator: Evaluator<'a>,
    pool: Arena<TypedExpr<'a>>,
}

impl<'a> Debug for Task<'a> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.target.fmt(f);
        self.variable_to_expr_map.fmt(f);
        Ok(())
    }
}

impl<'a> Task<'a> {
    pub fn new(input: &[String]) -> Self {
        let input_body = &input[..input.len() - 1];
        let input_target = input.last().unwrap();

        let mut variable_to_expr_map = HashMap::new();
        for input_stmt_str in input_body {
            let symbols: Vec<Symbol> = Task::string_to_symbols(input_stmt_str, "galaxy");
            let variable_id: i128 = match symbols[0] {
                Symbol::Variable(x) => x,
                _ => panic!(),
            };
            assert_eq!(symbols[1], Symbol::Eq);

            variable_to_expr_map.insert(variable_id, expr::parse(&symbols[2..].to_vec()));
        }

        let target_symbols = Task::string_to_symbols(input_target, "galaxy");
        assert_eq!(target_symbols[0], Symbol::Target);
        assert_eq!(target_symbols[1], Symbol::Eq);
        let pool = Arena::new();
        let evaluator = Evaluator::new();
        Task {
            variable_to_expr_map,
            target: expr::parse(&target_symbols[2..].to_vec()),
            pool,
            evaluator,
        }
    }

    // Currently, it works only when target statement doesn't contain any variable
    pub fn solve(&'a self) -> ExprNode<'a> {
        let mut env = self
            .variable_to_expr_map
            .iter()
            .map(|(k, v)| {
                let v:&TypedExpr = self.pool.alloc(TypedExpr::typing(&v).unwrap());
                (
                    *k,
                    v,
                )
            })
            .collect();
        let target_expr = self.pool.alloc(TypedExpr::typing(&self.target).unwrap());
        self.evaluator.eval(target_expr, &mut env).unwrap()
    }

    fn string_to_symbols(s: &str, target: &str) -> Vec<Symbol> {
        s.split_whitespace()
            .map(|s| Symbol::from_text(s, target))
            .collect()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    #[test]
    fn test() {
        let input = vec![
            ":1029 = ap ap cons 7 ap ap cons 123229502148636 nil".to_string(),
            ":1032 = ap ap cons 7 ap ap cons 560803991675135 nil".to_string(),
            "galaxy = :1032".to_string(),
        ];

        let task = Task::new(&input[..]);
        let mut expected_expr_map = HashMap::new();
        expected_expr_map.insert(
            1029,
            expr::parse(&vec![
                Symbol::App,
                Symbol::App,
                Symbol::Cons,
                Symbol::Number(7),
                Symbol::App,
                Symbol::App,
                Symbol::Cons,
                Symbol::Number(123229502148636),
                Symbol::Nil,
            ]),
        );
        expected_expr_map.insert(
            1032,
            expr::parse(&vec![
                Symbol::App,
                Symbol::App,
                Symbol::Cons,
                Symbol::Number(7),
                Symbol::App,
                Symbol::App,
                Symbol::Cons,
                Symbol::Number(560803991675135),
                Symbol::Nil,
            ]),
        );

        assert_eq!(task.variable_to_expr_map, expected_expr_map);
        assert_eq!(expr::parse(&vec![Symbol::Variable(1032)]), task.target);
}
}
