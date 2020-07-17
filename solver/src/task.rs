use std::collections::HashMap;

use crate::expr::Expr;
use crate::symbol::Symbol;
use crate::typing::TypedExpr;
use crate::{eval, expr};

#[derive(Debug, PartialEq, Eq)]
pub struct Task {
    pub variable_to_expr_map: HashMap<i128, Expr>,
    pub target: Expr,
}

impl Task {
    pub fn new(input: &Vec<String>) -> Task {
        let input_body = &input[0..input.len() - 1];
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
        Task {
            variable_to_expr_map,
            target: expr::parse(&target_symbols[2..].to_vec()),
        }
    }

    // Currently, it works only when target statement doesn't contain any variable
    pub fn solve(&self) -> TypedExpr {
        let target_expr = TypedExpr::typing(&self.target).unwrap();
        eval::eval(&target_expr).unwrap()
    }

    fn string_to_symbols(s: &String, target: &str) -> Vec<Symbol> {
        return s
            .split_whitespace()
            .map(|s| Symbol::from_text(s.to_string(), &target.to_string()))
            .collect();
    }
}

#[test]
fn test() {
    let input = vec![
        ":1029 = ap ap cons 7 ap ap cons 123229502148636 nil".to_string(),
        ":1032 = ap ap cons 7 ap ap cons 560803991675135 nil".to_string(),
        "galaxy = :1032".to_string(),
    ];

    let task = Task::new(&input);
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

    assert_eq!(
        task,
        Task {
            variable_to_expr_map: expected_expr_map,
            target: expr::parse(&vec![Symbol::Variable(1032)]),
        }
    );
}
