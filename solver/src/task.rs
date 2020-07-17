use std::collections::HashMap;
use std::collections::VecDeque;

use crate::expr::Expr;
use crate::symbol::Symbol;
use crate::typing::TypedExpr;
use crate::{eval, expr};

#[derive(Debug, PartialEq, Eq)]
pub struct Task {
    pub variable_to_expr_map: HashMap<i128, Expr>,
    pub target: Expr,
    exec_order: Vec<i128>,
}

impl Task {
    pub fn new(input: &Vec<String>) -> Task {
        let input_body = &input[0..input.len() - 1];
        let input_target = input.last().unwrap();

        let mut variable_to_expr_map = HashMap::new();
        let mut depend = HashMap::<i128, Vec<i128>>::new();
        let mut variable_ids = Vec::<i128>::new();
        for input_stmt_str in input_body {
            let symbols: Vec<Symbol> = Task::string_to_symbols(input_stmt_str, "galaxy");
            let variable_id: i128 = match symbols[0] {
                Symbol::Variable(x) => x,
                _ => panic!(),
            };
            assert_eq!(symbols[1], Symbol::Eq);
            variable_ids.push(variable_id);
            for &sym in &symbols[2..] {
                match sym {
                    Symbol::Variable(x) => {
                        depend
                            .entry(x)
                            .and_modify(|e| {
                                e.push(variable_id);
                            })
                            .or_insert(vec![variable_id]);
                    }
                    _ => (),
                }
            }

            variable_to_expr_map.insert(variable_id, expr::parse(&symbols[2..].to_vec()));
        }

        let target_symbols = Task::string_to_symbols(input_target, "galaxy");
        assert_eq!(target_symbols[0], Symbol::Target);
        assert_eq!(target_symbols[1], Symbol::Eq);
        Task {
            variable_to_expr_map,
            target: expr::parse(&target_symbols[2..].to_vec()),
            exec_order: Task::execution_order(&variable_ids, &depend),
        }
    }

    // Currently, it works only when target statement doesn't contain any variable
    pub fn solve(&self) -> TypedExpr {
        let target_expr = TypedExpr::typing(&self.target).unwrap();
        eval::eval(&target_expr).unwrap()
    }

    fn execution_order(variable_ids: &Vec<i128>, depend: &HashMap<i128, Vec<i128>>) -> Vec<i128> {
        let mut in_degs = HashMap::<i128, usize>::new();
        for v in variable_ids {
            in_degs.insert(*v, 0);
        }
        for (_, vs) in depend.iter() {
            for v2 in vs {
                in_degs.entry(*v2).and_modify(|e| {
                    *e += 1;
                });
            }
        }
        let mut topo_order = Vec::<i128>::new();
        let mut queue = VecDeque::<i128>::new();
        for v in variable_ids {
            let cnt = in_degs.get(v).unwrap();
            if *cnt == 0 {
                queue.push_back(*v);
            }
        }

        while !queue.is_empty() {
            let v = queue.pop_front().unwrap();
            assert_eq!(in_degs[&v], 0);
            topo_order.push(v);
            for &vecs in &depend.get(&v) {
                for nv in vecs {
                    assert!(in_degs[&nv] > 0);
                    *in_degs.get_mut(&nv).unwrap() -= 1;
                    if in_degs[&nv] == 0 {
                        queue.push_back(*nv);
                    }
                }
            }
        }
        assert_eq!(topo_order.len(), variable_ids.len());

        topo_order
    }

    fn string_to_symbols(s: &String, target: &str) -> Vec<Symbol> {
        return s
            .split_whitespace()
            .map(|s| Symbol::from_text(s.to_string(), &target.to_string()))
            .collect();
    }
}

#[test]
fn order_test() {
    let input = vec![
        ":1 = ap ap add :2 :3".to_string(),
        ":2 = ap ap add :3 :4".to_string(),
        ":3 = ap ap add :4 :4".to_string(),
        ":4 = 123".to_string(),
        ":5 = ap ap add :4 :3".to_string(),
        ":6 = 234".to_string(),
        "galaxy = :6".to_string(),
    ];
    let task = Task::new(&input);
    let order = &task.exec_order;
    let expected_order = vec![4, 6, 3, 2, 5, 1];
    assert_eq!(*order, expected_order);
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
            exec_order: vec![1029, 1032],
        }
    );
}
