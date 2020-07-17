use crate::symbol::Symbol;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Apply(Box<Expr>, Box<Expr>),
    Sym(Symbol),
}

pub fn parse(symbols: &Vec<Symbol>) -> Expr {
    let mut new_symbols = Vec::<Symbol>::new();
    let mut app_count: i128 = 0;
    for e in symbols {
        match e {
            Symbol::App => {
                new_symbols.push(e.clone());
                app_count += 1;
            }
            Symbol::Pred => {
                new_symbols.push(Symbol::PredN(app_count));
                app_count = 0;
            }
            Symbol::Sum => {
                new_symbols.push(Symbol::SumN(app_count));
                app_count = 0;
            }
            _ => {
                new_symbols.push(e.clone());
                app_count = 0;
            }
        }
    }

    let mut cur: usize = 0;
    parse_internal(&new_symbols, &mut cur)
}

fn parse_internal(symbols: &[Symbol], cur: &mut usize) -> Expr {
    match &symbols[*cur] {
        Symbol::App => {
            *cur += 1;
            let expr1 = parse_internal(&symbols, cur);
            let expr2 = parse_internal(&symbols, cur);
            return Expr::Apply(Box::<Expr>::new(expr1), Box::<Expr>::new(expr2));
        }
        _ => {
            *cur += 1;
            return Expr::Sym(symbols[*cur - 1].clone());
        }
    }
}

#[cfg(test)]
mod tests {
    use super::Expr::*;
    use super::*;
    use crate::symbol::Symbol::*;
    #[test]
    fn parse_simple_sum() {
        let symbols = vec![App, App, Sum, Number(1), Number(2)];
        let expr = parse(&symbols);
        let expected_expr = Apply(
            Box::new(Apply(Box::new(Sym(SumN(2))), Box::new(Sym(Number(1))))),
            Box::new(Sym(Number(2))),
        );
        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn parse_complex_sum() {
        let symbols = vec![
            App,
            App,
            Sum,
            Number(1),
            App,
            App,
            Sum,
            Number(2),
            Number(3),
        ];
        let expr = parse(&symbols);
        let expected_expr = Apply(
            Box::new(Apply(Box::new(Sym(SumN(2))), Box::new(Sym(Number(1))))),
            Box::new(Apply(
                Box::new(Apply(Box::new(Sym(SumN(2))), Box::new(Sym(Number(2))))),
                Box::new(Sym(Number(3))),
            )),
        );
        assert_eq!(expr, expected_expr);
    }

    #[test]
    fn parse_three_sum() {
        let symbols = vec![App, App, App, Sum, Number(1), Number(2), Number(3)];
        let expr = parse(&symbols);
        let expected_expr = Apply(
            Box::new(Apply(
                Box::new(Apply(Box::new(Sym(SumN(3))), Box::new(Sym(Number(1))))),
                Box::new(Sym(Number(2))),
            )),
            Box::new(Sym(Number(3))),
        );
        assert_eq!(expr, expected_expr);
    }
}