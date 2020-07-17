#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Symbol {
    Number(i128),
    App,
    Eq,
    Succ,
    Pred,
    PredN(i128), // Used only in the eval step.
    Sum,
    SumN(i128), // Used only in the eval step.
    Variable(i128),
    Prod,
    Div,
    True,
    False,
    BigEq,
    Less,
    Mod,
    DeMod,
    // Neg,
    Ellipsis,
}

use Symbol::*;
impl Symbol {
    const OPS: [Symbol; 13] = [
        App, Eq, Succ, Pred, Sum, Prod, Div, True, False, BigEq, Less, Mod, DeMod,
    ];

    pub fn shape(&self) -> Vec<Vec<bool>> {
        let ss = match self {
            App => vec!["##", "#."],
            _ => unimplemented!(),
        };
        ss.iter()
            .map(|&line| line.chars().map(|c| c == '#').collect())
            .collect()
    }

    fn shape_eq(size: (usize, usize, usize, usize), image: &Vec<Vec<bool>>, op: Symbol) -> bool {
        let (x, y, height, width) = size;
        let shape = op.shape();
        if x != shape.len() || y != shape[0].len() {
            false
        } else {
            for i in 0..height {
                for j in 0..width {
                    if image[i + x][j + y] != shape[i][j] {
                        return false;
                    }
                }
            }
            true
        }
    }

    pub fn from(
        x: usize,
        y: usize,
        height: usize,
        width: usize,
        image: &Vec<Vec<bool>>,
    ) -> Option<Self> {
        for &op in Self::OPS.iter() {
            if Symbol::shape_eq((x, y, height, width), &image, op) {
                return Some(op);
            }
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[ignore]
    fn it_works() {
        let image = vec![
            vec![false, false, false],
            vec![false, true, true],
            vec![false, true, false],
            vec![false, false, false],
        ];
        assert_eq!(Symbol::from(1, 1, 2, 2, &image), Some(Symbol::App));
        assert_eq!(Symbol::from(0, 0, 1, 2, &image), None);
    }
}
