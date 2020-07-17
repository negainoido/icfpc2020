#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Symbol {
    Number(i128),
    App,
    Eq,
    Succ,
    Pred,
    Sum,
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
    Lpar,
    Rpar,
    Sep,
}

use Symbol::*;
impl Symbol {
    const OPS: [Symbol; 16] = [
        App, Eq, Succ, Pred, Sum, Prod, Div, True, False, BigEq, Less, Mod, DeMod, Lpar, Rpar, Sep,
    ];

    fn str2vec(s: &str) -> Vec<Vec<bool>> {
        let mut v = vec![];

        for i in s.trim().split("\n") {
            let i = i.trim();
            if i.is_empty() {
                continue;
            }
            v.push(i.trim().chars().map(|x| x == '#').collect());
        }
        v
    }

    pub fn shape(&self) -> Vec<Vec<bool>> {
        match self {
            App => Symbol::str2vec(
                r#"
##
#.
"#,
            ),
            Lpar => Symbol::str2vec(
                r#"
..#
.##
###
.##
..#
"#,
            ),
            Rpar => Symbol::str2vec(
                r#"
#..
##.
###
##.
#..
"#,
            ),
            Sep => Symbol::str2vec(
                r#"
##
##
##
##
##
"#,
            ),
            _ => unimplemented!(),
        }
    }

    fn is_number(square: (usize, usize, usize, usize), image: &Vec<Vec<bool>>) -> bool {
        let (x, y, height, width) = square;
        if width <= 1 || height <= 1 {
            return false;
        }
        if image[x][y] {
            return false;
        }
        for i in 1..height {
            if !image[x + i][y] {
                return false;
            }
        }
        for j in 1..width {
            if !image[x][y + j] {
                return false;
            }
        }
        if height == width + 1 {
            for j in 1..width {
                if image[height - 1][j] {
                    return false;
                }
            }
        }
        true
    }

    fn as_number(
        square: (usize, usize, usize, usize),
        image: &Vec<Vec<bool>>,
    ) -> Option<(i128, bool)> {
        let (x, y, height, width) = square;
        if height == width || height == width + 1 {
            let sign = if height == width { 1 } else { -1 };
            let is_num = Symbol::is_number(square, &image);
            let mut k = 0;
            let mut num = 0;
            for i in 1..width {
                for j in 1..width {
                    if image[x + i][y + j] {
                        num |= 1 << k;
                    }
                    k += 1;
                }
            }
            Some((sign * num, is_num))
        } else {
            None
        }
    }

    fn shape_eq(square: (usize, usize, usize, usize), image: &Vec<Vec<bool>>, op: Symbol) -> bool {
        let (x, y, height, width) = square;
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
        if let Some((num, is_number)) = Symbol::as_number((x, y, height, width), &image) {
            if is_number {
                return Some(Number(num));
            }
        }
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
    fn test_number() {
        let image = vec![
            vec![false, false, true, false],
            vec![false, true, false, true],
            vec![false, false, true, true],
            vec![false, true, true, false],
        ];
        assert_eq!(Symbol::as_number((0, 0, 2, 2), &image), Some((1, false)));
        assert_eq!(Symbol::as_number((0, 1, 2, 2), &image), Some((0, true)));
        assert_eq!(Symbol::as_number((2, 1, 2, 2), &image), Some((1, true)));
        assert_eq!(Symbol::as_number((0, 0, 3, 3), &image), Some((9, false)));
        assert_eq!(Symbol::as_number((2, 2, 2, 2), &image), Some((0, false)));
        assert_eq!(Symbol::as_number((1, 2, 3, 2), &image), Some((-1, true)));

        {
            let image = vec![
                vec![false, true, true],
                vec![true, true, false],
                vec![true, true, true],
                vec![true, false, false],
            ];
            assert_eq!(Symbol::as_number((0, 0, 4, 3), &image), Some((-13, true)))
        }
        {
            let image = vec![
                vec![false, true, true],
                vec![true, true, false],
                vec![true, true, true],
                vec![true, true, false],
            ];
            assert_eq!(Symbol::as_number((0, 0, 4, 3), &image), Some((-13, false)))
        }
    }

    #[test]
    #[ignore]
    fn test_decode_symbols() {
        let image = vec![
            vec![false, false, false],
            vec![false, true, true],
            vec![false, true, false],
            vec![false, false, false],
        ];
        assert_eq!(Symbol::from(1, 1, 2, 2, &image), Some(Symbol::App));
        assert_eq!(Symbol::from(0, 0, 1, 2, &image), None);
    }

    #[test]
    fn test_str2vec() {
        assert_eq!(
            Symbol::str2vec(
                r#"
#.
##
"#
            ),
            vec![vec![true, false], vec![true, true]]
        );

        assert_eq!(
            Symbol::str2vec(
                r#"
#.

##
"#
            ),
            vec![vec![true, false], vec![true, true]]
        );
    }
}
