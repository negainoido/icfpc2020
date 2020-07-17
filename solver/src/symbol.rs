use Symbol::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Symbol {
    Number(i128),
    App,
    Eq,
    Succ,
    Pred,
    PredN(u32), // Used only in the eval step.
    Sum,
    SumN(u32), // Used only in the eval step.
    Variable(i128),
    Prod,
    Div,
    True,
    False,
    BigEq,
    Less,
    Mod,
    DeMod,
    Neg,
    BComb,
    CComb,
    SComb,
    TComb,
    IComb,
    Car,
    Cdr,
    Cons,
    Nil,
    IsNil,
    Ellipsis,
    Lpar,
    Rpar,
    Sep,
    Target,
}

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
                "
##
#.
",
            ),
            Lpar => Symbol::str2vec(
                "
..#
.##
###
.##
..#
",
            ),
            Rpar => Symbol::str2vec(
                "
#..
##.
###
##.
#..
",
            ),
            Sep => Symbol::str2vec(
                "
##
##
##
##
##
",
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
                if image[x + height - 1][y + j] {
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

    fn as_variable(square: (usize, usize, usize, usize), image: &Vec<Vec<bool>>) -> Option<i128> {
        let (x, y, height, width) = square;
        if height != width {
            return None;
        }
        if height < 4 || width < 4 {
            return None;
        }
        if !image[x + 1][y + 1] {
            return None;
        }
        // edge check
        for i in 0..height {
            if !image[x + i][y] {
                return None;
            }
            if !image[x + i][y + width - 1] {
                return None;
            }
        }
        for j in 0..width {
            if !image[x][y + j] {
                return None;
            }
            if !image[x + height - 1][y + j] {
                return None;
            }
        }
        if let Some((num, _)) = Symbol::as_number((x + 1, y + 1, height - 2, width - 2), &image) {
            let full = (1 << (width - 3).pow(2)) - 1;
            Some(full ^ num)
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
        if let Some(id) = Symbol::as_variable((x, y, height, width), &image) {
            return Some(Variable(id));
        }
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

    pub fn from_text(token: String, target: &String) -> Symbol {
        let token = token.trim();
        let option_symbol = match token {
            "add" => Some(Symbol::Sum),
            "ap" => Some(Symbol::App),
            "b" => Some(Symbol::BComb),
            "c" => Some(Symbol::CComb),
            "car" => Some(Symbol::Car),
            "cdr" => Some(Symbol::Cdr),
            "cons" => Some(Symbol::Cons),
            "div" => Some(Symbol::Div),
            "eq" | "=" => Some(Symbol::Eq),
            "i" => Some(Symbol::IComb),
            "isnil" => Some(Symbol::IsNil),
            "lt" => Some(Symbol::Less),
            "mul" => Some(Symbol::Prod),
            "neg" => Some(Symbol::Neg),
            "nil" => Some(Symbol::Nil),
            "s" => Some(Symbol::SComb),
            "t" => Some(Symbol::TComb),
            _ => None,
        };

        if let Some(symbol) = option_symbol {
            symbol
        } else if token.eq(target) {
            Symbol::Target
        } else {
            match token.chars().next().unwrap() {
                ':' => Symbol::Variable(token[1..].parse::<i128>().unwrap()),
                _ => {
                    dbg!(&token);
                    Symbol::Number(token.parse::<i128>().unwrap())
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_variable() {
        {
            let image = Symbol::str2vec(
                "
                ####
                ##.#
                #.##
                ####
            ",
            );
            assert_eq!(Symbol::as_variable((0, 0, 4, 4), &image), Some(0));
        }
        {
            let image = Symbol::str2vec(
                "
                ####
                ##.#
                #..#
                ####
            ",
            );
            assert_eq!(Symbol::as_variable((0, 0, 4, 4), &image), Some(1));
        }
        {
            let image = Symbol::str2vec(
                "
                #####
                ##..#
                #.#.#
                #.###
                #####
            ",
            );
            assert_eq!(Symbol::as_variable((0, 0, 5, 5), &image), Some(2));
        }
        {
            let image = Symbol::str2vec(
                "
                #####
                ##..#
                #.#.#
                #.###
                ####.
            ",
            );
            assert_eq!(Symbol::as_variable((0, 0, 5, 5), &image), None);
        }
    }

    #[test]
    fn test_number() {
        let image = Symbol::str2vec(
            "
            ..#.
            .#.#
            ..##
            .##.
            ",
        );
        assert_eq!(Symbol::as_number((0, 0, 2, 2), &image), Some((1, false)));
        assert_eq!(Symbol::as_number((0, 1, 2, 2), &image), Some((0, true)));
        assert_eq!(Symbol::as_number((2, 1, 2, 2), &image), Some((1, true)));
        assert_eq!(Symbol::as_number((0, 0, 3, 3), &image), Some((9, false)));
        assert_eq!(Symbol::as_number((2, 2, 2, 2), &image), Some((0, false)));
        assert_eq!(Symbol::as_number((1, 2, 3, 2), &image), Some((-1, true)));

        {
            let image = Symbol::str2vec(
                "
                .#
                #.
                ",
            );
            assert_eq!(Symbol::as_number((0, 0, 2, 2), &image), Some((0, true)))
        }
        {
            let image = Symbol::str2vec(
                "
                .#
                ##
                #.
                ",
            );
            assert_eq!(Symbol::as_number((0, 0, 3, 2), &image), Some((-1, true)))
        }
        {
            let image = Symbol::str2vec(
                "
                .##
                ##.
                ###
                #..
            ",
            );
            assert_eq!(Symbol::as_number((0, 0, 4, 3), &image), Some((-13, true)))
        }
        {
            let image = Symbol::str2vec(
                "
                .##
                ##.
                ###
                ##.
                ",
            );
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
                "
#.
##
"
            ),
            vec![vec![true, false], vec![true, true]]
        );

        assert_eq!(
            Symbol::str2vec(
                "
#.

##
"
            ),
            vec![vec![true, false], vec![true, true]]
        );
    }
}
