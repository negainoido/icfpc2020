use std::convert::TryInto;

fn encode_posnum(num: u32) -> Vec<Vec<bool>> {
    let mut size = 2;
    while size <= 6 {
        if num < (1 << ((size - 1) * (size - 1))) {
            break;
        }
        size += 1;
    }

    let mut ret = vec![vec![false; size]; size];
    for i in 1..size {
        ret[0][i] = true;
        ret[i][0] = true;
    }

    let mut k = 0;
    for i in 1..size {
        for j in 1..size {
            if num >> k & 1 == 1 {
                ret[i][j] = true;
            }
            k = k + 1;
        }
    }
    ret
}

pub fn encode_num(num: i32) -> Vec<Vec<bool>> {
    if num >= 0 {
        encode_posnum(num.try_into().expect("failed casting"))
    } else {
        let mut vec = encode_posnum(-num as u32);
        let mut extra = vec![false; vec[0].len()];
        extra[0] = true;
        vec.push(extra);
        vec
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::symbol::Symbol;

    #[test]
    fn test_encode_num() {
        assert_eq!(
            encode_num(5),
            vec![
                vec![false, true, true],
                vec![true, true, false],
                vec![true, true, false],
            ]
        );

        assert_eq!(
            encode_num(-5),
            vec![
                vec![false, true, true],
                vec![true, true, false],
                vec![true, true, false],
                vec![true, false, false],
            ]
        );

        assert_eq!(encode_num(0), vec![vec![false, true,], vec![true, false,]]);
    }

    fn encode_decode(n: i32) -> Option<Symbol> {
        let v = encode_num(n as i32);
        let h = v.len();
        let w = v[0].len();
        Symbol::from(0, 0, h, w, &v)
    }

    #[test]
    fn test_encode_decode() {
        for i in (-16)..16 {
            assert_eq!(encode_decode(i), Some(Symbol::Number(i.into())));
        }
    }
}
