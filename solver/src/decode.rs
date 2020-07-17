#![allow(unused)]
use crate::symbol::Symbol;

fn lines2expr(arr: &Vec<Vec<bool>>, x: usize, y: usize) -> Option<(Vec<Symbol>, usize)> {
    unimplemented!("lines2vec");
}

fn is_zeroline(line: &Vec<bool>) -> bool {
    line.iter().all(|x| !x)
}

pub fn table2exprs(arr: &Vec<Vec<bool>>) -> Vec<Vec<Symbol>> {
    let h = arr.len();
    let mut l = 0;
    let mut exprs = Vec::<Vec<Symbol>>::new();
    while l < h {
        // Skip the zero line.
        if (is_zeroline(&arr[l])) {
            l += 1;
            continue;
        }
        // l is not zero line now.
        // find next zero line.
        let mut next_zl = l + 1;
        while next_zl < h {
            if (is_zeroline(&arr[next_zl])) {
                break;
            }
            next_zl += 1;
        }

        match lines2expr(&arr, l, h) {
            None => panic!("Failed parsing"),
            Some((expr, l_inc)) => {
                exprs.push(expr);
                l += l_inc
            }
        }
        // The next start is the next line of next zero line.
        l = next_zl + 1;
    }

    exprs
}

/// get returns rectangle of detected symbol having (x, y) as top left corner
/// assuming following coordinate system.
/// +----> y
/// |....
/// |..#.
/// |.#..
/// |....
/// v
/// x
///
pub fn get_rectangle(
    arr: &Vec<Vec<bool>>,
    x: usize,
    y: usize,
) -> Option<(usize, usize, usize, usize)> {
    if x + 1 >= arr.len() || y + 1 >= arr[0].len() {
        return None;
    }

    if x + 1 < arr.len() && y + 8 < arr[0].len() {
        // check
        // .........
        // .#.#.#.#.
        let first = &arr[x][y..y + 9];
        let second = &arr[x + 1][y..y + 9];
        if first.iter().all(|&f| !f)
            && second.iter().step_by(2).all(|&f| !f)
            && second.iter().skip(1).step_by(2).all(|&t| t)
        {
            return Some((x + 1, y + 1, 1, 7));
        }
    }

    // detect following case
    //  ?#
    //  #?
    // or
    //  #?
    //  ??
    //
    if !(arr[x][y] || (arr[x + 1][y] && arr[x][y + 1])) {
        return None;
    }

    // calc rectangle from top and left edge.
    // take care linear binary representation in
    // https://message-from-space.readthedocs.io/en/latest/message13.html
    //
    let mut cx = x + 1;
    while cx < arr.len() && (arr[cx][y] || arr[cx][y + 1]) {
        cx += 1;
    }
    let cx = cx;

    let mut cy = y + 1;
    while cy < arr[0].len() && (arr[x][cy] || arr[x + 1][cy]) {
        cy += 1;
    }
    Some((x, y, cx - x, cy - y))
}
