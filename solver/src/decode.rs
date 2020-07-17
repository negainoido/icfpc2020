use crate::symbol::Symbol;
// use image::{DynamicImage, GrayImage};

// pub fn png2expr(dimg: DynamicImage) -> Vec<Vec<Symbol>> {
//     let img: GrayImage = dimg.to_luma();
//     let width = img.dimensions().0 as usize / 4 - 2;
//     let height = img.dimensions().1 as usize / 4 - 2;

//     let mut arr = vec![vec![false; width]; height];

//     // println!("{:?}", img.dimensions());
//     for x in 0..height {
//         for y in 0..width {
//             arr[x][y] = img.get_pixel((y as u32 + 1) * 4, (x as u32 + 1) * 4).0[0] > 0;
//         }
//     }

//     table2exprs(&arr)
// }

fn is_zero_column(arr: &Vec<Vec<bool>>, x: usize, h: usize, y: usize) -> bool {
    for i in x..x + h {
        if arr[i][y] {
            return false;
        }
    }
    true
}

fn lines2expr(arr: &Vec<Vec<bool>>, x: usize, h: usize) -> Option<Vec<Symbol>> {
    let w = arr[x].len();
    let mut squares = Vec::<(usize, usize, usize, usize)>::new();

    let mut j = 0;
    while j < w {
        if is_zero_column(&arr, x, h, j) {
            j += 1;
            continue;
        }
        // The origin of the current square is (x, j);
        let mut next_j = j + 1;
        while next_j < w {
            if is_zero_column(&arr, x, h, next_j) {
                break;
            }
            next_j += 1;
        }
        let square_w = next_j - j;
        let mut square_h = h;
        for i in (x + h - 1..=x).rev() {
            let mut last_square_line = false;
            for j2 in j..j + square_w {
                if arr[i][j2] {
                    last_square_line = true;
                    break;
                }
            }
            if last_square_line {
                square_h = i - x + 1;
            }
        }
        squares.push((x, j, square_h, square_w));
        j = next_j + 1;
    }

    let mut expr = Vec::<Symbol>::new();
    for (x, y, h, w) in squares {
        match Symbol::from(x, y, h, w, &arr) {
            None => {
                panic!("Failed parsing symbol");
            }
            Some(symbol) => {
                expr.push(symbol);
            }
        }
    }

    Some(expr)
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
        if is_zeroline(&arr[l]) {
            l += 1;
            continue;
        }
        // l is not zero line now.
        // find next zero line.
        let mut next_zl = l + 1;
        while next_zl < h {
            if is_zeroline(&arr[next_zl]) {
                break;
            }
            next_zl += 1;
        }

        match lines2expr(&arr, l, next_zl - l) {
            None => panic!("Failed parsing expression"),
            Some(expr) => {
                exprs.push(expr);
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
