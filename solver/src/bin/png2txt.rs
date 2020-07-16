use image::{DynamicImage, GrayImage};

use structopt::StructOpt;

use icfpc2020::opt::{common_init, CommonOpt};

#[derive(StructOpt)]
struct Opt {
    #[structopt(flatten)]
    common: CommonOpt,

    #[structopt(short, long, parse(try_from_str= image::open))]
    input: DynamicImage,
    //
    // #[structopt(short, long)]
    // output: String,
}

// enum Symbol {
//     SymNumber(i64),
//     SymOperator(i32),
//     SymVariable(i32),
//     SymEllipsis,
//     SymUnknown,
// }

fn get(arr: &Vec<Vec<bool>>, x: usize, y: usize) -> Option<(usize, usize, usize, usize)> {
    if x + 1 >= arr.len() || y + 1 >= arr[0].len() {
        return None;
    }

    if x + 1 < arr.len() && y + 8 < arr[0].len() {
        // check
        // .........
        // .#.#.#.#.
        // .........
        if !&arr[x][y..y + 9].iter().any(|&x| x)
            && !&arr[x + 1][y..y + 9].iter().step_by(2).any(|&x| x)
            && *&arr[x + 1][y + 1..y + 9].iter().step_by(2).all(|&x| x)
        {
            return Some((x + 1, y + 1, 1, 7));
        }
    }

    // detect following case
    //  .#
    //  #.
    // or
    //  #.
    //  .?
    //
    if !(arr[x][y] || (arr[x + 1][y] && arr[x][y + 1])) {
        return None;
    }

    // calc rectangle from top and left edge.
    // take care linear binary representation in
    // https://message-from-space.readthedocs.io/en/latest/message13.html
    //
    let mut cx = x + 1;
    while cx < arr.len() {
        if arr[cx][y] || arr[cx][y + 1] {
            cx += 1;
            continue;
        }
        break;
    }
    let cx = cx;

    let mut cy = y + 1;
    while cy < arr[0].len() {
        if arr[x][cy] || arr[x + 1][cy] {
            cy += 1;
            continue;
        }
        break;
    }
    Some((x, y, cx - x, cy - y))
}

fn main() {
    let opt: Opt = Opt::from_args();
    common_init(&opt.common);

    let img: GrayImage = opt.input.to_luma();

    let width = img.dimensions().0 as usize / 4 - 2;
    let height = img.dimensions().1 as usize / 4 - 2;

    let mut arr = vec![vec![false; width]; height];

    println!("{:?}", img.dimensions());

    for x in 0..height {
        for y in 0..width {
            arr[x][y] = img.get_pixel((y as u32 + 1) * 4, (x as u32 + 1) * 4).0[0] > 0;
        }
    }

    let mut vis = vec![vec![false; width]; height];

    for x in 0..height {
        for y in 0..width {
            if vis[x][y] {
                continue;
            }
            match get(&arr, x, y) {
                None => continue,
                Some((x, y, h, w)) => {
                    for cx in x..x + h {
                        for cy in y..y + w {
                            vis[cx][cy] = true;
                            print!("{}", if arr[cx][cy] { 1 } else { 0 });
                        }
                        println!();
                    }
                    println!();
                }
            }
        }
    }

    println!("{:?}", img.dimensions());
    println!("{}", opt.output);
}
