use image::{DynamicImage, GrayImage};

use structopt::StructOpt;

use icfpc2020::decode;
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
            match decode::get_rectangle(&arr, x, y) {
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
}
