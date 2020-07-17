// use image::DynamicImage;

use structopt::StructOpt;

// use icfpc2020::decode;
use icfpc2020::opt::{common_init, CommonOpt};

#[derive(StructOpt)]
struct Opt {
    #[structopt(flatten)]
    common: CommonOpt,
    // #[structopt(short, long, parse(try_from_str= image::open))]
    // input: DynamicImage,
    //
    // #[structopt(short, long)]
    // output: String,
}

fn main() {
    let opt: Opt = Opt::from_args();
    common_init(&opt.common);
    // let expr = decode::png2expr(opt.input);

    // for e in expr {
    //     println!("{:?}", e)
    // }
}
