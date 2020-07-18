use icfpc2020::opt::{common_init, CommonOpt};
use structopt::StructOpt;

extern crate icfpc2020;
use icfpc2020::eval::Evaluator;
use icfpc2020::modulate::*;

#[derive(Debug, StructOpt)]
struct Opt {
    #[structopt(flatten)]
    common: CommonOpt,

    #[structopt(
        short,
        long,
        default_value = "110110000111011111100001001010100000110000"
    )]
    input: String,
}

fn main() {
    let opt = Opt::from_args();
    common_init(&opt.common);

    let eval = Evaluator::new();
    let request = &opt.input; //    let request = "1101000";
    let res = demodulate(request, &eval);
    println!("result: {:?}", res);
}
