extern crate structopt;
use structopt::StructOpt;

extern crate icfpc2020;
use icfpc2020::ai::*;
use icfpc2020::opt::*;

#[derive(Debug, StructOpt)]
struct Opt {
    #[structopt(flatten)]
    common: CommonOpt,
    #[structopt(short, long, default_value = "3")]
    num: usize,
}

fn main() {
    let opt = Opt::from_args();
    common_init(&opt.common);

    let ai = AI();
    println!("{:?}", opt);
    println!("{:?}", ai);
}
