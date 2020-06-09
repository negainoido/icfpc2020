extern crate structopt;
use structopt::StructOpt;

extern crate icfpc2020;
use icfpc2020::ai::AI;
use icfpc2020::opt::{common_init, CommonOpt};

#[macro_use]
extern crate log;

#[derive(Debug, StructOpt)]
struct Opt {
    #[structopt(flatten)]
    common: CommonOpt,
    #[structopt(short, long, default_value = "3")]
    num: usize,

    #[structopt(short, long, default_value = "")]
    input: String,

    #[structopt(short, long, default_value = "")]
    output: String,
}

fn main() {
    let opt = Opt::from_args();
    common_init(&opt.common);

    let ai = AI();
    println!("{:?}", opt);
    println!("{:?}", ai);

    println!("This is solver1.");
    println!("{:?} {}", ai, rand::random::<u32>());
    info!("This is info log");
    warn!("This is warning log");
    error!("This is error log");
}
