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
}

fn main() {
    common_init();

    let opt = Opt::from_args();
    let ai = AI();
    println!("{:?}", opt);
    println!("{:?}", ai);

    println!("This is solver1.");
    println!("{:?} {}", ai, rand::random::<u32>());
    info!("This is info log");
    warn!("This is warning log");
    error!("This is error log");
}
