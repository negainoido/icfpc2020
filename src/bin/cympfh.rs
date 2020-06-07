extern crate structopt;
use structopt::StructOpt;

extern crate icfpc2020;
use icfpc2020::ai::AI;

#[derive(Debug, StructOpt)]
struct Opt {
    #[structopt(short, long)]
    verbose: bool,
}

fn main() {
    let opt = Opt::from_args();
    let ai = AI();
    println!("{:?}", opt);
    println!("{:?}", ai);
}
