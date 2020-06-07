extern crate structopt;
use structopt::StructOpt;

#[derive(StructOpt)]
struct Opt {
    #[structopt(short, long)]
    verbose: bool,
}

fn main() {
    let opt = Opt::from_args();
    println!("Hello, world! {}", rand::random::<u32>());
    println!("verbose: {:?}", opt.verbose);
}
