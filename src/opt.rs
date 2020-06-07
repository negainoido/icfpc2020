extern crate structopt;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
pub struct CommonOpt {
    #[structopt(short, long)]
    verbose: bool,
}
