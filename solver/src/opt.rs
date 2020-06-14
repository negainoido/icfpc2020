extern crate env_logger;

extern crate structopt;
use structopt::StructOpt;

extern crate log;
use log::LevelFilter;

#[derive(Debug, StructOpt)]
pub struct CommonOpt {
    #[structopt(short, long)]
    verbose: bool,
}

pub fn common_init(opt: &CommonOpt) {
    if opt.verbose {
        env_logger::builder().filter_level(LevelFilter::Info).init();
    } else {
        env_logger::init();
    }

    match std::env::var("SOLVER_REVISION") {
        Ok(val) => eprintln!("solver revison is {}", val),
        Err(e) => eprintln!("solver revision is unknown: {}", e),
    }
}
