use log::LevelFilter;
use structopt::StructOpt;

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
}
