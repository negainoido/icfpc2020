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

    match std::env::var("SOLVER_REVISION") {
        Ok(val) => eprintln!(
            "solver revison is https://github.com/negainoido/icfpc2020/commit/{}",
            val
        ),
        Err(e) => eprintln!("solver revision is unknown: {}", e),
    }
}
