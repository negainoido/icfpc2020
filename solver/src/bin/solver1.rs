use std::fs::File;
use std::io::prelude::*;
use std::io::BufReader;

use icfpc2020::ai::AI;
use icfpc2020::opt::{common_init, CommonOpt};
use proconio::input;
use proconio::source::auto::AutoSource;
use structopt::StructOpt;

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

fn main() -> std::io::Result<()> {
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

    let source = AutoSource::new(BufReader::new(File::open(&opt.input)?));

    input! {
        from source,
        contents: String,
    }

    File::create(&opt.output)?.write_all(&contents.as_bytes())?;

    Ok(())
}
