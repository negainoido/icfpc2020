use std::fs::File;
use std::io::prelude::*;

use icfpc2020::ai::AI;
use icfpc2020::opt::{common_init, CommonOpt};
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

    let mut contents = String::new();
    File::open(&opt.input)?.read_to_string(&mut contents)?;

    File::create(&opt.output)?.write_all(&contents.as_bytes())?;

    Ok(())
}
