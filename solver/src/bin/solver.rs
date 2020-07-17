use std::fs::File;
use std::io::{BufRead, BufReader};

use structopt::StructOpt;

use icfpc2020::opt::{common_init, CommonOpt};
use icfpc2020::task::Task;

#[derive(Debug, StructOpt)]
struct Opt {
    #[structopt(flatten)]
    common: CommonOpt,
    #[structopt(short, long, default_value = "")]
    input: String,
}

fn main() -> std::io::Result<()> {
    let opt = Opt::from_args();
    common_init(&opt.common);

    let mut reader = BufReader::new(File::open(&opt.input)?);
    let mut line = String::new();
    let mut lines = vec![];
    while let Ok(size) = reader.read_line(&mut line) {
        if size == 0 {
            break;
        }
        lines.push(line.trim().to_string());
        line.clear();
    }

    let task = Task::new(&lines);
    let final_expr = task.solve();
    println!("{:?}", final_expr);
    Ok(())
}
