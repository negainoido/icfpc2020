use std::fs::File;
use std::io::{BufRead, BufReader, Stdin};

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

fn read_lines(reader: &mut dyn BufRead) -> Vec<String> {
    let mut line = String::new();
    let mut lines = vec![];
    while let Ok(size) = reader.read_line(&mut line) {
        if size == 0 {
            break;
        }
        if line.trim() == "" || line.trim().chars().next() == Some('#') {
            continue;
        }
        lines.push(line.trim().to_string());
        line.clear();
    }
    lines
}

fn run() -> std::io::Result<()> {
    let opt = Opt::from_args();
    common_init(&opt.common);

    let lines = if opt.input != "" {
        let mut reader = BufReader::new(File::open(&opt.input)?);
        read_lines(&mut reader)
    } else {
        let stdin: Stdin = std::io::stdin();
        let mut reader = BufReader::new(stdin);
        read_lines(&mut reader)
    };

    let task = Task::new(&lines[..]);
    let final_expr = task.solve();
    println!("{:?}", final_expr);
    Ok(())
}

fn main() {
    let _ = ::std::thread::Builder::new()
        .name("run".to_string())
        .stack_size(32 * 1024 * 1024)
        .spawn(run)
        .unwrap()
        .join();
}
