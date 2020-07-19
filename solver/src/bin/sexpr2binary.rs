extern crate structopt;
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
struct Opt {
    #[structopt(short, long)]
    input: String,
}

fn modulate(sexp: &sexp::Sexp) -> String {
    match sexp {
        sexp::Sexp::Atom(sexp::Atom::I(i)) => icfpc2020::modulate::modulate_number(*i as i128),
        sexp::Sexp::Atom(sexp::Atom::S(s)) if s == "nil" => "00".to_string(),
        sexp::Sexp::List(xs) if xs.len() == 0 => "00".to_string(),
        sexp::Sexp::List(xs) if xs[0] == sexp::Sexp::Atom(sexp::Atom::S("cons".to_string())) => {
            assert!(xs.len() == 3);
            "11".to_string() + &modulate(&xs[1]) + &modulate(&xs[2])
        }
        sexp::Sexp::List(xs) => {
            "11".to_string() + &modulate(&xs[0]) + &modulate(&sexp::Sexp::List(xs[1..].to_vec()))
        }
        _ => panic!("unexpected sexp {}", sexp),
    }
}

fn main() -> Result<(), Box<sexp::Error>> {
    let opt: Opt = Opt::from_args();
    let parsed = sexp::parse(&opt.input)?;
    dbg!(&parsed);
    println!("{}", modulate(&parsed));
    Ok(())
}
