use icfpc2020::modulate::modulate_number;

fn modulate(sexp: &sexp::Sexp) -> String {
    match sexp {
        sexp::Sexp::Atom(sexp::Atom::I(i)) => modulate_number(*i as i128),
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

pub fn modulate_sexp(sexp: &str) -> Result<String, Box<sexp::Error>> {
    println!("modulate_sexp: {}", sexp);
    let parsed = sexp::parse(&sexp)?;
    Ok(modulate(&parsed))
}
