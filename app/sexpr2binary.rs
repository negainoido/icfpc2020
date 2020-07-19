fn modulate_number(n: i64) -> String {
    if n == 0 {
        return "010".to_string();
    }
    let mut ret: String = String::from(if n < 0 { "10" } else { "01" });
    let n = n.abs();
    let mut binary: String = format!("{:b}", n);
    while binary.len() % 4 != 0 {
        binary = "0".to_string() + &binary;
    }
    for _ in 0..binary.len() / 4 {
        ret += "1";
    }
    ret += "0";
    ret + &binary
}

fn modulate(sexp: &sexp::Sexp) -> String {
    match sexp {
        sexp::Sexp::Atom(sexp::Atom::I(i)) => modulate_number(*i),
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
