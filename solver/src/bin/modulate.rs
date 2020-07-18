#[derive(Debug)]
enum List {
    Cons(Box<List>, Box<List>),
    Integer(i128),
    Nil,
}

fn do_demodulate(a: &str) -> (&str, List) {
    let prefix = &a[..2];
    if prefix == "11" {
        let (a, car) = do_demodulate(&a[2..]);
        let (a, cdr) = do_demodulate(a);
        return (a, List::Cons(Box::new(car), Box::new(cdr)));
    } else if prefix == "00" {
        return (&a[2..], List::Nil);
    } else {
        let sign = if prefix == "01" { 1 } else { -1 };
        let a = &a[2..];
        let mut len = 0;
        for c in a.chars() {
            if c == '0' {
                break;
            }
            len += 1;
        }
        let a = &a[len + 1..];
        len *= 4;
        let res: i128 = i128::from_str_radix(&a[0..len], 2).unwrap();
        let a = &a[len..];
        return (a, List::Integer(sign * res));
    }
}
fn demodulate(a: &str) -> List {
    let (_, l) = do_demodulate(a);
    return l;
}
fn main() {
    //    let request = "1101000";
    let request = "110110000111011111100001001010100000110000";
    let res = demodulate(request);
    println!("result: {:?}", res);
}
