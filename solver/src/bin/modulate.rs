/*
enum List {
    Cons(Box<List>, Box<List>),
    Integer(i128),
    Nil,
}
*/

fn demodulate(a: &str) -> &str {
    let prefix = &a[..2];
    if prefix == "11" {
        println!("cons");
        let a = demodulate(&a[2..]);
        let a = demodulate(a);
        a
    } else if prefix == "00" {
        println!("nil");
        &a[2..]
    } else {
        let sign;
        if prefix == "01" {
            sign = 1;
        } else {
            sign = -1;
        }
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
        let mut res: i128 = 0;
        for i in 0..len {
            res *= 2;
            if a.chars().nth(i) == Some('1') {
                res += 1;
            }
        }
        println!("len {}, int {}", len, sign * res);
        a
    }
}
fn main() {
    let request = "1101000";
    demodulate(request);
}
