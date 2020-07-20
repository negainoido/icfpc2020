use crate::protocol::*;

pub fn gravity_of((x, y): &Coord) -> Coord {
    let mut gx = 0;
    let mut gy = 0;

    if *x >= y.abs() {
        gx -= 1;
    }
    if *y >= x.abs() {
        gy -= 1;
    }
    if *x <= -y.abs() {
        gx += 1;
    }
    if *y <= -x.abs() {
        gy += 1;
    }
    return (gx, gy);
}
