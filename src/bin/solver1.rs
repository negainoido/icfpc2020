extern crate icfpc2020;
use icfpc2020::ai::AI;

fn main() {
    let ai = AI();
    println!("This is solver1.");
    println!("{:?} {}", ai, rand::random::<u32>())
}
