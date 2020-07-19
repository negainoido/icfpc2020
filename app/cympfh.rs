#![allow(dead_code, unused_variables)]
use crate::ai::*;
use crate::moon::Moon;
use crate::protocol::*;

pub struct CympfhAI {
    rand: XorShift,
}

// .----> x0
// |
// |
// v x1

enum Section {
    Up,
    Right,
    Left,
    Down,
    DiagRU,
    DiagRD,
    DiagLU,
    DiagLD,
    O,
}

impl Section {
    fn from(x: &Coord) -> Section {
        use Section::*;
        let (x0, x1) = *x;
        if x0 + x1 == 0 && x0 > 0 {
            DiagRU
        } else if x0 == x1 && x0 > 0 {
            DiagRD
        } else if x0 + x1 == 0 && x0 < 0 {
            DiagLD
        } else if x0 == x1 && x0 < 0 {
            DiagLU
        } else if x0 > x1.abs() {
            Right
        } else if x1 > x0.abs() {
            Down
        } else if -x0 > x1.abs() {
            Left
        } else if -x1 > x0.abs() {
            Up
        } else {
            O
        }
    }
    fn gravity(&self) -> Coord {
        match self {
            Section::Up => (0, 1),
            Section::Right => (-1, 0),
            Section::Left => (1, 0),
            Section::Down => (0, -1),
            Section::DiagRU => (-1, 1),
            Section::DiagRD => (-1, -1),
            Section::DiagLU => (1, 1),
            Section::DiagLD => (1, -1),
            Section::O => (0, 0),
        }
    }
}

pub trait FromU64 {
    fn coerce(x: u64) -> Self;
}
impl FromU64 for u64 {
    fn coerce(x: u64) -> Self {
        x
    }
}
impl FromU64 for i64 {
    fn coerce(x: u64) -> Self {
        x as i64
    }
}
impl FromU64 for u32 {
    fn coerce(x: u64) -> Self {
        x as u32
    }
}
impl FromU64 for i32 {
    fn coerce(x: u64) -> Self {
        x as i32
    }
}
impl FromU64 for usize {
    fn coerce(x: u64) -> Self {
        x as usize
    }
}
impl FromU64 for bool {
    fn coerce(x: u64) -> Self {
        x % 2 == 0
    }
}

// returns [0, 1]
impl FromU64 for f64 {
    fn coerce(x: u64) -> Self {
        (x as f64) / (std::u64::MAX as f64)
    }
}
impl FromU64 for f32 {
    fn coerce(x: u64) -> Self {
        (x as f32) / (std::u64::MAX as f32)
    }
}

struct XorShift(u64);
impl XorShift {
    fn new() -> Self {
        XorShift(88172645463325252)
    }
    fn next(&mut self) -> u64 {
        let mut x = self.0;
        x = x ^ (x << 13);
        x = x ^ (x >> 7);
        x = x ^ (x << 17);
        self.0 = x;
        x
    }
    fn gen<T: FromU64>(&mut self) -> T {
        FromU64::coerce(self.next())
    }
}

fn add(a: &Coord, b: &Coord) -> Coord {
    (a.0 + b.0, a.1 + b.1)
}

fn close(a: &Ship, b: &Ship) -> bool {
    let x = add(
        &add(&a.position, &a.velocity),
        &Section::from(&a.position).gravity(),
    );
    let y = add(
        &add(&b.position, &b.velocity),
        &Section::from(&b.position).gravity(),
    );
    let d = (x.0 - y.0).abs() + (x.1 - y.1).abs();
    d <= 2
}

impl AI for CympfhAI {
    fn new() -> Self {
        Self {
            rand: XorShift::new(),
        }
    }
    fn main(&mut self, _info: &GameInfo, _state: &GameState) -> Vec<Command> {
        let role_self = _info.role;
        let ship_self: &Ship = _state
            .ship_and_commands
            .iter()
            .filter(|&(ship, _)| ship.role == role_self)
            .map(|(ship, _)| ship)
            .next()
            .unwrap();
        let ship_enemy: &Ship = _state
            .ship_and_commands
            .iter()
            .filter(|&(ship, _)| ship.role != role_self)
            .map(|(ship, _)| ship)
            .next()
            .unwrap();

        let boost = Moon::get_boost(&ship_self.position, &ship_self.velocity);
        if ship_self.role == Role::Attacker && close(&ship_self, &ship_enemy) {
            return vec![Command::Detonate {
                ship_id: ship_self.id,
            }];
        } else if boost != (0, 0) {
            return vec![Command::Accelerate {
                ship_id: ship_self.id,
                vector: boost,
            }];
        } else if ship_self.role == Role::Attacker {
            let y = add(
                &add(&ship_enemy.position, &ship_enemy.velocity),
                &Section::from(&ship_enemy.position).gravity(),
            );
            return vec![Command::Shoot {
                ship_id: ship_self.id,
                target: y,
            }];
        } else {
            return vec![];
        }
    }
}
