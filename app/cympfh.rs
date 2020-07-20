#![allow(dead_code, unused_variables)]
use crate::ai::*;
use crate::new_moon::NewMoon;
use crate::protocol::*;

pub struct CympfhAI {
    rand: XorShift,
    atack_waiting: bool,
}

impl CympfhAI {
    fn estimate_next_position(ship: &Ship) -> Ship {
        let pos = if ship.velocity == (0, 0) {
            // 停止してる
            ship.position
        } else {
            add(
                &add(&ship.position, &ship.velocity),
                &Section::from(&ship.position).gravity(),
            )
        };
        Ship {
            role: ship.role,
            id: ship.id,
            position: pos,
            velocity: ship.velocity,
            x4: ship.x4.clone(),
            x5: ship.x5,
            x6: ship.x6,
            x7: ship.x7,
        }
    }
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
impl FromU64 for i128 {
    fn coerce(x: u64) -> Self {
        x as i128
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

const DETONATE_DIST: i128 = 3;

fn dist_max(x: &Coord, y: &Coord) -> i128 {
    std::cmp::max((x.0 - y.0).abs(), (x.1 - y.1).abs())
}

fn dist_manhattan(x: &Coord, y: &Coord) -> i128 {
    (x.0 - y.0).abs() + (x.1 - y.1).abs()
}

fn close_manhattan(a: &Ship, b: &Ship, dist_sup: i128) -> bool {
    let x = add(
        &add(&a.position, &a.velocity),
        &Section::from(&a.position).gravity(),
    );
    let y = add(
        &add(&b.position, &b.velocity),
        &Section::from(&b.position).gravity(),
    );
    dist_manhattan(&x, &y) < dist_sup
}

fn close_max(a: &Ship, b: &Ship, dist_sup: i128) -> bool {
    let x = add(
        &add(&a.position, &a.velocity),
        &Section::from(&a.position).gravity(),
    );
    let y = add(
        &add(&b.position, &b.velocity),
        &Section::from(&b.position).gravity(),
    );
    dist_max(&x, &y) < dist_sup
}

impl AI for CympfhAI {
    fn new() -> Self {
        Self {
            rand: XorShift::new(),
            atack_waiting: true,
        }
    }
    fn main(&mut self, _info: &GameInfo, _state: &GameState) -> Vec<Command> {
        use Role::*;

        let role_self = _info.role;
        let self_ships: Vec<&Ship> = _state
            .ship_and_commands
            .iter()
            .filter(|&(ship, _)| ship.role == role_self)
            .map(|(ship, _)| ship)
            .collect();
        let enemy_ships: Vec<&Ship> = _state
            .ship_and_commands
            .iter()
            .filter(|&(ship, _)| ship.role != role_self)
            .map(|(ship, _)| ship)
            .collect();

        let role = self_ships[0].role;

        let commands_enemy = _state
            .ship_and_commands
            .iter()
            .filter(|&(ship, _)| ship.role != role_self)
            .map(|(_, commands)| commands)
            .next()
            .unwrap();

        let mut cmds = vec![];

        for &ship in self_ships.iter() {
            // 自爆
            if role == Attacker
                && enemy_ships.len() == 1
                && close_max(&ship, &enemy_ships[0], DETONATE_DIST)
            {
                cmds.push(Command::Detonate { ship_id: ship.id });
                continue;
            }
            // 衛星軌道
            if let Some(boost) = NewMoon::get_boost(&ship, &_state) {
                cmds.push(boost);
                continue;
            }
            // ビーム
            if ship.x5 <= 10 {
                let mut done = false;
                for &enemy_ship in enemy_ships.iter() {
                    let target = CympfhAI::estimate_next_position(&enemy_ship);
                    let power = ship.x6 - ship.x5;
                    cmds.push(Command::Shoot {
                        ship_id: ship.id,
                        target: target.position,
                        power,
                    });
                    done = true;
                    break;
                }
                if done {
                    continue;
                }
            }
            // 自爆回避
            if role == Defender {
                let mut done = false;
                let self_ship = CympfhAI::estimate_next_position(&ship);
                for &enemy_ship in enemy_ships.iter() {
                    let target = CympfhAI::estimate_next_position(&enemy_ship);
                    if close_max(&target, &self_ship, DETONATE_DIST * 2) {
                        let g = Section::from(&ship.position).gravity();
                        let boost = (-g.0, -g.1);
                        cmds.push(Command::Accelerate {
                            ship_id: ship.id,
                            vector: boost,
                        });
                        done = true;
                        break;
                    }
                }
                if done {
                    continue;
                }
            }
            // Random Accel
            {
                if ship.position.0.abs() + ship.position.1.abs() > 32 {
                    if self.rand.gen::<i128>() % 20 == 0 {
                        let g = Section::from(&ship.position).gravity();
                        let boost = (-g.0, -g.1);
                        cmds.push(Command::Accelerate {
                            ship_id: ship.id,
                            vector: boost,
                        });
                        continue;
                    }
                }
            }
        }

        cmds
    }
}
