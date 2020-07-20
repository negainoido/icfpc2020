#![allow(dead_code, unused_variables)]
use crate::ai::*;
use crate::moon::Moon;
use crate::protocol::*;
use crate::utility::*;

pub struct CympfhAI {
    rand: XorShift,
}

impl CympfhAI {
    fn estimate_next_position(ship: &Ship) -> Ship {
        let pos = if ship.velocity == (0, 0) {
            // 停止してる
            ship.position
        } else {
            add(
                &add(&ship.position, &ship.velocity),
                &gravity_of(&ship.position),
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

fn dist(p1: Coord, p2: Coord) -> i128 {
    (p1.0 - p2.0) * (p1.0 - p2.0) + (p1.1 - p2.1) * (p1.1 - p2.1)
}

fn dist_max(x: &Coord, y: &Coord) -> i128 {
    std::cmp::max((x.0 - y.0).abs(), (x.1 - y.1).abs())
}

fn dist_manhattan(x: &Coord, y: &Coord) -> i128 {
    (x.0 - y.0).abs() + (x.1 - y.1).abs()
}

fn close_manhattan(a: &Ship, b: &Ship, dist_sup: i128) -> bool {
    let x = add(&add(&a.position, &a.velocity), &gravity_of(&a.position));
    let y = add(&add(&b.position, &b.velocity), &gravity_of(&b.position));
    dist_manhattan(&x, &y) < dist_sup
}

fn close_max(a: &Ship, b: &Ship, dist_sup: i128) -> bool {
    let x = add(&add(&a.position, &a.velocity), &gravity_of(&a.position));
    let y = add(&add(&b.position, &b.velocity), &gravity_of(&b.position));
    dist_max(&x, &y) < dist_sup
}

fn shoot_target<'a>(
    my_ship: &Ship,
    enemy_ships: &Vec<&'a Ship>,
    max_power: i128,
) -> Option<(&'a Ship, i128)> {
    if max_power <= 0 {
        return None;
    }

    let mut ei: i32 = -1;
    let mut nearest_dist: i128 = std::i128::MAX;
    for i in 0..enemy_ships.len() {
        let e = &enemy_ships[i];
        let e_d = dist(my_ship.position, e.position);
        if nearest_dist > e_d {
            nearest_dist = e_d;
            ei = i as i32;
        }
    }
    if ei < 0 {
        return None;
    }

    Some((&enemy_ships[ei as usize], max_power))
}

impl AI for CympfhAI {
    fn new() -> Self {
        Self {
            rand: XorShift::new(),
        }
    }
    fn main(&mut self, info: &GameInfo, state: &GameState) -> Vec<Command> {
        use Role::*;

        let role_self = info.role;
        let self_ships: Vec<&Ship> = state.get_ships(&role_self);
        let enemy_ships: Vec<&Ship> = state.get_ships(&role_self.opponent());

        let role = self_ships[0].role;

        let commands_enemy = state
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
            {
                let boost = Moon::get_boost(&ship.position, &ship.velocity);
                if boost != (0, 0) {
                    cmds.push(Command::Accelerate {
                        ship_id: ship.id,
                        vector: boost,
                    });
                    continue;
                }
            }
            // ビーム
            match shoot_target(ship, &enemy_ships, ship.x6 - ship.x5) {
                Some((enemy_ship, power)) => {
                    let target = CympfhAI::estimate_next_position(enemy_ship);
                    cmds.push(Command::Shoot {
                        ship_id: ship.id,
                        target: target.position,
                        power,
                    });
                    continue;
                }
                None => {}
            }
            // Swing-By
            if role == Attacker {
                let mut done = false;
                for &enemy_ship in enemy_ships.iter() {
                    let target = CympfhAI::estimate_next_position(&enemy_ship);
                    let self_nop = CympfhAI::estimate_next_position(&ship);
                    let mut self_acc = ship.clone();
                    let g = gravity_of(&self_acc.position);
                    self_acc.velocity.0 += g.0;
                    self_acc.velocity.1 += g.1;
                    self_acc = CympfhAI::estimate_next_position(&self_acc);
                    let d1 = dist_manhattan(&target.position, &self_nop.position);
                    let d2 = dist_manhattan(&target.position, &self_acc.position);
                    if d2 < d1 {
                        let g = gravity_of(&ship.position);
                        let boost = (-g.0, -g.1);
                        cmds.push(Command::Accelerate {
                            ship_id: ship.id,
                            vector: boost,
                        });
                        done = true;
                    }
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
                        let g = gravity_of(&ship.position);
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
                if dist_max(&(0, 0), &ship.position) > 32 {
                    if self.rand.gen::<i128>() % 20 == 0 {
                        let g = gravity_of(&ship.position);
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
