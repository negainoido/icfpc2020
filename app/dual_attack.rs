#![allow(dead_code, unused_variables)]
use crate::ai::*;
use crate::new_moon::NewMoon;
use crate::protocol::*;
use crate::utility::*;

pub struct CympfhAI {
    rand: XorShift,
    atack_waiting: bool,
    main: Option<i128>,
    sub: Option<i128>,
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

#[allow(dead_code)]
const ATTACK_AVAILABLE_PARAMS: &'static [ShipState] = &[
    ShipState {
        fuel: 128,
        power: 47,
        capacity: 16,
        units: 2,
    }, // パワーヒッターコンビ
    ShipState {
        fuel: 128,
        power: 32,
        capacity: 16,
        units: 32,
    }, // 雑魚量産型
    ShipState {
        fuel: 128,
        power: 56,
        capacity: 8,
        units: 32,
    }, // 攻撃型雑魚量産型
    ShipState {
        fuel: 156,
        power: 64,
        capacity: 8,
        units: 2,
    }, // 高速エース型
];

#[allow(dead_code)]
const DEFENCE_AVAILABLE_PARAMS: &'static [ShipState] = &[
    ShipState {
        fuel: 128,
        power: 31,
        capacity: 16,
        units: 2,
    }, // 攻防一体の陣
    ShipState {
        fuel: 128,
        power: 0,
        capacity: 16,
        units: 64,
    }, // 分裂雲隠れの陣
];

impl AI for CympfhAI {
    fn new() -> Self {
        Self {
            rand: XorShift::new(),
            atack_waiting: true,
            main: None,
            sub: None,
        }
    }

    fn initial_params(info: &GameInfo) -> ShipState {
        match info.role {
            Role::Attacker => ATTACK_AVAILABLE_PARAMS[3].clone(),
            Role::Defender => DEFENCE_AVAILABLE_PARAMS[0].clone(),
        }
    }

    fn main(&mut self, info: &GameInfo, state: &GameState) -> Vec<Command> {
        use Role::*;

        let role_self = info.role;
        let mut self_ships: Vec<&Ship> = state.get_ships(&role_self);
        let enemy_ships: Vec<&Ship> = state.get_ships(&role_self.opponent());
        self_ships.sort_by(|s,t| s.id.cmp(&t.id));

        let role = self_ships[0].role;

        let commands_enemy = state
            .ship_and_commands
            .iter()
            .filter(|&(ship, _)| ship.role != role_self)
            .map(|(_, commands)| commands)
            .next()
            .unwrap();

        let mut cmds = vec![];

        if self.main.is_none() {
            self.main = Some(self_ships[0].id);
        }
        if self.sub.is_none() && self_ships.len() >= 2 {
            self.sub = Some(self_ships[1].id);
        }

        let main_ship: Option<&Ship> = self.main.and_then(|id| self_ships.iter().find(|s| s.id == id).map(|s| *s));
        let sub_ship: Option<&Ship> = self.sub.and_then(|id| self_ships.iter().find(|s| s.id == id).map(|s| *s));

        main_ship.map(|ship| {
            // 衛星軌道
            if let Some(boost) = NewMoon::get_boost(&ship, &state) {
                cmds.push(boost);
                return;
            }
            // clone sub
            if sub_ship.is_none() && ship.x4[3] >= 2 {
                cmds.push(Command::Clone {
                    ship_id: ship.id.clone(),
                    child: ShipState {
                        fuel: ship.x4[0] / ship.x4[3],
                        power: 0,
                        capacity: ship.x4[2] / ship.x4[3],
                        units: 1
                    },
                });
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
                    return;
                }
            }

        });
        sub_ship.map(|ship| {
            // 自爆
            if role == Attacker
                && enemy_ships.len() <= self_ships.len()
                && close_max(&ship, &enemy_ships[0], DETONATE_DIST)
            {
                cmds.push(Command::Detonate { ship_id: ship.id });
                return;
            }
            // 追跡・撲滅
            if ship.x6 - ship.x5 > 10 && ship.x4[0] > 10 {
                let largest_enemy = enemy_ships.iter().max_by(|e1, e2| e1.x4[3].cmp(&e2.x4[3]));
                if let Some(&enemy) = largest_enemy {
                    let epos = CympfhAI::estimate_next_position(enemy);
                    let mypos = CympfhAI::estimate_next_position(ship);
                    if epos.position.0.signum() == mypos.position.0.signum() && epos.position.1.signum() == mypos.position.1.signum() {
                        let dx = epos.position.0 - mypos.position.0;
                        let dy = epos.position.1 - mypos.position.1;

                        cmds.push(Command::Accelerate {
                            ship_id: ship.id,
                            vector: (dx.signum(), dy.signum()),
                        });
                    }
                }
            }
            // 衛星軌道
            if let Some(boost) = NewMoon::get_boost(&ship, &state) {
                cmds.push(boost);
                return;
            }
        });

        cmds
    }
}
