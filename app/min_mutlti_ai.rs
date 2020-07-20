use crate::ai::AI;
use crate::moon::Moon;
use crate::protocol::*;

#[derive(Clone)]
struct State {
    my_ships: Vec<Ship>,
    enemy_ships: Vec<Ship>,
}

pub struct MinMultiAi {
    round: i128,
    history: Vec<State>,
}

fn get_distance(s: &Coord, t: &Coord) -> f64 {
    let x = (s.0 - t.0) as f64;
    let y = (s.1 - t.1) as f64;
    (x * x + y * y).sqrt()
}

const SHOT_THRESHOLD: f64 = 200.0;

fn find_target<'a>(ship: &Ship, enemies: &Vec<&'a Ship>) -> Option<&'a Ship> {
    let mut result: Option<&Ship> = None;
    let pos = Moon::get_next_pos(&ship.position, &ship.velocity);
    for &e in enemies.iter() {
        let epos = Moon::get_next_pos(&e.position, &e.velocity);
        let d = get_distance(&pos, &epos);
        if d < SHOT_THRESHOLD {
            if let Some(current) = result {
                let cpos = Moon::get_next_pos(&current.position, &current.velocity);
                if get_distance(&pos, &cpos) > d {
                    result = Some(e)
                }
            } else {
                result = Some(e);
            }
        }
    }
    result
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

impl AI for MinMultiAi {
    fn new() -> Self {
        MinMultiAi {
            round: 0,
            history: Vec::with_capacity(256),
        }
    }

    fn initial_params(info: &GameInfo) -> ShipState {
        match info.role {
            Role::Attacker => ATTACK_AVAILABLE_PARAMS[3].clone(),
            Role::Defender => DEFENCE_AVAILABLE_PARAMS[1].clone(),
        }
    }

    fn main(&mut self, info: &GameInfo, state: &GameState) -> Vec<Command> {
        let my_role = &info.role;
        let mut my_ships: Vec<&Ship> = state
            .ship_and_commands
            .iter()
            .filter(|(s, _)| s.role == *my_role)
            .map(|(s, _)| s)
            .collect();
        let enemy_ships: Vec<&Ship> = state
            .ship_and_commands
            .iter()
            .filter(|(s, _)| s.role != *my_role)
            .map(|(s, _)| s)
            .collect();
        my_ships.sort_by(|s, t| s.id.cmp(&t.id));

        let mut commands = Vec::<Command>::new();
        dbg!(my_ships.len());

        for ship in my_ships.iter() {
            let mut boost = Moon::get_boost(&ship.position, &ship.velocity);

            for t in my_ships.iter() {
                // change ship direction after cloning
                if t.id < ship.id && t.position == ship.position && t.velocity == ship.velocity {
                    // TODO: improve this logic
                    if boost.0 <= 0 {
                        boost.0 += 1;
                    } else if boost.1 <= 0 {
                        boost.1 += 1;
                    } else {
                        boost.0 -= 1;
                    }
                }
            }

            dbg!(&boost);
            if boost != (0, 0) && ship.x4[0] > 0 {
                commands.push(Command::Accelerate {
                    ship_id: ship.id.clone(),
                    vector: boost,
                });
            } else {
                if !enemy_ships.is_empty() && ship.x4[1] > 0 {
                    let target = find_target(ship, &enemy_ships);
                    if let Some(target_ship) = target {
                        let next_target_pos =
                            Moon::get_next_pos(&target_ship.position, &target_ship.velocity);
                        commands.push(Command::Shoot {
                            ship_id: ship.id.clone(),
                            target: next_target_pos,
                            power: ship.x4[1],
                        });
                    }
                }
            }
            // クローン可能
            if ship.x4[3] > 1 {
                commands.push(dbg!(Command::Clone {
                    ship_id: ship.id.clone(),
                    child: ShipState {
                        fuel: ship.x4[0] / ship.x4[3],
                        power: ship.x4[1] / ship.x4[3],
                        capacity: ship.x4[2] / ship.x4[3],
                        units: 1
                    },
                }));
            }
        }

        let state = State {
            my_ships: my_ships.iter().map(|s| (*s).clone()).collect(),
            enemy_ships: enemy_ships.iter().map(|e| (*e).clone()).collect(),
        };

        self.round += 1;
        self.history.push(state);
        commands
    }
}
