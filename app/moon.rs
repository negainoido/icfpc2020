use crate::ai::AI;
use crate::protocol::*;

pub struct Moon {
    round: i128,
}

const LEN: i128 = 16;

#[derive(Debug)]
enum WallType {
    A, // x = LEN
    B, // y = -LEN
    C, // x = -LEN
    D, // y = LEN
}

impl AI for Moon {
    fn new() -> Self {
        Moon { round: 0 }
    }

    fn main(&mut self, info: &GameInfo, state: &GameState) -> Vec<Command> {
        let my_role = &info.role;

        let my_ships: Vec<&Ship> = state.get_ships(my_role);
        let mut enemy_ships: Vec<&Ship> = state.get_ships(&my_role.opponent());

        let mut commands = Vec::<Command>::new();
        for ship in my_ships {
            let boost = Moon::get_boost(&ship.position, &ship.velocity);
            if boost != (0, 0) {
                commands.push(Command::Accelerate {
                    ship_id: ship.id.clone(),
                    vector: boost,
                });
            } else {
                if enemy_ships.is_empty() {
                    continue;
                }
                let target_ship: &Ship = enemy_ships.pop().unwrap();
                let next_target_pos =
                    Moon::get_next_pos(&target_ship.position, &target_ship.velocity);
                commands.push(Command::Shoot {
                    ship_id: target_ship.id.clone(),
                    target: next_target_pos,
                    power: 4,
                })
            }
        }

        self.round += 1;
        commands
    }
}

impl Moon {
    pub fn get_boost(pos: &Coord, velocity: &Coord) -> Coord {
        let wall = Moon::ground_wall(pos);
        let complete = Moon::complete_func(&wall);
        let is_crash = Moon::is_crash_func(&wall);
        let mut cur_pos = pos.clone();
        let mut cur_velocity = velocity.clone();
        loop {
            let (gdx, gdy) = gravity_of(&cur_pos);
            cur_velocity.0 += gdx;
            cur_velocity.1 += gdy;
            cur_pos.0 += cur_velocity.0;
            cur_pos.1 += cur_velocity.1;
            if complete(cur_pos) {
                // No boost is needed
                return (0, 0);
            }
            if is_crash(cur_pos) {
                // Compute the helper boost from the initial position.
                return Moon::helper_boost(&pos);
            }
        }
    }

    pub fn get_next_pos(pos: &Coord, velocity: &Coord) -> Coord {
        let (gdx, gdy) = gravity_of(&pos);

        (pos.0 + velocity.0 + gdx, pos.1 + velocity.1 + gdy)
    }

    fn complete_func(wall: &WallType) -> Box<dyn Fn(Coord) -> bool> {
        match wall {
            WallType::A => Box::new(|(_x, y)| y < -LEN),
            WallType::B => Box::new(|(x, _y)| x < -LEN),
            WallType::C => Box::new(|(_x, y)| y > LEN),
            WallType::D => Box::new(|(x, _y)| x > LEN),
        }
    }

    fn is_crash_func(wall: &WallType) -> Box<dyn Fn(Coord) -> bool> {
        match wall {
            WallType::A => Box::new(|(x, _y)| x <= LEN),
            WallType::B => Box::new(|(_x, y)| y >= -LEN),
            WallType::C => Box::new(|(x, _y)| x >= -LEN),
            WallType::D => Box::new(|(_x, y)| y <= LEN),
        }
    }

    fn ground_wall((x, y): &Coord) -> WallType {
        if *x > 0 {
            // A, D or B.
            if *y > *x {
                WallType::D
            } else if *y < -*x {
                WallType::B
            } else {
                if *y == -*x {
                    WallType::B
                } else {
                    WallType::A
                }
            }
        } else {
            // B, C or D.
            if *y > -*x {
                WallType::D
            } else if *y < *x {
                WallType::B
            } else {
                if *y == -*x {
                    WallType::D
                } else {
                    WallType::C
                }
            }
        }
    }

    fn helper_boost((x, y): &Coord) -> Coord {
        // NOTE: Accelerate command comment
        // => Accelerates ship identified by shipId to the direction **opposite** to vector.
        // Therefore, to accelerate the ship to (+1, +1), the proper boost value is (-1, -1).
        if *x > 0 {
            // A, D or B.
            if *y > *x {
                // D
                (-1, -1)
            } else if *y < -*x {
                // B
                (1, 1)
            } else {
                if *y == *x {
                    // DA
                    return (-1, 0);
                } else if *y == -*x {
                    // AB
                    return (0, 1);
                }
                (-1, 1)
            }
        } else {
            // B, C or D.
            if *y > -*x {
                // D
                (-1, -1)
            } else if *y < *x {
                // B
                (1, 1)
            } else {
                if *y == -*x {
                    // CD
                    return (0, -1);
                } else if *y == *x {
                    // BC
                    return (1, 0);
                }
                // C
                (1, -1)
            }
        }
    }
}

#[test]
fn start_with_corner() {
    let init_velocity = (0, 0);
    for init_pos in &[(48, 48), (-48, 48), (48, -48), (-48, -48)] {
        assert_ne!((0, 0), Moon::get_boost(&init_pos, &init_velocity));
    }
}
