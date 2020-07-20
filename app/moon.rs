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
        let my_ships: Vec<&Ship> = state
            .ship_and_commands
            .iter()
            .filter(|(s, _)| s.role == *my_role)
            .map(|(s, _)| s)
            .collect();
        let mut enemy_ships: Vec<&Ship> = state
            .ship_and_commands
            .iter()
            .filter(|(s, _)| s.role != *my_role)
            .map(|(s, _)| s)
            .collect();
        let mut commands = Vec::<Command>::new();
        // dbg!(my_ships.len());
        for ship in my_ships {
            let boost = Moon::get_boost(&ship.position, &ship.velocity);
            // dbg!(&boost);
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
    fn is_diag(x: &Coord) -> bool {
        let (x0, x1) = x;
        x0 == x1 || x0 + x1 == 0
    }

    pub fn get_boost(pos: &Coord, velocity: &Coord) -> Coord {
        if Moon::is_diag(&pos) && Moon::is_diag(&velocity) {
            return (0, -pos.1);
        }
        let wall = Moon::ground_wall(pos);
        let (gdx, gdy) = Moon::base_gravity(&wall);
        let complete = Moon::complete_func(&wall);
        let is_crash = Moon::is_crash_func(&wall);
        let mut cur_pos = pos.clone();
        let mut cur_velocity = velocity.clone();
        // dbg!(&wall);
        loop {
            // dbg!(cur_pos);
            // dbg!(cur_velocity);
            cur_pos.0 += cur_velocity.0;
            cur_pos.1 += cur_velocity.1;
            if is_crash(cur_pos) {
                return Moon::helper_boost(&wall);
            }
            if complete(cur_pos) {
                // No boost is needed
                return (0, 0);
            }
            // dbg!(gdx, gdy);
            cur_velocity.0 += gdx;
            cur_velocity.1 += gdy;
        }
    }

    fn get_next_pos(pos: &Coord, velocity: &Coord) -> Coord {
        let wall = Moon::ground_wall(pos);
        let (gdx, gdy) = Moon::base_gravity(&wall);
        (pos.0 + velocity.0 + gdx, pos.1 + velocity.1 + gdy)
    }

    fn helper_boost(wall: &WallType) -> Coord {
        // NOTE: Accelerate command comment
        // => Accelerates ship identified by shipId to the direction **opposite** to vector.
        // Therefore, to accelerate the ship to (+1, +1), the proper boost value is (-1, -1).
        match wall {
            WallType::A => (-1, 1),
            WallType::B => (1, 1),
            WallType::C => (1, -1),
            WallType::D => (-1, -1),
        }
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

    fn base_gravity(wall: &WallType) -> Coord {
        match wall {
            WallType::A => (-1, 0),
            WallType::B => (0, 1),
            WallType::C => (1, 0),
            WallType::D => (0, -1),
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
                WallType::A
            }
        } else {
            // B, C or D.
            if *y > -*x {
                WallType::D
            } else if *y < *x {
                WallType::B
            } else {
                WallType::C
            }
        }
    }
}
