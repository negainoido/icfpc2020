use crate::ai::AI;
use crate::protocol::*;

pub struct Moon {}

const LEN: i128 = 16;

enum WallType {
    A, // x = LEN
    B, // y = -LEN
    C, // x = -LEN
    D, // y = LEN
}

impl AI for Moon {
    fn new() -> Self {
        Moon {}
    }

    fn main(&mut self, info: &GameInfo, state: &GameState) -> Vec<Command> {
        let my_role = &info.role;
        let my_ships: Vec<&Ship> = state
            .ship_and_commands
            .iter()
            .filter(|(s, _)| s.role == *my_role)
            .map(|(s, _)| s)
            .collect();
        let mut commands = Vec::<Command>::new();
        for ship in my_ships {
            let boost = Moon::get_boost(&ship.position, &ship.velocity);
            if boost != (0, 0) {
                commands.push(Command::Accelerate {
                    ship_id: ship.id.clone(),
                    vector: boost,
                });
            }
        }
        commands
    }
}

impl Moon {
    fn get_boost(pos: &Coord, velocity: &Coord) -> Coord {
        let wall = Moon::ground_wall(pos);
        let (mut gdx, mut gdy) = Moon::base_gravity(&wall);
        let complete = Moon::complete_func(&wall);
        let is_crash = Moon::is_crash_func(&wall);
        let mut cur_pos = pos.clone();
        let mut cur_velocity = velocity.clone();
        loop {
            cur_pos.0 += cur_velocity.0;
            cur_pos.1 += cur_velocity.1;
            if is_crash(cur_pos) {
                return Moon::helper_boost(&wall);
            }
            if complete(cur_pos) {
                // No boost is needed
                return (0, 0);
            }
            cur_velocity.0 += gdx;
            cur_velocity.1 += gdx;
            if gdx > 0 {
                gdx += 1;
            }
            if gdy > 0 {
                gdy += 1;
            }
        }
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
