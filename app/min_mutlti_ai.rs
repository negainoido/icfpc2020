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

const CLONE_FUEL_THRESHOLD: i128 = 32;
const CLONE_CAPCITY_THRESHOLD: i128 = 4;

impl AI for MinMultiAi {
    fn new() -> Self {
        MinMultiAi {
            round: 0,
            history: Vec::with_capacity(256),
        }
    }

    fn main(&mut self, info: &GameInfo, state: &GameState) -> Vec<Command> {
        let my_role = &info.role;
        let mut my_ships: Vec<Ship> = state
            .ship_and_commands
            .iter()
            .filter(|(s, _)| s.role == *my_role)
            .map(|(s, _)| s.clone())
            .collect();
        let enemy_ships: Vec<Ship> = state
            .ship_and_commands
            .iter()
            .filter(|(s, _)| s.role != *my_role)
            .map(|(s, _)| s.clone())
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
            if boost != (0, 0) {
                commands.push(Command::Accelerate {
                    ship_id: ship.id.clone(),
                    vector: boost,
                });
            } else {
                if enemy_ships.is_empty() {
                    continue;
                }
                let target_ship: &Ship = &enemy_ships[0];
                let next_target_pos =
                    Moon::get_next_pos(&target_ship.position, &target_ship.velocity);
                commands.push(Command::Shoot {
                    ship_id: ship.id.clone(),
                    target: next_target_pos,
                    power: 4,
                })
            }
            if ship.x4[3] > 1
                && ship.x4[0] / 2 >= CLONE_FUEL_THRESHOLD
                && ship.x4[2] / 2 >= CLONE_CAPCITY_THRESHOLD
            {
                commands.push(Command::Clone {
                    ship_id: ship.id.clone(),
                    fuel: ship.x4[0] / 2,
                    x2: ship.x4[1],
                    capacity: ship.x4[2] / 2,
                    units: ship.x4[3],
                })
            }
        }

        let state = State {
            my_ships,
            enemy_ships,
        };

        self.round += 1;
        self.history.push(state);
        commands
    }
}
