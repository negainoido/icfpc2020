#![allow(dead_code, unused_variables)]
use crate::ai::*;
use crate::protocol::*;

pub struct NewMoon {
    state_history: Vec<GameState>,
    command_history: Vec<Vec<Command>>,
}

impl Ship {
    fn apply(&mut self, command: &Command) {
        match command {
            Command::Accelerate { ship_id, vector } => {
                if *ship_id != self.id {
                    return;
                }

                self.velocity.0 -= vector.0;
                self.velocity.1 -= vector.1;
            }
            _ => (),
        };
    }

    fn next_steps(&mut self, number_of_steps: i128) {
        for i in 0..number_of_steps {
            self.next();
        }
    }

    fn next(&mut self) {
        let (gx, gy) = gravity_of(&self.position);
        self.velocity.0 += gx;
        self.velocity.1 += gy;

        self.position.0 += self.velocity.0;
        self.position.1 += self.velocity.1;
    }

    fn apply_commands(&mut self, commands: &Vec<Vec<Command>>) {
        for cmds in commands {
            // one turn
            for cmd in cmds {
                self.apply(cmd);
            }
            // gravity
            self.next();
        }
    }

    fn is_inside(&self) -> bool {
        self.position.0.abs() <= 128 && self.position.1.abs() <= 128
    }
    fn is_on_earth(&self) -> bool {
        self.position.0.abs() <= 16 && self.position.1.abs() <= 16
    }
    fn is_safe(&self) -> bool {
        return self.is_inside() && !self.is_on_earth();
    }

    fn is_safe_after(&self, number_of_steps: i128) -> bool {
        let mut ship = self.clone();
        for _ in 0..number_of_steps {
            if !ship.is_safe() {
                return false;
            }
            ship.next();
        }
        return ship.is_safe();
    }

    fn safe_until(&self) -> i128 {
        let mut ship = self.clone();
        for i in 0..256 {
            if !ship.is_safe() {
                return i;
            }
            ship.next();
        }
        return 256;
    }
}

impl NewMoon {
    pub fn get_boost(ship: &Ship, _state: &GameState) -> Option<Command> {
        let remaining_turn = 256 - _state.tick;
        if !ship.is_safe_after(remaining_turn) {
            let mut best = -1;
            let mut bestacc = Command::Accelerate {
                ship_id: ship.id,
                vector: (0, 0),
            };
            for x in -1..=1 {
                for y in -1..=1 {
                    if x == 0 && y == 0 {
                        continue;
                    }
                    let mut ship = ship.clone();
                    let acc = Command::Accelerate {
                        ship_id: ship.id,
                        vector: (x, y),
                    };
                    ship.apply(&acc);
                    let num = ship.safe_until();
                    if best < num {
                        best = num;
                        bestacc = acc;
                    }
                }
            }
            return Some(bestacc);
        }
        None
    }

    fn compute(&self, info: &GameInfo, state: &GameState) -> Vec<Command> {
        let role_self = info.role;
        let ship_self: &Ship = state.get_ships(&role_self)[0];
        let ship_enemy: &Ship = state.get_ships(&role_self.opponent())[0];

        let commands_enemy = state
            .ship_and_commands
            .iter()
            .filter(|&(ship, _)| ship.role != role_self)
            .map(|(_, commands)| commands)
            .next()
            .unwrap();

        let ships = &self.state_history[0].get_ships(&role_self);
        let expected_ship = {
            let mut ship = ships[0].clone();
            ship.apply_commands(&self.command_history);
            ship
        };

        println!("expected ship: {:?}", expected_ship);
        println!("true ship    : {:?}", ship_self);

        assert!(expected_ship.position == ship_self.position);

        let remaining_turn = 256 - state.tick;
        if !ship_self.is_safe_after(remaining_turn) {
            let mut best = -1;
            let mut bestacc = Command::Accelerate {
                ship_id: ship_self.id,
                vector: (0, 0),
            };
            for x in -1..=1 {
                for y in -1..=1 {
                    if x == 0 && y == 0 {
                        continue;
                    }
                    let mut ship = ship_self.clone();
                    let acc = Command::Accelerate {
                        ship_id: ship.id,
                        vector: (x, y),
                    };
                    ship.apply(&acc);
                    let num = ship.safe_until();
                    if best < num {
                        best = num;
                        bestacc = acc;
                    }
                }
            }
            return vec![bestacc];
        }

        return vec![];
    }
}

impl AI for NewMoon {
    fn new() -> Self {
        Self {
            command_history: vec![],
            state_history: vec![],
        }
    }

    fn main(&mut self, info: &GameInfo, state: &GameState) -> Vec<Command> {
        self.state_history.push(state.clone());
        let res = self.compute(info, state);
        self.command_history.push(res.clone());
        return res;
    }
}
