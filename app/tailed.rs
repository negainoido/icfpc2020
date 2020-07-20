#![allow(dead_code, unused_variables)]
use crate::ai::*;
use crate::moon::Moon;
use crate::protocol::*;

pub struct TailedAI {
    state_history: Vec<GameState>,
    command_history: Vec<Vec<Command>>,
}
fn ships_of_role(state: &GameState, role: Role) -> Vec<Ship> {
    return state
        .ship_and_commands
        .iter()
        .filter(|&(ship, _)| ship.role == role)
        .map(|(ship, _)| ship)
        .cloned()
        .collect::<Vec<Ship>>();
}

fn gravity_of(pos: &Coord) -> Coord {
    let mut gx = 0;
    let mut gy = 0;
    let &(x, y) = pos;
    if x >= y.abs() {
        gx -= 1;
    }
    if y >= x.abs() {
        gy -= 1;
    }
    if x <= -y.abs() {
        gx += 1;
    }
    if y <= -x.abs() {
        gy += 1;
    }
    return (gx, gy);
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

    fn next(&mut self) {
        let (gx, gy) = gravity_of(&self.position);
        self.velocity.0 += gx;
        self.velocity.1 += gy;

        self.position.0 += self.velocity.0;
        self.position.1 += self.velocity.1;
    }

    fn apply_commands(&mut self, commands: &Vec<Vec<Command>>) {
        for cmds in commands.iter() {
            // one turn
            for cmd in cmds.iter() {
                self.apply(cmd);
            }
            // gravity
            self.next();
        }
    }
}

impl TailedAI {
    fn compute(&self, _info: &GameInfo, _state: &GameState) -> Vec<Command> {
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

        let commands_enemy = _state
            .ship_and_commands
            .iter()
            .filter(|&(ship, _)| ship.role != role_self)
            .map(|(_, commands)| commands)
            .next()
            .unwrap();

        let ships = ships_of_role(&self.state_history[0], role_self);
        let expected_ship = {
            let mut ship = ships[0].clone();
            ship.apply_commands(&self.command_history);
            ship
        };

        println!("expected ship: {:?}", expected_ship);
        println!("true ship    : {:?}", ship_self);

        let boost = Moon::get_boost(&ship_self.position, &ship_self.velocity);

        /*
        dbg!(&boost);
        dbg!(&self.command_history);
        */
        for commands in self.command_history.iter() {
            for i in commands.iter() {
                println!("history: {:?}", i)
            }
        }

        if boost != (0, 0) {
            return vec![Command::Accelerate {
                ship_id: ship_self.id,
                vector: boost,
            }];
        }

        return vec![];
    }
}

impl AI for TailedAI {
    fn new() -> Self {
        Self {
            command_history: vec![],
            state_history: vec![],
        }
    }

    fn main(&mut self, _info: &GameInfo, _state: &GameState) -> Vec<Command> {
        self.state_history.push(_state.clone());
        let res = self.compute(_info, _state);
        self.command_history.push(res.clone());
        return res;
    }
}
