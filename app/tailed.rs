#![allow(dead_code, unused_variables)]
use crate::ai::*;
use crate::moon::Moon;
use crate::protocol::*;

pub struct TailedAI {
    command_history: Vec<Vec<Command>>,
}

impl TailedAI {
    fn compute(&mut self, _info: &GameInfo, _state: &GameState) -> Vec<Command> {
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

        let boost = Moon::get_boost(&ship_self.position, &ship_self.velocity);

        dbg!(&boost);
        dbg!(&self.command_history);

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
        }
    }

    fn main(&mut self, _info: &GameInfo, _state: &GameState) -> Vec<Command> {
        let res = self.compute(_info, _state);
        self.command_history.push(res.clone());
        return res;
    }
}
