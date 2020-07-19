#![allow(dead_code, unused_variables)]
use crate::ai::*;
use crate::moon::Moon;
use crate::protocol::*;

pub struct CympfhAI {}

// .----> x0
// |
// |
// v x1

enum Section {
    Up,
    Right,
    Left,
    Down,
    DiagRU,
    DiagRD,
    DiagLU,
    DiagLD,
    O,
}

impl Section {
    fn from(x: Coord) -> Section {
        use Section::*;
        let (x0, x1) = x;
        if x0 + x1 == 0 && x0 > 0 {
            DiagRU
        } else if x0 == x1 && x0 > 0 {
            DiagRD
        } else if x0 + x1 == 0 && x0 < 0 {
            DiagLD
        } else if x0 == x1 && x0 < 0 {
            DiagLU
        } else if x0 > x1.abs() {
            Right
        } else if x1 > x0.abs() {
            Down
        } else if -x0 > x1.abs() {
            Left
        } else if -x1 > x0.abs() {
            Up
        } else {
            O
        }
    }
}

impl AI for CympfhAI {
    fn new() -> Self {
        Self {}
    }
    fn main(&mut self, _info: &GameInfo, _state: &GameState) -> Vec<Command> {
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

        let boost = Moon::get_boost(&ship_self.position, &ship_self.velocity);
        if boost != (0, 0) {
            return vec![Command::Accelerate {
                ship_id: ship_self.id,
                vector: boost,
            }];
        } else if ship_self.role == Role::Attacker {
            return vec![Command::Shoot {
                ship_id: ship_self.id,
                target: (
                    ship_enemy.position.0 + ship_enemy.velocity.0,
                    ship_enemy.position.1 + ship_enemy.velocity.1,
                ),
            }];
        }

        vec![Command::Accelerate {
            ship_id: ship_self.id,
            vector: (-1, -1),
        }]
    }
}
