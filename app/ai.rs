use crate::protocol::*;

pub trait AI {
    fn new() -> Self;

    fn initial_params(info: &GameInfo) -> (u32, u32, u32, u32) {
        match info.role {
            Role::Attacker => (134, 64, 10, 1),
            Role::Defender => (70, 64, 10, 1),
        }
    }

    fn main(&mut self, _info: &GameInfo, _state: &GameState) -> Vec<Command> {
        vec![]
    }
}
