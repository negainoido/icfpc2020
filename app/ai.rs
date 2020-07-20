use crate::protocol::*;

pub trait AI {
    fn new() -> Self
    where
        Self: Sized;

    fn initial_params(info: &GameInfo) -> ShipState
    where
        Self: Sized,
    {
        match info.role {
            Role::Attacker => ShipState::new(134, 64, 10, 1),
            Role::Defender => ShipState::new(70, 64, 10, 1),
        }
    }

    fn main(&mut self, _info: &GameInfo, _state: &GameState) -> Vec<Command> {
        vec![]
    }
}
