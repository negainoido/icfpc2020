use crate::protocol::{Command, GameInfo, GameState};

pub trait AI {
    fn new() -> Self;

    fn main(&mut self, _info: &GameInfo, _state: &GameState) -> Vec<Command> {
        vec![]
    }
}
