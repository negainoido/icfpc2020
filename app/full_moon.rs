use crate::ai::AI;
use crate::moon::Moon;
use crate::protocol::*;
use crate::utility::*;

type BaseMoon = Moon; // NewMoon

pub struct FullMoon {
    base_moon: BaseMoon,
}

impl AI for FullMoon {
    fn new() -> Self {
        FullMoon {
            base_moon: BaseMoon::new(),
        }
    }

    fn main(&mut self, info: &GameInfo, state: &GameState) -> Vec<Command> {
        let mut commands = self.base_moon.main(info, state);
        if !commands.is_empty() {
            return commands;
        }
        let my_role = &info.role;
        let my_ships: Vec<&Ship> = state
            .ship_and_commands
            .iter()
            .filter(|(s, _)| s.role == *my_role)
            .map(|(s, _)| s)
            .collect();
        let enemy_ships: Vec<&Ship> = state
            .ship_and_commands
            .iter()
            .filter(|(s, _)| s.role != *my_role)
            .map(|(s, _)| s)
            .collect();
        dbg!(info.x4.len());
        let cool = info.x4[2];
        for target_ship in enemy_ships {
            let pos = &target_ship.position;
            let (gdx, gdy) = gravity_of(pos);
            let velocity = &target_ship.velocity;
            let next_pos = (pos.0 + velocity.0 + gdx, pos.1 + velocity.1 + gdy);
            commands.push(Command::Shoot {
                ship_id: my_ships[0].id.clone(),
                target: next_pos,
                power: cool,
            });
            break;
        }
        commands
    }
}
