use crate::ai::*;
use crate::protocol::{Command, GameInfo, GameState, Ship};

pub struct FloatingAI {}

impl FloatingAI {
    fn ship_floating_command(&self, ship: &Ship) -> Command {
        let gx = match ship.position {
            (x, y) if x >= y && x >= -y => -1,
            (x, y) if x <= y && x <= -y => 1,
            _ => 0,
        };

        let gy = match ship.position {
            (x, y) if y >= x && y >= -x => -1,
            (x, y) if y <= x && y <= -x => 1,
            _ => 0,
        };

        return Command::Accelerate {
            ship_id: ship.id,
            vector: (gx, gy),
        };
    }
}

impl AI for FloatingAI {
    fn new() -> Self {
        FloatingAI {}
    }

    fn main(&mut self, info: &GameInfo, state: &GameState) -> Vec<Command> {
        let my_ships: Vec<&Ship> = state
            .ship_and_commands
            .iter()
            .map(|(s, _)| s)
            .filter(|s| s.role == info.role)
            .collect();
        my_ships
            .iter()
            .map(|s| self.ship_floating_command(s))
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use crate::ai::AI;
    use crate::protocol::{Command, Role, Ship};

    use super::FloatingAI;

    #[test]
    fn test0() {
        let floating_ai: FloatingAI = FloatingAI::new();
        let ship = Ship {
            role: Role::Attacker,
            id: 0,
            position: (10, 9),
            velocity: (0, 0),
        };

        let command = floating_ai.ship_floating_command(&ship);
        assert_eq!(
            command,
            Command::Accelerate {
                ship_id: 0,
                vector: (-1, 0)
            }
        )
    }

    #[test]
    fn test1() {
        let floating_ai: FloatingAI = FloatingAI::new();
        let ship = Ship {
            role: Role::Attacker,
            id: 1,
            position: (-10, -10),
            velocity: (2, 1),
        };

        let command = floating_ai.ship_floating_command(&ship);
        assert_eq!(
            command,
            Command::Accelerate {
                ship_id: 1,
                vector: (1, 1)
            }
        );
    }
}
