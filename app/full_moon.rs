use crate::ai::AI;
use crate::moon::Moon;
use crate::protocol::*;
// use crate::new_moon::NewMoon;

type BaseMoon = Moon; // NewMoon

pub struct FullMoon {
    base_moon: BaseMoon,
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

impl AI for FullMoon {
    fn new() -> Self {
        FullMoon {
            base_moon: BaseMoon::new(),
        }
    }

    fn near(a: &Ship, b: &Ship) -> bool {
        const nearDiff: i128 = 16 * 16;
        let (&x1, &y1) = a.position;
        let (&x2, &y2) = a.position;
        let xd = abs(x2 - x1);
        let yd = abs(y2 - y1);
        nearDiff >= xd * xd + yd * yd
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
        let my_ship = my_ships[0];
        let target_ship = &enemy_ships[0];
        if (!near(&my_sip, &target_ship)) {
            // Move to the target_ship.
            commands.push(move_to_target(my_ship.clone(), target_ship.clone()));
            return commands;
        }

        let next_pos = target_ship.clone().next().position;
        commands.push(Command::Shoot {
            ship_id: my_ships[0].id.clone(),
            target: next_pos,
            power: cool,
        });
        commands
    }
}
