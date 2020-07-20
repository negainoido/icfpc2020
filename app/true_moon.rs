use crate::protocol::*;
use crate::utility::*;
use std::collections::{HashMap, HashSet, VecDeque};

pub struct TrueMoon {
    table: HashMap<(Coord, Coord), VisitStatus>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum VisitStatus {
    NotVisited,
    DeadEnd,
    Loop(usize),
}

use VisitStatus::*;

impl TrueMoon {
    const LIMIT_FIELD_INNER: i128 = 17;
    const LIMIT_FIELD_OUTER: i128 = 130;
    const LIMIT_VELOCITY: i128 = 10;

    pub fn new() -> Self {
        Self {
            table: HashMap::new(),
        }
    }
    fn add_coord(c0: &Coord, c1: &Coord) -> Coord {
        Coord::from((c0.0 + c1.0, c0.1 + c1.1))
    }
    fn new_velocity(position: &Coord, velocity: &Coord) -> Coord {
        Self::add_coord(velocity, &gravity_of(position))
    }

    fn new_position(position: &Coord, velocity: &Coord) -> Coord {
        Self::add_coord(position, &Self::new_velocity(position, velocity))
    }

    fn is_out_of_field(position: &Coord) -> bool {
        let limit = Self::LIMIT_FIELD_OUTER;
        position.0 < -limit || position.0 > limit || position.1 < -limit || position.1 > limit
    }

    fn is_inside_planet(position: &Coord) -> bool {
        let limit = Self::LIMIT_FIELD_INNER;
        -limit <= position.0 && position.0 <= limit && -limit <= position.1 && position.1 <= limit
    }

    fn is_invalid_position(position: &Coord) -> bool {
        Self::is_out_of_field(position) || Self::is_inside_planet(position)
    }

    fn is_invalid_velocity(velocity: &Coord) -> bool {
        let limit = Self::LIMIT_VELOCITY;
        velocity.0.abs() > limit || velocity.1.abs() > limit
    }

    fn lookup_table(&self, position: &Coord, velocity: &Coord) -> VisitStatus {
        if Self::is_invalid_position(position) {
            return DeadEnd;
        }
        *self
            .table
            .get(&(*position, *velocity))
            .unwrap_or(&NotVisited)
    }

    fn update_table(&mut self, position: &Coord, velocity: &Coord, value: VisitStatus) {
        assert!(!Self::is_out_of_field(position));
        self.table.insert((*position, *velocity), value);
    }

    fn simulate(&mut self, position: &Coord, velocity: &Coord, id: usize) {
        let mut pos = position.clone();
        let mut vel = velocity.clone();
        loop {
            if Self::is_invalid_position(&pos) || Self::is_invalid_velocity(&vel) {
                return;
            }
            match self.lookup_table(position, velocity) {
                NotVisited => {
                    self.update_table(&pos, &vel, Loop(id));
                    let newpos = Self::new_position(&pos, &vel);
                    let newvel = Self::new_velocity(&pos, &vel);
                    pos = newpos;
                    vel = newvel;
                    continue;
                }
                _ => break,
            }
        }
    }

    pub fn get_boost(&mut self, mother_ship: &Ship) -> Option<Command> {
        let mut states = HashSet::new();
        let mut prev_states = HashMap::new();
        let mut prev_actions = HashMap::new();
        let mut state_queue = VecDeque::new();
        states.insert((mother_ship.position, mother_ship.velocity));
        state_queue.push_back((mother_ship.position, mother_ship.velocity));

        let mut newlabel = 0;

        while let Some((position, velocity)) = state_queue.pop_front() {
            let curr_state = (position, velocity);
            if self.lookup_table(&position, &velocity) == NotVisited {
                newlabel += 1;
                self.simulate(&position, &velocity, newlabel);
            }
            match self.lookup_table(&position, &velocity) {
                Loop(_) => {
                    let mut curr_state = curr_state;
                    let mut last_action = None;
                    eprintln!("FOUND!!! {:?}", curr_state);
                    while let Some(prev_state) = prev_states.get(&curr_state) {
                        eprintln!("<- {:?}", prev_state);
                        let last_accel: (i128, i128) = *prev_actions.get(&curr_state).unwrap();
                        last_action = Some(Command::Accelerate {
                            ship_id: 0,
                            vector: (-last_accel.0, -last_accel.1),
                        });
                        curr_state = *prev_state;
                    }
                    return last_action;
                }
                _ => {}
            }

            for ax in -1..=1 {
                for ay in -1..=1 {
                    if ax == 0 && ay == 0 {
                        continue;
                    }
                    let acceleration = Coord::from((ax, ay));
                    let accelerated_velocity = Self::add_coord(&velocity, &acceleration);
                    let new_velocity = Self::new_velocity(&position, &accelerated_velocity);
                    let new_position = Self::new_position(&position, &accelerated_velocity);
                    let new_state = (new_position, new_velocity);
                    if !Self::is_invalid_position(&new_position) && !states.contains(&new_state) {
                        states.insert(new_state);
                        state_queue.push_back(new_state);
                        prev_states.insert(new_state, curr_state);
                        prev_actions.insert(new_state, acceleration);
                    }
                }
            }
        }
        eprintln!("Gave up searching a new orbit...");
        None
    }
}
