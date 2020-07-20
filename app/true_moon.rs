use crate::protocol::*;
use crate::utility::*;
use std::collections::{HashMap, HashSet, VecDeque};

pub struct TrueMoon {
    table: Vec<Vec<Vec<Vec<VisitStatus>>>>,
    current_loop_id: i32,
    used_loops: HashSet<i32>,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum VisitStatus {
    NotVisited,
    Visiting,
    DeadEnd,
    Count(i32),
}

use VisitStatus::*;

impl TrueMoon {
    const MIN_CHEBYSHEV_DIST: i128 = 17;
    const MAX_CHEBYSHEV_DIST: i128 = 100;
    const MAX_ABSOLUTE_VELOCITY: i128 = 10;
    const MINIMUM_LOOP_ID: i32 = 1_000_000;

    pub fn new() -> Self {
        let table = vec![
            vec![
                vec![
                    vec![NotVisited; (Self::MAX_ABSOLUTE_VELOCITY * 2 + 1) as usize];
                    (Self::MAX_ABSOLUTE_VELOCITY * 2 + 1) as usize
                ];
                (Self::MAX_CHEBYSHEV_DIST * 2 + 1) as usize
            ];
            (Self::MAX_CHEBYSHEV_DIST * 2 + 1) as usize
        ];
        Self {
            table,
            current_loop_id: Self::MINIMUM_LOOP_ID,
            used_loops: HashSet::new(),
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

    fn get_position_index(pos: i128) -> usize {
        (pos + Self::MAX_CHEBYSHEV_DIST) as usize
    }

    fn get_velocity(vec: i128) -> usize {
        (vec + Self::MAX_ABSOLUTE_VELOCITY) as usize
    }

    fn is_out_of_field(position: &Coord) -> bool {
        if position.0 < -Self::MAX_CHEBYSHEV_DIST || position.0 > Self::MAX_CHEBYSHEV_DIST {
            return true;
        }
        if position.1 < -Self::MAX_CHEBYSHEV_DIST || position.1 > Self::MAX_CHEBYSHEV_DIST {
            return true;
        }
        false
    }

    fn is_inside_planet(position: &Coord) -> bool {
        -Self::MIN_CHEBYSHEV_DIST <= position.0
            && position.0 <= Self::MIN_CHEBYSHEV_DIST
            && -Self::MIN_CHEBYSHEV_DIST <= position.1
            && position.1 <= Self::MIN_CHEBYSHEV_DIST
    }

    fn is_invalid_position(position: &Coord) -> bool {
        Self::is_out_of_field(position) || Self::is_inside_planet(position)
    }

    fn is_invalid_velocity(velocity: &Coord) -> bool {
        if velocity.0 < -Self::MAX_ABSOLUTE_VELOCITY || velocity.0 > Self::MAX_ABSOLUTE_VELOCITY {
            return true;
        }

        if velocity.1 < -Self::MAX_ABSOLUTE_VELOCITY || velocity.1 > Self::MAX_ABSOLUTE_VELOCITY {
            return true;
        }
        false
    }

    fn lookup_table(&self, position: &Coord, velocity: &Coord) -> VisitStatus {
        if Self::is_invalid_position(position) {
            return DeadEnd;
        }
        self.table[Self::get_position_index(position.0)][Self::get_position_index(position.1)]
            [Self::get_velocity(velocity.0)][Self::get_velocity(velocity.1)]
    }

    fn update_table(&mut self, position: &Coord, velocity: &Coord, value: VisitStatus) {
        assert!(!Self::is_out_of_field(position));
        self.table[Self::get_position_index(position.0)][Self::get_position_index(position.1)]
            [Self::get_velocity(velocity.0)][Self::get_velocity(velocity.1)] = value;
    }

    fn is_loop_id(maybe_loop_id: i32) -> bool {
        maybe_loop_id >= Self::MINIMUM_LOOP_ID
    }

    fn dfs(&mut self, position: &Coord, velocity: &Coord) -> VisitStatus {
        if Self::is_invalid_position(position) || Self::is_invalid_velocity(velocity) {
            return DeadEnd;
        }

        if self.lookup_table(position, velocity) == Visiting {
            self.current_loop_id += 1;
            return Count(self.current_loop_id);
        }

        let curr_value = self.lookup_table(position, velocity);
        if curr_value != NotVisited {
            return curr_value;
        }

        self.update_table(position, velocity, Visiting);
        let new_position = Self::new_position(position, velocity);
        let new_velocity = Self::new_velocity(position, velocity);

        match self.dfs(&new_position, &new_velocity) {
            Count(iter_count) if iter_count == self.current_loop_id => {
                self.update_table(position, velocity, Count(self.current_loop_id));
                Count(self.current_loop_id)
            }
            Count(i) => {
                self.update_table(position, velocity, Count(i + 1));
                Count(i + 1)
            }
            _ => {
                self.update_table(position, velocity, Count(1));
                Count(1)
            }
        }
    }

    fn search(&mut self, mother_ship: &Ship) -> Vec<Command> {
        let mut states = HashSet::new();
        let mut prev_states = HashMap::new();
        let mut prev_actions = HashMap::new();
        let mut state_queue = VecDeque::new();
        states.insert((mother_ship.position, mother_ship.velocity));
        state_queue.push_back((mother_ship.position, mother_ship.velocity));

        while let Some((position, velocity)) = state_queue.pop_front() {
            let curr_state = (position, velocity);
            if let Count(maybe_loop_id) = self.dfs(&position, &velocity) {
                if Self::is_loop_id(maybe_loop_id) && !self.used_loops.contains(&maybe_loop_id) {
                    let mut curr_state = curr_state;
                    let mut last_action = Command::Accelerate {
                        ship_id: mother_ship.id,
                        vector: (0, 0),
                    };
                    while let Some(prev_state) = prev_states.get(&curr_state) {
                        let last_accel: (i128, i128) = *prev_actions.get(&curr_state).unwrap();
                        last_action = Command::Accelerate {
                            ship_id: 0,
                            vector: (-last_accel.0, -last_accel.1),
                        };
                        curr_state = *prev_state;
                    }
                    return vec![last_action];
                }
            }

            for ax in -1..=1 {
                for ay in -1..=1 {
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
        dbg!("Gave up searching a new orbit...", self.used_loops.len());
        vec![]
    }

    pub fn get_boost(&mut self, ship: &Ship) -> Option<Command> {
        let vs = self.search(&ship);
        if vs.len() > 0 {
            Some(vs[0].clone())
        } else {
            None
        }
    }
}
