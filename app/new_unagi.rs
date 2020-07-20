#![allow(dead_code, unused_variables)]

use std::collections::{HashMap, HashSet, VecDeque};

use crate::ai::*;
use crate::protocol::*;
use crate::utility::*;

pub struct UnagiCloneAI {
    rand: XorShift,
    defense_ai: DefenseAI,
}

pub trait FromU64 {
    fn coerce(x: u64) -> Self;
}
impl FromU64 for u64 {
    fn coerce(x: u64) -> Self {
        x
    }
}
impl FromU64 for i64 {
    fn coerce(x: u64) -> Self {
        x as i64
    }
}
impl FromU64 for u32 {
    fn coerce(x: u64) -> Self {
        x as u32
    }
}
impl FromU64 for i128 {
    fn coerce(x: u64) -> Self {
        x as i128
    }
}
impl FromU64 for usize {
    fn coerce(x: u64) -> Self {
        x as usize
    }
}
impl FromU64 for bool {
    fn coerce(x: u64) -> Self {
        x % 2 == 0
    }
}

// returns [0, 1]
impl FromU64 for f64 {
    fn coerce(x: u64) -> Self {
        (x as f64) / (std::u64::MAX as f64)
    }
}
impl FromU64 for f32 {
    fn coerce(x: u64) -> Self {
        (x as f32) / (std::u64::MAX as f32)
    }
}

struct XorShift(u64);
impl XorShift {
    fn new() -> Self {
        XorShift(88172645463325252)
    }
    fn next(&mut self) -> u64 {
        let mut x = self.0;
        x = x ^ (x << 13);
        x = x ^ (x >> 7);
        x = x ^ (x << 17);
        self.0 = x;
        x
    }
    fn gen<T: FromU64>(&mut self) -> T {
        FromU64::coerce(self.next())
    }
}

const DETONATE_DIST: i128 = 3;
const BEAM_DIST: i128 = 64;

impl AI for UnagiCloneAI {
    fn new() -> Self {
        Self {
            rand: XorShift::new(),
            defense_ai: DefenseAI::new(),
        }
    }

    fn initial_params(info: &GameInfo) -> ShipState {
        match info.role {
            Role::Attacker => ShipState::new(134, 64, 10, 1),
            Role::Defender => DefenseAI::initial_params(info),
        }
    }

    fn main(&mut self, info: &GameInfo, state: &GameState) -> Vec<Command> {
        let role_self = info.role;
        if role_self == Role::Defender {
            return self.defense_ai.main(info, state);
        }

        let ship_self = state.get_ships(&role_self)[0];
        let ship_enemy = state.get_ships(&role_self.opponent())[0];

        let commands_enemy = state
            .ship_and_commands
            .iter()
            .filter(|&(ship, _)| ship.role != role_self)
            .map(|(_, commands)| commands)
            .next()
            .unwrap();

        let boost = self.defense_ai.get_boost(ship_self);
        if boost.is_some() {
            return vec![boost.unwrap()];
        } else if ship_self.x5 == 0 {
            // 近距離で (等速直線運動 OR 停止) ならビーム
            let y = if commands_enemy.is_empty() {
                add(
                    &add(&ship_enemy.position, &ship_enemy.velocity),
                    &gravity_of(&ship_enemy.position),
                )
            } else {
                // 停止してる
                ship_enemy.position
            };
            let power = ship_self.x6;
            return vec![Command::Shoot {
                ship_id: ship_self.id,
                target: y,
                power,
            }];
        } else {
            return vec![];
        }
    }
}

const MIN_CHEBYSHEV_DIST: i128 = 17;
const MAX_CHEBYSHEV_DIST: i128 = 100;
const MAX_ABSOLUTE_VELOCITY: i128 = 10;
const NOT_VISITED: i32 = -1;
const VISITING: i32 = -2;
const DEAD_END: i32 = -3;
const MINIMUM_LOOP_ID: i32 = 1_000_000;
type Table = Vec<Vec<Vec<Vec<i32>>>>;

struct DefenseAI {
    table: Table,
    current_loop_id: i32,
    used_loops: HashSet<i32>,
    shipid2loopid: HashMap<i128, i32>,
}

impl DefenseAI {
    fn new() -> DefenseAI {
        let table = vec![
            vec![
                vec![
                    vec![-1; (MAX_ABSOLUTE_VELOCITY * 2 + 1) as usize];
                    (MAX_ABSOLUTE_VELOCITY * 2 + 1) as usize
                ];
                (MAX_CHEBYSHEV_DIST * 2 + 1) as usize
            ];
            (MAX_CHEBYSHEV_DIST * 2 + 1) as usize
        ];
        DefenseAI {
            table,
            current_loop_id: MINIMUM_LOOP_ID,
            used_loops: HashSet::new(),
            shipid2loopid: HashMap::new(),
        }
    }

    fn initial_params(info: &GameInfo) -> ShipState {
        match info.role {
            Role::Attacker => panic!(),
            Role::Defender => ShipState::new(152, 0, 8, 100),
        }
    }

    fn new_velocity(position: &Coord, velocity: &Coord) -> Coord {
        add(velocity, &gravity_of(&position))
    }

    fn new_position(position: &Coord, velocity: &Coord) -> Coord {
        add(position, &DefenseAI::new_velocity(position, velocity))
    }

    fn get_position_index(pos: i128) -> usize {
        (pos + MAX_CHEBYSHEV_DIST) as usize
    }

    fn get_velocity(vec: i128) -> usize {
        (vec + MAX_ABSOLUTE_VELOCITY) as usize
    }

    fn is_out_of_field(position: &Coord) -> bool {
        if position.0 < -MAX_CHEBYSHEV_DIST || position.0 > MAX_CHEBYSHEV_DIST {
            return true;
        }

        if position.1 < -MAX_CHEBYSHEV_DIST || position.1 > MAX_CHEBYSHEV_DIST {
            return true;
        }
        false
    }

    fn is_inside_planet(position: &Coord) -> bool {
        -MIN_CHEBYSHEV_DIST <= position.0
            && position.0 <= MIN_CHEBYSHEV_DIST
            && -MIN_CHEBYSHEV_DIST <= position.1
            && position.1 <= MIN_CHEBYSHEV_DIST
    }

    fn is_invalid_position(position: &Coord) -> bool {
        DefenseAI::is_out_of_field(position) || DefenseAI::is_inside_planet(position)
    }

    fn is_invalid_velocity(velocity: &Coord) -> bool {
        if velocity.0 < -MAX_ABSOLUTE_VELOCITY || velocity.0 > MAX_ABSOLUTE_VELOCITY {
            return true;
        }

        if velocity.1 < -MAX_ABSOLUTE_VELOCITY || velocity.1 > MAX_ABSOLUTE_VELOCITY {
            return true;
        }
        false
    }

    fn lookup_table(&self, position: &Coord, velocity: &Coord) -> i32 {
        if DefenseAI::is_invalid_position(position) {
            return DEAD_END;
        }

        self.table[DefenseAI::get_position_index(position.0)]
            [DefenseAI::get_position_index(position.1)][DefenseAI::get_velocity(velocity.0)]
            [DefenseAI::get_velocity(velocity.1)]
    }

    fn update_table(&mut self, position: &Coord, velocity: &Coord, value: i32) {
        assert!(!DefenseAI::is_out_of_field(position));
        self.table[DefenseAI::get_position_index(position.0)]
            [DefenseAI::get_position_index(position.1)][DefenseAI::get_velocity(velocity.0)]
            [DefenseAI::get_velocity(velocity.1)] = value;
    }

    fn is_loop_id(maybe_loop_id: i32) -> bool {
        maybe_loop_id >= MINIMUM_LOOP_ID
    }

    fn dfs(&mut self, position: &Coord, velocity: &Coord) -> i32 {
        // dbg!(position, velocity);
        if DefenseAI::is_invalid_position(position) || DefenseAI::is_invalid_velocity(velocity) {
            return DEAD_END;
        }

        if self.lookup_table(position, velocity) == VISITING {
            self.current_loop_id += 1;
            return self.current_loop_id;
        }

        let curr_value = self.lookup_table(position, velocity);
        if curr_value != NOT_VISITED {
            return curr_value;
        }

        self.update_table(position, velocity, VISITING);
        let new_position = DefenseAI::new_position(position, velocity);
        let new_velocity = DefenseAI::new_velocity(position, velocity);
        let iter_count = self.dfs(&new_position, &new_velocity);

        if iter_count == self.current_loop_id {
            self.update_table(position, velocity, self.current_loop_id);
            self.current_loop_id
        } else if iter_count == DEAD_END {
            self.update_table(position, velocity, 1);
            1
        } else {
            self.update_table(position, velocity, iter_count + 1);
            iter_count + 1
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
            let maybe_loop_id = self.dfs(&position, &velocity);
            // dbg!("SEARCHING", maybe_loop_id);

            if DefenseAI::is_loop_id(maybe_loop_id)
                && (!self.used_loops.contains(&maybe_loop_id)
                    || self.shipid2loopid.get(&mother_ship.id) == Some(&maybe_loop_id))
            {
                let mut curr_state = curr_state;
                let mut last_vec = (0, 0);
                while let Some(prev_state) = prev_states.get(&curr_state) {
                    // dbg!(&prev_state);
                    let last_accel: (i128, i128) = *prev_actions.get(&curr_state).unwrap();
                    last_vec = (-last_accel.0, -last_accel.1);
                    curr_state = *prev_state;
                }
                // dbg!("GOAL", (&position, &velocity), (&mother_ship.position, mother_ship.velocity));
                self.used_loops.insert(maybe_loop_id);
                self.shipid2loopid.insert(mother_ship.id, maybe_loop_id);

                if last_vec != (0, 0) {
                    return vec![Command::Accelerate {
                        ship_id: mother_ship.id,
                        vector: last_vec,
                    }];
                }

                return vec![];
            }

            for ax in -1..=1 {
                for ay in -1..=1 {
                    if ax == 0 && ay == 0 {
                        continue;
                    }
                    let acceleration = Coord::from((ax, ay));
                    let accelerated_velocity = add(&velocity, &acceleration);
                    let new_velocity = DefenseAI::new_velocity(&position, &accelerated_velocity);
                    let new_position = DefenseAI::new_position(&position, &accelerated_velocity);
                    let new_state = (new_position, new_velocity);
                    if !DefenseAI::is_invalid_position(&new_position)
                        && !states.contains(&new_state)
                    {
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

    fn main(&mut self, info: &GameInfo, state: &GameState) -> Vec<Command> {
        assert_eq!(info.role, Role::Defender);
        let mother_ships: Vec<&Ship> = state
            .ship_and_commands
            .iter()
            .filter(|&(ship, _)| ship.role == Role::Defender)
            .map(|(ship, _)| ship)
            .filter(|ship| ship.x4[3] > 1)
            .collect();

        let mut cmds = vec![];

        for ship in mother_ships {
            let maybe_loop_id = self.dfs(&ship.position, &ship.velocity);
            dbg!(maybe_loop_id);
            if DefenseAI::is_loop_id(maybe_loop_id)
                && self.shipid2loopid.get(&ship.id) == Some(&maybe_loop_id)
            {
                self.shipid2loopid.remove(&ship.id);
                dbg!("CLONE SURUYO", &ship.position);
                let unit = ship.x4[3];
                let is_no_clone = unit < 14;
                cmds.push(Command::Clone {
                    ship_id: ship.id,
                    child: ShipState {
                        fuel: if is_no_clone { 0 } else { ship.x4[0] / 2 },
                        power: ship.x4[1] / 2,
                        capacity: ship.x4[2] / 2,
                        units: if is_no_clone { 1 } else { ship.x4[3] / 2 },
                    },
                })
            } else {
                for cmd in self.search(&ship) {
                    cmds.push(cmd);
                }
            }
        }

        cmds
    }
}
