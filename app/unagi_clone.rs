#![allow(dead_code, unused_variables)]

use std::cmp::{max, min};
use std::collections::{HashMap, HashSet, VecDeque};

use crate::ai::*;
use crate::moon::Moon;
use crate::protocol::*;

pub struct UnagiCloneAI {
    rand: XorShift,
    defense_ai: DefenseAI,
}

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
    fn from(x: &Coord) -> Section {
        use Section::*;
        let (x0, x1) = *x;
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
    fn gravity(&self) -> Coord {
        match self {
            Section::Up => (0, 1),
            Section::Right => (-1, 0),
            Section::Left => (1, 0),
            Section::Down => (0, -1),
            Section::DiagRU => (-1, 1),
            Section::DiagRD => (-1, -1),
            Section::DiagLU => (1, 1),
            Section::DiagLD => (1, -1),
            Section::O => (0, 0),
        }
    }
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

fn add(a: &Coord, b: &Coord) -> Coord {
    (a.0 + b.0, a.1 + b.1)
}

const DETONATE_DIST: i128 = 3;
const BEAM_DIST: i128 = 64;

fn dist_max(x: &Coord, y: &Coord) -> i128 {
    std::cmp::max((x.0 - y.0).abs(), (x.1 - y.1).abs())
}

fn dist_manhattan(x: &Coord, y: &Coord) -> i128 {
    (x.0 - y.0).abs() + (x.1 - y.1).abs()
}

fn close_manhattan(a: &Ship, b: &Ship, dist_sup: i128) -> bool {
    let x = add(
        &add(&a.position, &a.velocity),
        &Section::from(&a.position).gravity(),
    );
    let y = add(
        &add(&b.position, &b.velocity),
        &Section::from(&b.position).gravity(),
    );
    dist_manhattan(&x, &y) < dist_sup
}

fn close_max(a: &Ship, b: &Ship, dist_sup: i128) -> bool {
    let x = add(
        &add(&a.position, &a.velocity),
        &Section::from(&a.position).gravity(),
    );
    let y = add(
        &add(&b.position, &b.velocity),
        &Section::from(&b.position).gravity(),
    );
    dist_max(&x, &y) < dist_sup
}

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

    fn main(&mut self, _info: &GameInfo, _state: &GameState) -> Vec<Command> {
        let role_self = _info.role;
        if _info.role == Role::Defender {
            return self.defense_ai.main(_info, _state);
        }

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

        let commands_enemy = _state
            .ship_and_commands
            .iter()
            .filter(|&(ship, _)| ship.role != role_self)
            .map(|(_, commands)| commands)
            .next()
            .unwrap();

        let boost = Moon::get_boost(&ship_self.position, &ship_self.velocity);
        if ship_self.role == Role::Attacker && close_max(&ship_self, &ship_enemy, DETONATE_DIST) {
            {
                let a = ship_self.clone();
                let b = ship_enemy.clone();
                let x = add(
                    &add(&a.position, &a.velocity),
                    &Section::from(&a.position).gravity(),
                );
                let y = add(
                    &add(&b.position, &b.velocity),
                    &Section::from(&b.position).gravity(),
                );
                let d = dist_max(&x, &y);
                eprintln!("\x1b[34m!!! DETONATE min_dist = {} !!!\x1b[0m", d);
            }
            return vec![Command::Detonate {
                ship_id: ship_self.id,
            }];
        } else if boost != (0, 0) {
            return vec![Command::Accelerate {
                ship_id: ship_self.id,
                vector: boost,
            }];
        } else if close_manhattan(&ship_self, &ship_enemy, BEAM_DIST)
            && (commands_enemy.is_empty() || ship_enemy.velocity == (0, 0))
            && ship_self.x5 == 0
            && ship_enemy.x4[0] > 0
        {
            // 近距離で (等速直線運動 OR 停止) ならビーム
            let y = if commands_enemy.is_empty() {
                add(
                    &add(&ship_enemy.position, &ship_enemy.velocity),
                    &Section::from(&ship_enemy.position).gravity(),
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
        } else if ship_self.role == Role::Defender
            && close_max(&ship_self, &ship_enemy, DETONATE_DIST)
        {
            // 自爆回避
            let g = Section::from(&ship_self.position).gravity();
            let boost = (g.0, g.1);
            return vec![Command::Accelerate {
                ship_id: ship_self.id,
                vector: boost,
            }];
        } else if self.rand.gen::<i128>() % 4 == 0 {
            // random noise
            let v = ship_self.velocity;
            let boost = (v.0, v.1);
            return vec![Command::Accelerate {
                ship_id: ship_self.id,
                vector: boost,
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
        }
    }

    fn initial_params(info: &GameInfo) -> ShipState {
        match info.role {
            Role::Attacker => panic!(),
            Role::Defender => ShipState::new(152, 0, 8, 100),
        }
    }

    fn add_coord(c0: &Coord, c1: &Coord) -> Coord {
        Coord::from((c0.0 + c1.0, c0.1 + c1.1))
    }

    fn get_gravity(velocity: &Coord) -> Coord {
        let gx = match velocity {
            (x, y) if *x >= max(*y, -*y) => -1,
            (x, y) if *x <= min(*y, -*y) => 1,
            _ => 0,
        };

        let gy = match velocity {
            (x, y) if *y >= max(*x, -*x) => -1,
            (x, y) if *y <= min(*x, -*x) => 1,
            _ => 0,
        };
        Coord::from((gx, gy))
    }

    fn new_velocity(position: &Coord, velocity: &Coord) -> Coord {
        DefenseAI::add_coord(velocity, &DefenseAI::get_gravity(position))
    }

    fn new_position(position: &Coord, velocity: &Coord) -> Coord {
        DefenseAI::add_coord(position, &DefenseAI::new_velocity(position, velocity))
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

            if DefenseAI::is_loop_id(maybe_loop_id) && !self.used_loops.contains(&maybe_loop_id) {
                let mut curr_state = curr_state;
                let mut last_action = Command::Accelerate {
                    ship_id: mother_ship.id,
                    vector: (0, 0),
                };
                while let Some(prev_state) = prev_states.get(&curr_state) {
                    // dbg!(&prev_state);
                    let last_accel: (i128, i128) = *prev_actions.get(&curr_state).unwrap();
                    last_action = Command::Accelerate {
                        ship_id: 0,
                        vector: (-last_accel.0, -last_accel.1),
                    };
                    curr_state = *prev_state;
                }
                // dbg!("GOAL", (&position, &velocity), (&mother_ship.position, mother_ship.velocity));
                return vec![last_action];
            }

            for ax in -1..=1 {
                for ay in -1..=1 {
                    let acceleration = Coord::from((ax, ay));
                    let accelerated_velocity = DefenseAI::add_coord(&velocity, &acceleration);
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

    fn main(&mut self, _info: &GameInfo, _state: &GameState) -> Vec<Command> {
        assert_eq!(_info.role, Role::Defender);
        let mother_ship = _state
            .ship_and_commands
            .iter()
            .filter(|&(ship, _)| ship.role == Role::Defender)
            .map(|(ship, _)| ship)
            .filter(|ship| ship.x4[3] > 1)
            .next()
            .unwrap();

        let maybe_loop_id = self.dfs(&mother_ship.position, &mother_ship.velocity);
        dbg!(maybe_loop_id);
        if DefenseAI::is_loop_id(maybe_loop_id) && !self.used_loops.contains(&maybe_loop_id) {
            self.used_loops.insert(maybe_loop_id);
            dbg!("CLONE SURUYO", &mother_ship.position);
            vec![Command::Clone {
                ship_id: mother_ship.id,
                child: ShipState {
                    fuel: 0,
                    power: 0,
                    capacity: 0,
                    units: 1,
                },
            }]
        } else {
            self.search(&mother_ship)
        }
    }
}
