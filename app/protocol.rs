use std::convert::TryFrom;

use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

use icfpc2020::modulate::List;

pub type ShipId = i128;

pub type Coord = (i128, i128);

#[derive(FromPrimitive, Debug, Clone, Copy, PartialEq, Eq, serde::Serialize)]
pub enum GameStage {
    NotStarted = 0,
    Started = 1,
    Finished = 2,
}

#[derive(FromPrimitive, Debug, Clone, Copy, PartialEq, Eq, serde::Serialize)]
pub enum Role {
    Attacker = 0,
    Defender = 1,
}

impl Role {
    pub fn opponent(&self) -> Role {
        match self {
            Role::Attacker => Role::Defender,
            Role::Defender => Role::Attacker,
        }
    }
}

#[derive(Debug, serde::Serialize)]
pub struct GameInfo {
    pub x0: i128,
    pub role: Role,
    pub x2: Vec<i128>,
    pub x3: Vec<i128>,
    pub x4: Vec<i128>,
}

impl TryFrom<List> for GameInfo {
    type Error = String;

    fn try_from(l: List) -> Result<Self, Self::Error> {
        let (x0, l) = l.decompose().expect(&format!("not pair: {}", l));
        let (role, l) = l.decompose().expect(&format!("not pair: {}", l));
        let (x2, l) = l.decompose().expect(&format!("not pair: {}", l));
        let (x3, l) = l.decompose().expect(&format!("not pair: {}", l));
        let (x4, l) = l.decompose().expect(&format!("not pair: {}", l));

        if l.is_nil() {
            Ok(GameInfo {
                x0: x0.as_int().unwrap(),
                role: FromPrimitive::from_i64(role.as_int().unwrap() as i64).unwrap(),
                x2: x2.as_vec().unwrap(),
                x3: x3.as_vec().unwrap(),
                x4: x4.as_vec().unwrap(),
            })
        } else {
            Err(format!("GameInfo l is not nil: {}", l))
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize)]
pub struct Ship {
    pub role: Role,
    pub id: ShipId,
    pub position: Coord,
    pub velocity: Coord,
    pub x4: Vec<i128>,
    pub x5: i128,
    pub x6: i128,
    pub x7: i128,
}

impl TryFrom<List> for Ship {
    type Error = String;

    fn try_from(l: List) -> Result<Self, Self::Error> {
        let (role, l) = l.decompose().expect(&format!("not pair: {}", l));
        let (shipid, l) = l.decompose().expect(&format!("not pair: {}", l));
        let (position, l) = l.decompose().expect(&format!("not pair: {}", l));
        let (velocity, l) = l.decompose().expect(&format!("not pair: {}", l));
        let (x4, l) = l.decompose().expect(&format!("not pair: {}", l));
        let (x5, l) = l.decompose().expect(&format!("not pair: {}", l));
        let (x6, l) = l.decompose().expect(&format!("not pair: {}", l));
        let (x7, l) = l.decompose().expect(&format!("not pair: {}", l));
        if l.is_nil() {
            Ok(Ship {
                role: FromPrimitive::from_i64(role.as_int().unwrap() as i64).unwrap(),
                id: shipid.as_int().unwrap(),
                position: position.as_coord().unwrap(),
                velocity: velocity.as_coord().unwrap(),
                x4: x4.as_vec().unwrap(),
                x5: x5.as_int().unwrap(),
                x6: x6.as_int().unwrap(),
                x7: x7.as_int().unwrap(),
            })
        } else {
            Err(format!("Ship l is not nil: {}", l))
        }
    }
}

#[derive(Debug, serde::Serialize, Clone)]
pub struct ShipState {
    pub fuel: i128,     // 残り燃料
    pub power: i128,    // レーザーの出力
    pub capacity: i128, // 機体がnopだったときに下がる温度
    pub units: i128,    // 分裂可能な数(2以上でクローンが実行可能)
}

impl ShipState {
    pub fn new(fuel: i128, power: i128, capacity: i128, units: i128) -> Self {
        ShipState {
            fuel,
            power,
            capacity,
            units,
        }
    }
}

#[derive(Debug, serde::Serialize, Clone)]
pub enum Command {
    Accelerate {
        ship_id: ShipId,
        vector: Coord,
    },
    Detonate {
        ship_id: ShipId,
    },
    Shoot {
        ship_id: ShipId,
        target: Coord,
        power: i128,
    },
    Clone {
        ship_id: ShipId,
        child: ShipState,
    },
}

impl From<ShipState> for List {
    fn from(state: ShipState) -> List {
        use icfpc2020::modulate::cons;
        use List::*;
        cons(
            Integer(state.fuel),
            cons(
                Integer(state.power),
                cons(Integer(state.capacity), cons(Integer(state.units), Nil)),
            ),
        )
    }
}

impl From<Command> for List {
    fn from(com: Command) -> List {
        use icfpc2020::modulate::cons;
        use Command::*;
        use List::*;
        match com {
            Accelerate {
                ship_id,
                vector: (x, y),
            } => cons(
                Integer(0),
                cons(Integer(ship_id), cons(cons(Integer(x), Integer(y)), Nil)),
            ),
            Detonate { ship_id } => cons(Integer(1), cons(Integer(ship_id), Nil)),
            Shoot {
                ship_id,
                target: (x, y),
                power,
            } => cons(
                Integer(2),
                cons(
                    Integer(ship_id),
                    cons(cons(Integer(x), Integer(y)), cons(Integer(power), Nil)),
                ),
            ),
            Clone { ship_id, child } => {
                cons(Integer(3), cons(Integer(ship_id), cons(child.into(), Nil)))
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize)]
pub enum AppliedCommand {
    Accelerated {
        vector: Coord,
    },
    Detonated {
        x1: i128,
        x2: i128,
    },
    Shot {
        target: Coord,
        power: i128,
        x4: i128,
        x5: i128,
    },
    Unknown(List),
}

impl From<List> for AppliedCommand {
    fn from(cmd: List) -> Self {
        use AppliedCommand::*;
        println!("AppliedCommand: {}", cmd);

        let (typ, l) = cmd
            .decompose()
            .expect(&format!("failed getting command type: {}", cmd));

        match typ.as_int().unwrap() {
            0 => {
                let (vector, l) = l.decompose().unwrap();
                let vector = vector.as_coord().unwrap();
                if !l.is_nil() {
                    panic!("unexpected value for applied Accelerated command: {}", l);
                }
                Accelerated { vector }
            }
            1 => {
                let (x1, l) = l.decompose().unwrap();
                let (x2, l) = l.decompose().unwrap();
                if !l.is_nil() {
                    panic!("unexpected value for applied Detonated command: {}", l);
                }
                Detonated {
                    x1: x1.as_int().unwrap(),
                    x2: x2.as_int().unwrap(),
                }
            }
            2 => {
                let (target, l) = l.decompose().unwrap();
                let (power, l) = l.decompose().unwrap();
                let (x4, l) = l.decompose().unwrap();
                let (x5, l) = l.decompose().unwrap();
                if !l.is_nil() {
                    panic!("unexpected value for applied Shot command: {}", l);
                }
                Shot {
                    target: target.as_coord().unwrap(),
                    power: power.as_int().unwrap(),
                    x4: x4.as_int().unwrap(),
                    x5: x5.as_int().unwrap(),
                }
            }
            typ => {
                println!("Unknown applied command: {}", typ);
                Unknown(cmd)
            }
        }
    }
}

#[derive(Default, Debug, serde::Serialize, Clone)]
pub struct GameState {
    pub tick: i128,
    pub x1: Vec<i128>,
    pub ship_and_commands: Vec<(Ship, Vec<AppliedCommand>)>,
}

impl TryFrom<List> for GameState {
    type Error = String;

    fn try_from(l: List) -> Result<Self, Self::Error> {
        if l.is_nil() {
            return Ok(Default::default());
        }
        let (tick, l) = l.decompose().expect(&format!("not pair: {}", l));
        let (x1, l) = l.decompose().expect(&format!("not pair: {}", l));
        let (ship_and_commands, l) = l.decompose().expect(&format!("not pair: {}", l));

        let mut cur_ship_and_commands = ship_and_commands;
        let mut ship_commands = vec![];
        loop {
            let (cur, cdr) = cur_ship_and_commands.decompose().unwrap();

            let (ship, cur) = cur.decompose().unwrap();
            let (commands, _cur) = cur.decompose().unwrap();

            let mut cmd_vec: Vec<AppliedCommand> = Default::default();
            let mut cur_cmds = commands;
            loop {
                if cur_cmds.is_nil() {
                    break;
                }
                let (cmd, cmds) = cur_cmds.decompose().unwrap();
                if cmd.is_nil() {
                    break;
                }
                cmd_vec.push(AppliedCommand::from(*cmd));
                cur_cmds = cmds;
            }
            ship_commands.push((Ship::try_from(*ship).unwrap(), cmd_vec));

            if cdr.is_nil() {
                break;
            }
            cur_ship_and_commands = cdr;
        }

        if l.is_nil() {
            Ok(GameState {
                tick: tick.as_int().unwrap(),
                x1: x1.as_vec().unwrap(),
                ship_and_commands: ship_commands,
            })
        } else {
            Err(format!("GameState l is not nil: {}", l))
        }
    }
}

impl GameState {
    pub fn get_ships(&self, role: &Role) -> Vec<&Ship> {
        self.ship_and_commands
            .iter()
            .map(|(s, _)| s)
            .filter(|s| s.role == *role)
            .collect()
    }
}

#[derive(Debug, serde::Serialize)]
pub struct GameResponse {
    pub stage: GameStage,
    pub info: GameInfo,
    pub state: GameState,
}

impl TryFrom<List> for GameResponse {
    type Error = String;

    fn try_from(l: List) -> Result<Self, Self::Error> {
        let (status, l) = l.decompose().expect(&format!("not pair: {}", l));

        if let Some(1) = status.as_int() {
        } else {
            return Err(format!("GameResponse status is not 1: {}", status));
        }

        let (stage, l) = l.decompose().expect(&format!("not pair: {}", l));
        let (info, l) = l.decompose().expect(&format!("not pair: {}", l));
        let (state, l) = l.decompose().expect(&format!("not pair: {}", l));

        if l.is_nil() {
            Ok(GameResponse {
                stage: FromPrimitive::from_i64(stage.as_int().unwrap() as i64).unwrap(),
                info: GameInfo::try_from(*info)?,
                state: GameState::try_from(*state)?,
            })
        } else {
            Err(format!("GameResponse l is not nil: {}", l))
        }
    }
}

impl GameResponse {
    pub fn is_finished(&self) -> bool {
        self.stage == GameStage::Finished
    }
}

#[cfg(test)]
mod test {
    use super::*;
    // use icfpc2020::modulate::*;

    #[test]
    fn join() {
        // https://icfpc2020-api.testkontur.ru/logs/?logKey=6QcVllpAF%2BrDE%2BLh1mU37FG5ZyIlbx93Si%2BG%2BIls370%3D&apiKey=9ffa61129e0c45378b01b0817117622c
        let join_resp = "110110000111010111101111000010000000011010111101111000100000000011011000011101110010000000011110111000010000110111010000000001111011000011101100001110110000111011000010000110000";
        /*

        (1 (0 ((256 (0 ((512 (1 (64 nil))) ((16 (128 nil)) ((1 (1 (1 (1 nil)))) nil))))) (nil nil))))
         */
        let l = List::demodulate(&join_resp).unwrap();

        let game_resp = GameResponse::try_from(l).unwrap();
        dbg!(&game_resp.stage);
        dbg!(&game_resp.info);
        assert_eq!(game_resp.stage, GameStage::NotStarted);
        let game_info = game_resp.info;
        assert_eq!(game_info.role, Role::Attacker);
        assert_eq!(game_info.x4.len(), 4);
    }

    #[test]
    fn start() {
        // https://icfpc2020-api.testkontur.ru/logs?logKey=BvVVF1rAhqmTR9a0eTKCBMhNL4AZg7cor7gztcr%2B18k%3D&apiKey=9ffa61129e0c45378b01b0817117622c
        let start_resp = "1101100001110110000111110111100001000000001101100001111101111000011100000011011000011101110010000000011110111000010000110111010000000001100001111010111101110000100001101110100000000011111111011000011101011111011000010111101100011000011110100101111011000011101100001110110000111011000010011010110111001000000110110000100110000111111010110110000111110111000010111011100011000011110100101111011000011101100001110110000111011000010011010110111001000000110110000100110000000000";
        /*

        (1 (1 ((256 (1 ((448 (1 (64 nil))) ((16 (128 nil)) (nil nil))))) ((0 ((16 (128 nil)) ((((1 (0 ((-23 -48) ((0 0) ((1 (1 (1 (1 nil)))) (0 (64 (1 nil)))))))) (nil nil)) (((0 (1 ((23 48) ((0 0) ((1 (1 (1 (1 nil)))) (0 (64 (1 nil)))))))) (nil nil)) nil)) nil))) nil))))
         */
        let l = List::demodulate(&start_resp).unwrap();

        let game_resp = GameResponse::try_from(l).unwrap();
        assert_eq!(game_resp.stage, GameStage::Started);
        dbg!(&game_resp);
        let game_info = game_resp.info;
        assert_eq!(game_info.role, Role::Defender);
        let game_state = game_resp.state;
        assert_eq!(game_state.tick, 0);

        let defender = &game_state.ship_and_commands[0].0;
        assert_eq!(
            &Ship {
                role: Role::Defender,
                id: 0,
                position: (-23, -48),
                velocity: (0, 0),
                x4: vec![1, 1, 1, 1],
                x5: 0,
                x6: 64,
                x7: 1,
            },
            defender
        );

        let attacker = &game_state.ship_and_commands[1].0;

        assert_eq!(
            &Ship {
                role: Role::Attacker,
                id: 1,
                position: (23, 48),
                velocity: (0, 0),
                x4: vec![1, 1, 1, 1],
                x5: 0,
                x6: 64,
                x7: 1,
            },
            attacker,
        );
    }

    #[test]
    fn shot_and_accelarated() {
        // The last line of https://icfpc2020-api.testkontur.ru/logs?logKey=4KQ8iZkTIOaBSETrwSnX0OT7sVAGzyIOjrF5kQIT1xQ%3D&apiKey=9ffa61129e0c45378b01b0817117622c
        let resp = "1101100001110110001011110111100001000000001101100001111101111000011100000011011000011101110010000000011110111000010000110111010000000001100001111011100010000011110111000010000110111010000000001111111101100001110101111101000110111000101111111101100011101001001111010110101101011010001101110010000001101110010000001101100001001100001111110101101100001111101110010101001011000100110111101100011011001111111011100111011011011100100000011011010101101100001001101110011111001101110100000001101100010001111110110001011111010001101110001011111101110010000001101110011000101101100100001111010111101010100001000000000000";
        let l = List::demodulate(&resp).unwrap();
        let game_resp = GameResponse::try_from(l).unwrap();
        eprintln!("resp: {:?}", game_resp);
        let state = game_resp.state;
        let (ship1, commands1) = &state.ship_and_commands[0];
        assert_eq!(
            *ship1,
            Ship {
                role: Role::Defender,
                id: 0,
                position: (-3, 47),
                velocity: (3, -4),
                x4: vec![0, 0, 0, 0],
                x5: 64,
                x6: 64,
                x7: 1
            },
        );
        assert_eq!(*commands1, vec![]);

        let (ship2, commands2) = &state.ship_and_commands[1];
        assert_eq!(
            *ship2,
            Ship {
                role: Role::Attacker,
                id: 1,
                position: (84, -38),
                velocity: (3, 7),
                x4: vec![118, 64, 10, 1],
                x5: 124,
                x6: 128,
                x7: 2
            }
        );

        assert_eq!(
            *commands2,
            vec![
                AppliedCommand::Shot {
                    target: (-3, 47),
                    power: 64,
                    x4: 98,
                    x5: 4
                },
                AppliedCommand::Accelerated { vector: (0, -1) }
            ]
        );
    }

    #[test]
    fn unknown_applied_command() {
        // https://icfpc2020-api.testkontur.ru/logs?logKey=5lyYxfZwONT37pqrSBiSVg1WqtjuZowdn8joV8A1YPo%3D&apiKey=9ffa61129e0c45378b01b0817117622c
        let resp = "1101100001110110000111110111100001000000001101011110111100010000000001101100001110111001000000001111011100001000011011101000000000111101110100110001101011011010001101110011001000000111101100100111101110000100001101110100000000011111111011000011101011111011000011101011100011010111110110011101100101111101110100100101101011011010001101110011000110011011100001000011011101000000011011000100011111101100011111101011010110101101100001000000001111110101101100001111101110001100001011000100100111101001011110111010000010110111001000000110110101011011000010011010110111001000000110110000100111111010111110100001010000000111111011000011101100010111110110000111010111000110101111101100111011001011111010110101101011011000010011010110111010000000110110001000110000000000";
        let l = List::demodulate(&resp).unwrap();
        let game_resp = GameResponse::try_from(l).unwrap();
        eprintln!("resp: {:?}", game_resp);
    }

    #[test]
    fn detonated() {
        // https://icfpcontest2020.github.io/#/visualize?game=06f74ed2-6d3e-4cce-b235-063d37eee065
        // https://icfpc2020-api.testkontur.ru/logs?logKey=%2FyCYUNif1%2BjYAD73yrbDyLPjllShNz8eav70EWk%2F%2Bew%3D&apiKey=9ffa61129e0c45378b01b0817117622c
        let resp = "11011000011101100010111101111000010000000011011000011111011110000111000000110110000111011100100000000111101110000100001101110100000000011000011110111000010110111101110000100001101110100000000011111111011000011101011110111000011000011100011011011110110010101011110101101011010110100011011100001100011011100100000011011000010011000011111101011011000011111011100001011101110001100001111010010111101011010110101101000110111001000100110111010000000110110001000111111010111110100001101000010011110110000111011110000101101101110111000100000000000000000";
        let l = List::demodulate(&resp).unwrap();
        let game_resp = GameResponse::try_from(l).unwrap();
        eprintln!("resp: {:?}", game_resp);
        let state = game_resp.state;

        let (ship1, commands1) = &state.ship_and_commands[0];
        assert_eq!(
            *ship1,
            Ship {
                role: Role::Defender,
                id: 0,
                position: (24, 54),
                velocity: (5, 0),
                x4: vec![0, 0, 0, 0],
                x5: 24,
                x6: 64,
                x7: 1
            }
        );
        assert_eq!(*commands1, vec![]);

        let (ship2, commands2) = &state.ship_and_commands[1];
        assert_eq!(
            *ship2,
            Ship {
                role: Role::Attacker,
                id: 1,
                position: (23, 48),
                velocity: (0, 0),
                x4: vec![0, 0, 0, 0],
                x5: 68,
                x6: 128,
                x7: 2
            }
        );

        assert_eq!(
            *commands2,
            vec![
                AppliedCommand::Accelerated { vector: (-1, -1) },
                AppliedCommand::Detonated { x1: 365, x2: 32 }
            ]
        );
    }
}
