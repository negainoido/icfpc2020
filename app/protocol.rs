use std::convert::TryFrom;

use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

use icfpc2020::modulate::List;

pub type ShipId = i128;

pub type Coord = (i128, i128);

#[derive(FromPrimitive, Debug, Clone, Copy, PartialEq, Eq)]
pub enum GameStage {
    NotStarted = 0,
    Started = 1,
    Finished = 2,
}

#[derive(FromPrimitive, Debug, Clone, Copy, PartialEq, Eq)]
pub enum Role {
    Attacker = 0,
    Defender = 1,
}

#[derive(Debug)]
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Ship {
    pub role: Role,
    pub id: ShipId,
    pub position: Coord,
    pub velocity: Coord,
}

impl TryFrom<List> for Ship {
    type Error = String;

    fn try_from(l: List) -> Result<Self, Self::Error> {
        let (role, l) = l.decompose().expect(&format!("not pair: {}", l));
        let (shipid, l) = l.decompose().expect(&format!("not pair: {}", l));
        let (position, l) = l.decompose().expect(&format!("not pair: {}", l));
        let (velocity, l) = l.decompose().expect(&format!("not pair: {}", l));
        let (_x4, l) = l.decompose().expect(&format!("not pair: {}", l));
        let (_x5, l) = l.decompose().expect(&format!("not pair: {}", l));
        let (_x6, l) = l.decompose().expect(&format!("not pair: {}", l));
        let (_x7, l) = l.decompose().expect(&format!("not pair: {}", l));
        if l.is_nil() {
            Ok(Ship {
                role: FromPrimitive::from_i64(role.as_int().unwrap() as i64).unwrap(),
                id: shipid.as_int().unwrap(),
                position: position.as_coord().unwrap(),
                velocity: velocity.as_coord().unwrap(),
            })
        } else {
            Err(format!("Ship l is not nil: {}", l))
        }
    }
}

#[derive(Debug)]
pub enum Command {
    Accelerate { ship_id: ShipId, vector: Coord },
    Detonate { ship_id: ShipId },
    Shoot { ship_id: ShipId, target: Coord },
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
            } => cons(
                Integer(2),
                cons(
                    Integer(ship_id),
                    cons(cons(Integer(x), Integer(y)), cons(Integer(4), Nil)),
                ),
            ),
        }
    }
}

#[derive(Default, Debug)]
pub struct GameState {
    pub tick: i128,
    pub x1: Vec<i128>,
    pub ship_and_commands: Vec<(Ship, Vec<Command>)>,
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

            let (ship, _command) = cur.decompose().unwrap();
            // TODO: parse _command;
            ship_commands.push((Ship::try_from(*ship).unwrap(), vec![]));

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

#[derive(Debug)]
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
            },
            attacker,
        );
    }
}
