#![allow(unused)]

use std::convert::TryFrom;

use num_derive::FromPrimitive;
use num_traits::FromPrimitive;

use icfpc2020::modulate::List;

type ShipId = i128;

type Coord = (i128, i128);

#[derive(FromPrimitive)]
enum GameStage {
    NotStarted = 0,
    Started = 1,
    Finished = 2,
}

#[derive(FromPrimitive)]
enum Role {
    Attacker = 0,
    Defender = 1,
}

#[derive(Default)]
struct GameInfo {
    x0: Option<()>,
    role: Option<Role>,
    x2: Option<()>,
    x3: Option<()>,
    x4: Option<()>,
}

impl TryFrom<List> for GameInfo {
    type Error = String;

    fn try_from(l: List) -> Result<Self, Self::Error> {
        let (_x0, l) = l.decompose().ok_or(format!("not pair: {}", l))?;
        let (role, l) = l.decompose().ok_or(format!("not pair: {}", l))?;
        let (_x2, l) = l.decompose().ok_or(format!("not pair: {}", l))?;
        let (_x3, l) = l.decompose().ok_or(format!("not pair: {}", l))?;
        let (_x4, l) = l.decompose().ok_or(format!("not pair: {}", l))?;
        if l.is_nil() {
            Ok(GameInfo {
                role: FromPrimitive::from_i64(role.as_int().unwrap() as i64),
                ..Default::default()
            })
        } else {
            Err(format!("GameInfo l is not nil: {}", l))
        }
    }
}

struct Ship {
    role: Role,
    id: ShipId,
    position: Coord,
    velocity: Coord,
}

impl TryFrom<List> for Ship {
    type Error = String;

    fn try_from(l: List) -> Result<Self, Self::Error> {
        let (role, l) = l.decompose().ok_or(format!("not pair: {}", l))?;
        let (shipid, l) = l.decompose().ok_or(format!("not pair: {}", l))?;
        let (position, l) = l.decompose().ok_or(format!("not pair: {}", l))?;
        let (velocity, l) = l.decompose().ok_or(format!("not pair: {}", l))?;
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

enum Command {
    Accelerate(ShipId, Coord),
    Detonate(ShipId),
    Shoot(ShipId, Coord),
}

#[derive(Default)]
struct GameState {
    tick: i128,
    x1: Option<()>,
    ship_and_command: Vec<(Ship, Vec<Command>)>,
}

impl TryFrom<List> for GameState {
    type Error = String;

    fn try_from(l: List) -> Result<Self, Self::Error> {
        let (tick, l) = l.decompose().ok_or(format!("not pair: {}", l))?;
        let (_x1, l) = l.decompose().ok_or(format!("not pair: {}", l))?;
        let (_ship_and_command, l) = l.decompose().ok_or(format!("not pair: {}", l))?;
        if l.is_nil() {
            Ok(GameState {
                tick: tick.as_int().unwrap(),
                ..Default::default()
            })
        } else {
            Err(format!("GameState l is not nil: {}", l))
        }
    }
}

struct GameResponse {
    stage: GameStage,
    info: GameInfo,
    state: GameState,
}

impl TryFrom<List> for GameResponse {
    type Error = String;

    fn try_from(l: List) -> Result<Self, Self::Error> {
        let (stage, l) = l.decompose().ok_or(format!("not pair: {}", l))?;
        let (info, l) = l.decompose().ok_or(format!("not pair: {}", l))?;
        let (state, l) = l.decompose().ok_or(format!("not pair: {}", l))?;
        dbg!(&stage);
        dbg!(&info);
        dbg!(&state);

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

#[cfg(test)]
mod test {
    use super::*;
    use icfpc2020::modulate::*;

    #[test]
    fn join() {
        let join_resp = "110110000111010111101111000010000000011010111101111000100000000011011000011101110010000000011110111000010000110111010000000001111011000011101100001110110000111011000010000110000";
        /*

        (1 (0 ((256 (0 ((512 (1 (64 nil))) ((16 (128 nil)) ((1 (1 (1 (1 nil)))) nil))))) (nil nil))))
         */
        let l = List::demodulate(&join_resp).unwrap();

        GameResponse::try_from(l).unwrap();
    }
}
