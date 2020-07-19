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
            Err(format!("l is not nil: {}", l))
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
            Err(format!("l is not nil: {}", l))
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
            Err(format!("l is not nil: {}", l))
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
        if l.is_nil() {
            Ok(GameResponse {
                stage: FromPrimitive::from_i64(stage.as_int().unwrap() as i64).unwrap(),
                info: GameInfo::try_from(*info)?,
                state: GameState::try_from(*state)?,
            })
        } else {
            Err(format!("l is not nil: {}", l))
        }
    }
}
