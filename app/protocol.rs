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

struct GameInfo {
    x0: (),
    role: Role,
    x2: (),
    x3: (),
    x4: (),
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

struct GameState {
    tick: i128,
    x1: (),
    ship_and_command: Vec<(Ship, Vec<Command>)>,
}

struct GameResponse {
    stage: GameStage,
    info: GameInfo,
    state: GameState,
}
