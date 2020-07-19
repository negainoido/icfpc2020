type ShipId = i128;
type Coord = (i128, i128);

enum GameStage {
    NotStarted = 0,
    Started = 1,
    Finished = 2,
}

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
