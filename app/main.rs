#![allow(unused_imports)]
mod ai;
mod cympfh;
mod full_moon;
mod min_mutlti_ai;
mod moon;
mod new_moon;
mod protocol;
mod unagi_clone;
mod utility;

use std::convert::TryFrom;
use std::env;

use ureq;

use icfpc2020::modulate::{cons, List};
use protocol::{Command, GameResponse, Role};

use crate::ai::AI;
use crate::cympfh::CympfhAI;
use crate::protocol::ShipState;

/*****************************
 * Change this type to your AIs
 */
type AttackerAI = cympfh::CympfhAI;
type DefenderAI = unagi_clone::UnagiCloneAI;

fn send(server_url: &str, request: &str) -> Result<List, Box<dyn std::error::Error>> {
    println!("request: {}", request);

    let url = format!(
        "{}/aliens/send?apiKey=9ffa61129e0c45378b01b0817117622c",
        server_url
    );

    let resp = ureq::post(&url).send_string(&request);
    if resp.ok() {
        println!("success");
    } else {
        // This can include errors like failure to parse URL or connect timeout.
        // They are treated as synthetic HTTP-level error statuses.
        println!("error {}", resp.status());
    }

    let resp = resp.into_string()?;
    println!("binary response: {}", resp);
    let list = List::demodulate(&resp).unwrap();
    println!("response: {}", list);
    Ok(list)
}

fn make_join_request(player_key: &i128, vec: Vec<i128>) -> String {
    use List::*;
    let sexp = cons(
        Integer(2),
        cons(Integer(*player_key), cons(vec.into(), Nil)),
    );
    println!("join request: {}", sexp);
    sexp.modulate()
}

fn make_start_request(player_key: &i128, state: ShipState) -> String {
    use List::*;
    let state = state.into();
    let sexp = cons(Integer(3), cons(Integer(*player_key), cons(state, Nil)));
    println!("start request: {}", sexp);
    sexp.modulate()
}

fn make_command_request(player_key: &i128, commands: Vec<Command>) -> String {
    use List::*;
    let mut coms = Nil;
    for com in commands {
        coms = cons(List::from(com), coms);
    }
    let sexp = cons(Integer(4), cons(Integer(*player_key), cons(coms, Nil)));
    println!("game request: {}", sexp);
    sexp.modulate()
}

fn make_create_request() -> String {
    let create = List::from(vec![1, 0]);
    println!("game request: {}", create);
    create.modulate()
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();

    let server_url = &args[1];

    // Create
    // If player_key is not given, send a create command.
    if args.len() == 2 {
        // https://message-from-space.readthedocs.io/en/latest/game.html#create
        let request = make_create_request();

        // (1, ((0, attackPlayerKey), (1, defenderPlayerKey)))
        let resp = send(server_url, &request)?;

        let resp = resp.cdr().unwrap();
        let resp = resp.car().unwrap();
        let (car, cdr) = resp.decompose().unwrap();
        let car = car.cdr().unwrap().car().unwrap();
        let cdr = cdr.car().unwrap();
        let cdr = cdr.cdr().unwrap().car().unwrap();
        println!("{} {}", car, cdr);

        return Ok(());
    }

    let player_key = &args[2].parse::<i128>().expect("failed parsing");

    println!("ServerUrl: {}; PlayerKey: {}", server_url, player_key);

    let join_params: Vec<i128> = if args.len() > 3 {
        args[3]
            .split(",")
            .map(|s| s.parse::<i128>().expect("failed parsing"))
            .collect()
    } else {
        vec![]
    };

    println!("Join params: {:?}", join_params);

    // Join
    let request = make_join_request(player_key, join_params);
    let resp = send(server_url, &request)?;
    let game_response: GameResponse = GameResponse::try_from(resp).unwrap();
    if game_response.is_finished() {
        println!("Game is finished before Joining");
        return Ok(());
    }
    let mut info = game_response.info;
    println!("info: {:?}", info);

    let (mut ai, init_state): (Box<dyn AI>, ShipState) = match info.role {
        Role::Attacker => (
            Box::new(AttackerAI::new()),
            AttackerAI::initial_params(&info),
        ),
        Role::Defender => (
            Box::new(DefenderAI::new()),
            DefenderAI::initial_params(&info),
        ),
    };

    // Start
    let request = make_start_request(player_key, init_state);
    let resp = send(server_url, &request)?;

    let game_response: GameResponse = GameResponse::try_from(resp)?;
    if game_response.is_finished() {
        println!("Game is immediately finished!!");
        return Ok(());
    }

    let mut state = game_response.state;

    let mut turn = 0;
    // Game start
    loop {
        println!("Turn {}", turn);
        turn += 1;

        println!("state: {}", serde_json::to_string(&state).unwrap());
        let commands = ai.main(&info, &state);
        println!("command: {}", serde_json::to_string(&commands).unwrap());
        let request = make_command_request(player_key, commands);
        let resp = send(server_url, &request);
        if let Err(e) = resp {
            println!("error: {}", e);
            break;
        }

        let game_response: GameResponse = GameResponse::try_from(resp?)?;
        println!(
            "game_response: {}",
            serde_json::to_string(&game_response).unwrap()
        );
        if game_response.is_finished() {
            println!("Game is successfully finished!!");
            break;
        }
        info = game_response.info;
        state = game_response.state;
    }

    Ok(())
}
