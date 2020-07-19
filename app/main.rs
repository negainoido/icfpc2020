mod protocol;

use std::convert::TryFrom;
use std::env;

use ureq;

use icfpc2020::modulate::{cons, List};
use protocol::{GameResponse, GameStage};

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

fn make_join_request(player_key: &i128) -> String {
    use List::*;
    let sexp = cons(Integer(2), cons(Integer(*player_key), cons(Nil, Nil)));
    println!("join request: {}", sexp);
    sexp.modulate()
}

fn make_start_request(player_key: &i128) -> String {
    use List::*;
    let state = List::from(vec![1, 1, 1, 1]);
    let sexp = cons(Integer(3), cons(Integer(*player_key), cons(state, Nil)));
    println!("start request: {}", sexp);
    sexp.modulate()
}

fn make_command_request(player_key: &i128) -> String {
    use List::*;
    let sexp = cons(Integer(4), cons(Integer(*player_key), cons(Nil, Nil)));
    println!("game request: {}", sexp);
    sexp.modulate()
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = env::args().collect();

    let server_url = &args[1];
    let player_key = &args[2].parse::<i128>().expect("failed parsing");

    println!("ServerUrl: {}; PlayerKey: {}", server_url, player_key);

    // Join
    let request = make_join_request(player_key);
    send(server_url, &request)?;

    // Start
    let request = make_start_request(player_key);
    send(server_url, &request)?;

    // Game start
    loop {
        let request = make_command_request(player_key);
        let resp = send(server_url, &request);
        if let Err(e) = send(server_url, &request) {
            println!("error: {}", e);
            break;
        }
        let game_response: GameResponse = GameResponse::try_from(resp.unwrap()).unwrap();
        println!("game_response: {:?}", game_response);
        if game_response.stage == GameStage::Finished {
            break;
        }
    }

    Ok(())
}
