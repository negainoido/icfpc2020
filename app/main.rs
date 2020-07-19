use std::env;

use ureq;

mod sexpr2binary;
use sexpr2binary::*;

async fn send(
    server_url: &str,
    request: &str,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    println!("request: {}", request);

    let url = format!(
        "{}/aliens/send?apiKey=9ffa61129e0c45378b01b0817117622c",
        server_url
    );

    let resp = ureq::post(url).sned_string(&request);

    if resp.ok() {
        println!("success: {}", resp.into_string()?);
    } else {
        // This can include errors like failure to parse URL or connect timeout.
        // They are treated as synthetic HTTP-level error statuses.
        println!("error {}: {}", resp.status(), resp.into_string()?);
    }

    Ok(())
}

fn make_join_request(player_key: &str) -> String {
    modulate_sexp(&format!("(cons 2 (cons {} (cons nil nil)))", &player_key)).unwrap()
}

fn make_start_request(player_key: &str) -> String {
    let state = format!("(cons 1 (cons 1 (cons 1 (cons 1 nil))))");
    modulate_sexp(&format!(
        "(cons 3 (cons {} (cons {} nil)))",
        &player_key, state
    ))
    .unwrap()
}

fn make_game_request(player_key: &str) -> String {
    modulate_sexp(&format!("(cons 4 (cons {} (cons nil nil)))", &player_key)).unwrap()
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let args: Vec<String> = env::args().collect();

    let server_url = &args[1];
    let player_key = &args[2];

    println!("ServerUrl: {}; PlayerKey: {}", server_url, player_key);

    // Join
    let request = make_join_request(player_key);
    send(server_url, &request);

    // Start
    let request = make_start_request(player_key);
    send(server_url, &request);

    // Game start
    loop {
        let request = make_game_request(player_key);
        if let Err(_) = send(server_url, &request) {
            break;
        }
    }

    Ok(())
}
