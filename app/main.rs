use http_body::Body as _;
use hyper::{Body, Client, Method, Request, StatusCode};
use std::env;
use std::process;

mod sexpr2binary;
use sexpr2binary::*;

async fn send(
    server_url: &str,
    request: &str,
) -> Result<(), Box<dyn std::error::Error + Send + Sync>> {
    let client = Client::new();

    let req = Request::builder()
        .method(Method::POST)
        .uri(&format!(
            "{}/aliens/send?apiKey=9ffa61129e0c45378b01b0817117622c",
            server_url
        ))
        .body(Body::from(request.to_string()))?;

    match client.request(req).await {
        Ok(mut res) => match res.status() {
            StatusCode::OK => {
                print!("Server response: ");
                while let Some(chunk) = res.body_mut().data().await {
                    match chunk {
                        Ok(content) => println!("{:?}", content),
                        Err(why) => println!("error reading body: {:?}", why),
                    }
                }
            }
            _ => {
                println!("Unexpected server response:");
                println!("HTTP code: {}", res.status());
                print!("Response body: ");
                while let Some(chunk) = res.body_mut().data().await {
                    match chunk {
                        Ok(content) => println!("{:?}", content),
                        Err(why) => println!("error reading body: {:?}", why),
                    }
                }
                process::exit(2);
            }
        },
        Err(err) => {
            println!("Unexpected server response:\n{}", err);
            process::exit(1);
        }
    }

    Ok(())
}

fn make_join_request(player_key: &str) -> String {
    modulate_sexp(&format!("(cons 2 (cons {} (cons nil nil)))", &player_key)).unwrap()
}

fn make_start_request(player_key: &str) -> String {
    let state = format!("(cons 0 (cons 0 (cons 0 (cons 0 nil))))");
    modulate_sexp(&format!(
        "(cons 2 (cons {} (cons {} nil)))",
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
    send(server_url, &request).await?;

    // Start
    let request = make_start_request(player_key);
    send(server_url, &request).await?;

    // Game start
    loop {
        let request = make_game_request(player_key);
        if let Err(_) = send(server_url, &request).await {
            break;
        }
    }

    Ok(())
}
