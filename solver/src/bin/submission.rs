use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();

    let server_url = &args[1];
    let player_key = &args[2];

    println!("ServerUrl: {}; PlayerKey: {}", server_url, player_key);

    let response: ureq::Response =
        ureq::get(&format!("{}?player_key={}", server_url, player_key)).call();

    let text = response.into_string().unwrap();
    println!("text {}", text)
}
