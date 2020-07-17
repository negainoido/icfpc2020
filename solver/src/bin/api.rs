fn main() -> std::io::Result<()> {
    let request = "1101000";

    let resp = ureq::post(
        "https://icfpc2020-api.testkontur.ru/aliens/send?apiKey=9ffa61129e0c45378b01b0817117622c",
    )
    .send_bytes(&request.as_bytes());

    // .ok() tells if response is 200-299.
    if resp.ok() {
        println!("success: {}", resp.into_string()?);
    } else {
        // This can include errors like failure to parse URL or connect timeout.
        // They are treated as synthetic HTTP-level error statuses.
        println!("error {}: {}", resp.status(), resp.into_string()?);
    }
    Ok(())
}
