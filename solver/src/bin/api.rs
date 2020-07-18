use icfpc2020::eval::Evaluator;
use icfpc2020::modulate::*;

fn send(request: &str) -> String {
    let resp = ureq::post(
        "https://icfpc2020-api.testkontur.ru/aliens/send?apiKey=9ffa61129e0c45378b01b0817117622c",
    )
    .send_bytes(&request.as_bytes());

    if resp.ok() {
        let res = resp.into_string().unwrap();
        return res;
    } else {
        panic!(
            "error {} while using the 'send' API: {}",
            resp.status(),
            resp.into_string().unwrap()
        );
    }
}

fn main() {
    let request = "1101000";

    let res = send(request);
    let eval = Evaluator::new();
    let a = demodulate(&res, &eval);
    println!("{:?}", a);
}
