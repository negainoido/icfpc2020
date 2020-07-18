use failure::err_msg;
use lazy_static::lazy_static;
use ureq::Response;
use url::Url;

const DOMAIN: &str = "https://icfpc2020-api.testkontur.ru";
lazy_static! {
    static ref BASE_URL: Url = Url::parse(DOMAIN).unwrap();
}

pub struct ApiClient {
    key: String,
}

type Error = Box<dyn std::error::Error>;

impl ApiClient {
    pub fn new(key: &str) -> Self {
        ApiClient {
            key: key.to_owned(),
        }
    }

    #[allow(dead_code)]
    fn get(&self, path: &str) -> Response {
        ureq::get(BASE_URL.join(path).expect("Invalid path").as_str())
            .query("apiKey", &self.key)
            .call()
    }

    fn post_with_str(&self, path: &str, body: &str) -> Result<Response, Error> {
        Ok(ureq::post(BASE_URL.join(path)?.as_str())
            .query("apiKey", &self.key)
            .send_string(body))
    }

    pub fn send_aliens(&self, body: &str) -> Result<String, Error> {
        let res = self.post_with_str("/aliens/send", body)?;

        if res.ok() {
            res.into_string().map_err(|err| err.into())
        } else {
            Err(err_msg(format!("request failure: {}", res.status())).into())
        }
    }
}
