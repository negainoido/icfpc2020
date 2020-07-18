use std::env;
use std::io::stdin;
use std::process::Command;

use serde::{Deserialize, Serialize};
use structopt::StructOpt;

use crate::slack::SlackHookRequest;
use icfpc2020::api_client::ApiClient;

#[derive(Serialize, Deserialize, StructOpt)]
enum Action {
    Edit,
    Config(Config),
    Send(SendOpt),
    Password,
    Slack,
}

#[derive(Serialize, Deserialize, Debug, Default, Clone, StructOpt)]
struct Config {
    editor: Option<String>,
    hook_url: Option<String>,
}

#[derive(Serialize, Deserialize, Debug, Default, Clone, StructOpt)]
struct SendOpt {
    body: String,
    #[structopt(long)]
    api_key: Option<String>,
}

impl Config {
    pub fn assign(&self, new_config: &Self) -> Self {
        Config {
            editor: new_config.editor.clone().or_else(|| self.editor.clone()),
            hook_url: new_config
                .hook_url
                .clone()
                .or_else(|| self.hook_url.clone()),
        }
    }
}

mod slack;

const APP_NAME: &str = "wish";
const ICFPC_KEY: &str = "ICFPC2020";

type Error = Box<dyn std::error::Error>;

fn main() -> Result<(), Error> {
    let args = Action::from_args();
    let config: Config = confy::load(APP_NAME).expect("failed to load");
    match args {
        Action::Edit => {
            let editor = config
                .editor
                .or_else(|| env::var("EDITOR").ok())
                .expect("Please set $EDITOR");
            let status = Command::new(editor).status().expect("Something wrong");
            if status.success() {
                println!("success");
            } else {
                println!("Exit {}", status);
            }
        }
        Action::Config(new_config) => {
            let new_config = config.assign(&new_config);
            println!("Config: {}", serde_json::to_string(&new_config).unwrap());
            confy::store(APP_NAME, new_config).expect("Failed to store the config");
        }
        Action::Password => {
            let pass = rpassword::read_password_from_tty(Some("Password: "))
                .expect("Counldn't read your password");
            let user = env::var("USER").expect("Please set $USER");
            let keyring = keyring::Keyring::new(APP_NAME, &user);
            keyring
                .set_password(&pass)
                .expect("Failed to store password");
        }
        Action::Slack => {
            let hook_url = if config.hook_url.is_none() {
                print!("Please put slack hook url: ");
                let mut hook_url = String::new();
                stdin()
                    .read_line(&mut hook_url)
                    .expect("failed to read hook url");
                confy::store(
                    APP_NAME,
                    Config {
                        hook_url: Some(hook_url.clone()),
                        ..config
                    },
                )
                .expect("Failed to store the cofig");
                hook_url
            } else {
                config.hook_url.unwrap()
            };

            let json = SlackHookRequest {
                text: "hello".to_owned(),
                ..SlackHookRequest::default()
            };
            println!("{:?}", json);
            let result = ureq::post(&hook_url).send_json(serde_json::to_value(json).unwrap());

            println!("{:?}", result);
        }
        Action::Send(opt) => {
            let user = env::var("USER")?;
            let keyring = keyring::Keyring::new(ICFPC_KEY, &user);
            let api_key = opt
                .api_key
                .clone()
                .or_else(|| env::var("API_KEY").ok())
                .or_else(|| keyring.get_password().ok())
                .ok_or(failure::err_msg("NO API_KEY provided"))?;
            keyring.set_password(&api_key)?;

            let client = ApiClient::new(&api_key);
            println!("{}", client.send_aliens(&opt.body)?);
        }
    }
    Ok(())
}
