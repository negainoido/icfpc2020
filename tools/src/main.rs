use std::env;
use std::io::stdin;
use std::process::Command;

use serde::{Deserialize, Serialize};
use structopt::StructOpt;

use crate::slack::SlackHookRequest;

#[derive(Serialize, Deserialize, StructOpt)]
enum Action {
    Edit,
    Config(Config),
    Password,
    Slack,
}

#[derive(Serialize, Deserialize, Debug, Default, Clone, StructOpt)]
struct Config {
    editor: Option<String>,
    hook_url: Option<String>,
}

impl Config {
    pub fn assign(&self, new_config: &Self) -> Self {
        Config {
            editor: new_config.editor.clone().or(self.editor.clone()),
            hook_url: new_config.hook_url.clone().or(self.hook_url.clone()),
        }
    }
}

mod slack;

const APP_NAME: &str = "wish";

fn main() {
    let args = Action::from_args();
    let config: Config = confy::load(APP_NAME).expect("failed to load");
    match args {
        Action::Edit => {
            let editor = config
                .editor
                .or(env::var("EDITOR").ok())
                .expect("Please set $EDITOR");
            let status = Command::new(editor).status().expect("Something wrong");
            if status.success() {
                println!("success");
            } else {
                println!("Exit {}", status);
            }
        }
        Action::Config(new_config) => {
            let mut new_config = config.assign(&new_config);
            println!("Config: {}", serde_json::to_string(&new_config).unwrap());
            confy::store(APP_NAME, new_config);
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
                );
                hook_url
            } else {
                config.hook_url.unwrap()
            };

            let client = reqwest::blocking::Client::new();
            let json = SlackHookRequest {
                text: "hello".to_owned(),
                ..SlackHookRequest::default()
            };
            println!("{:?}", json);
            let result = client
                .post(&hook_url)
                .json(&json)
                .send()
                .expect("request failed");

            println!("{:?}", result);
        }
    }
}
