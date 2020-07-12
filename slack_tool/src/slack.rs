use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Default, Debug)]
pub struct SlackHookRequest {
    pub text: String,
    pub channel: Option<String>,
    pub username: Option<String>,
    pub icon_url: Option<String>,
    pub icon_emoji: Option<String>,
}
