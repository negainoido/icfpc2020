use crate::ai::*;

pub struct NopAI;

impl AI for NopAI {
    fn new() -> Self {
        NopAI {}
    }
}
