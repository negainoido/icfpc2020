#[derive(Debug, Clone)]
pub struct AI();
impl AI {
    pub fn new() -> Self {
        AI()
    }
    pub fn square(self, n: u128) -> u128 {
        n * n
    }
}

#[cfg(test)]
mod tests {
    use super::AI;

    #[test]
    fn it_works() {
        let ai = AI::new();
        assert_eq!(ai.square(2), 4);
    }
}
