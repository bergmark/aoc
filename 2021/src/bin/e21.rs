fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a(4, 8), 739785);
    assert_eq!(a(9, 3), 1073709);
}

struct Dice {
    n: usize,
    rolls: usize,
}

impl Dice {
    fn new() -> Dice {
        Dice { n: 1, rolls: 0 }
    }

    fn roll(&mut self) -> usize {
        self.rolls += 1;
        let ret = self.n;
        self.n += 1;
        if self.n == 101 {
            self.n = 1;
        }
        ret
    }
}

#[derive(Debug, Copy, Clone)]
struct Player {
    pos: usize,
    score: usize,
}

impl Player {
    fn new(pos: usize) -> Player {
        Player { pos, score: 0 }
    }
}

fn has_won(players: &[Player]) -> bool {
    players.iter().any(|p| p.score >= 1000)
}

fn a(a: usize, b: usize) -> usize {
    let mut dice = Dice::new();
    let mut players = vec![Player::new(a), Player::new(b)];
    let mut current = 0;

    while !has_won(&players) {
        let player = &mut players[current];
        let a = dice.roll();
        let b = dice.roll();
        let c = dice.roll();
        player.pos += a + b + c;
        while player.pos > 10 {
            player.pos -= 10;
        }
        player.score += player.pos;
        current = (current + 1) % 2;
    }

    dbg!(&players);

    let loser = players
        .iter()
        .max_by(|p1, p2| p2.score.cmp(&p1.score))
        .unwrap();
    dice.rolls * loser.score
}
