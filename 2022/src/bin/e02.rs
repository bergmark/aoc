use aoc2021::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    //assert_eq!(a("txt/s02.txt"), 15);
    //assert_eq!(a("txt/e02.txt"), 13446);
    assert_eq!(b("txt/s02.txt"), 12);
    assert_eq!(b("txt/e02.txt"), 13509);
}

fn _a(_s: &str) -> i64 {
    0 // read_parsed::<Round>(s).map(score_a).sum()
}

fn b(s: &str) -> i64 {
    read_parsed::<Round>(s).map(score_b).sum()
}

fn score_b(round: Round) -> i64 {
    use Goal::*;
    use RPS::*;
    let res = round.b.score();
    let rps = match (round.a, round.b) {
        (Rock, Draw) => Rock,
        (Paper, Draw) => Paper,
        (Scissor, Draw) => Scissor,

        (Rock, Lose) => Scissor,
        (Rock, Win) => Paper,

        (Paper, Lose) => Rock,
        (Paper, Win) => Scissor,

        (Scissor, Lose) => Paper,
        (Scissor, Win) => Rock,
    };
    res + rps.score()
}

#[derive(Debug, Copy, Clone)]
enum RPS {
    Rock,
    Paper,
    Scissor,
}
impl RPS {
    fn score(self) -> i64 {
        use RPS::*;
        match self {
            Rock => 1,
            Paper => 2,
            Scissor => 3,
        }
    }
}

#[derive(Debug, Copy, Clone)]
enum Goal {
    Lose,
    Draw,
    Win,
}

#[derive(Debug, Copy, Clone)]
struct Round {
    a: RPS,
    b: Goal,
}
impl Goal {
    fn score(self) -> i64 {
        use Goal::*;
        match self {
            Lose => 0,
            Draw => 3,
            Win => 6,
        }
    }
}

impl FromStr for Round {
    type Err = ();
    fn from_str(s: &str) -> Result<Round, ()> {
        use Goal::*;
        use RPS::*;
        let l: Vec<char> = s.chars().collect();
        let a = match l[0] {
            'A' => Rock,
            'B' => Paper,
            'C' => Scissor,
            _ => todo!(),
        };
        let b = match l[2] {
            'X' => Lose,
            'Y' => Draw,
            'Z' => Win,
            _ => todo!(),
        };
        Ok(Round { a, b })
    }
}
