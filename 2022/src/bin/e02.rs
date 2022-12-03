use aoc2022::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s02.txt"), 15);
    assert_eq!(a("txt/e02.txt"), 13446);
    assert_eq!(b("txt/s02.txt"), 12);
    assert_eq!(b("txt/e02.txt"), 13509);
}

fn a(s: &str) -> i64 {
    read_parsed(s).map(score_a).sum()
}

fn b(s: &str) -> i64 {
    read_parsed(s).map(score_b).sum()
}

fn score_a(round: RoundA) -> i64 {
    use Goal::*;
    use RPS::*;
    round.b.score()
        + (match (round.a, round.b) {
            (Rock, Rock) => Draw,
            (Rock, Paper) => Win,
            (Rock, Scissor) => Lose,

            (Paper, Rock) => Lose,
            (Paper, Paper) => Draw,
            (Paper, Scissor) => Win,

            (Scissor, Rock) => Win,
            (Scissor, Paper) => Lose,
            (Scissor, Scissor) => Draw,
        })
        .score()
}

fn score_b(round: RoundB) -> i64 {
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

#[derive(Debug, Copy, Clone)]
struct RoundA {
    a: RPS,
    b: RPS,
}

impl FromStr for RoundA {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, ()> {
        use RPS::*;
        let l: Vec<char> = s.chars().collect();
        let a = match l[0] {
            'A' => Rock,
            'B' => Paper,
            'C' => Scissor,
            _ => todo!(),
        };
        let b = match l[2] {
            'X' => Rock,
            'Y' => Paper,
            'Z' => Scissor,
            _ => todo!(),
        };
        Ok(Self { a, b })
    }
}

#[derive(Debug, Copy, Clone)]
struct RoundB {
    a: RPS,
    b: Goal,
}

impl FromStr for RoundB {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, ()> {
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
        Ok(Self { a, b })
    }
}
