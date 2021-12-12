use aoc2021::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s02.txt"), 150);
    assert_eq!(a("txt/e02.txt"), 1947824);
    assert_eq!(b("txt/s02.txt"), 900);
    assert_eq!(b("txt/e02.txt"), 1813062561);
}

pub struct Line {
    direction: Direction,
    amount: i64,
}

impl FromStr for Line {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (direction, amount) = split2(s, " ")?;
        Ok(Line {
            direction: Direction::from_str(direction)?,
            amount: i64::from_str(amount).map_err(|_| ())?,
        })
    }
}

pub enum Direction {
    Up,
    Down,
    Forward,
}

impl FromStr for Direction {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        use Direction::*;
        match s {
            "forward" => Ok(Forward),
            "down" => Ok(Down),
            "up" => Ok(Up),
            _ => Err(()),
        }
    }
}

fn a(s: &str) -> i64 {
    let mut horizontal = 0;
    let mut depth = 0;

    for Line { direction, amount } in read_parsed::<Line>(s) {
        use Direction::*;
        match direction {
            Up => depth -= amount,
            Down => depth += amount,
            Forward => horizontal += amount,
        }
    }

    horizontal * depth
}

fn b(s: &str) -> i64 {
    let mut aim = 0;
    let mut horizontal = 0;
    let mut depth = 0;

    for Line { direction, amount } in read_parsed::<Line>(s) {
        use Direction::*;
        match direction {
            Up => aim -= amount,
            Down => aim += amount,
            Forward => {
                horizontal += amount;
                depth += aim * amount;
            }
        }
    }

    horizontal * depth
}
