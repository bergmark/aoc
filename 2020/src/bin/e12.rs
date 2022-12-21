use aoc2020::direction::*;
use aoc2020::point::*;
use aoc2020::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s12.txt"), 25);
    assert_eq!(a("txt/e12.txt"), 858);
    assert_eq!(b("txt/s12.txt"), 286);
}

fn a(filename: &str) -> i64 {
    let mut face = Direction::E;
    let mut p = Point::default();
    for Ins { mov, val } in read_parsed(filename).collect::<Vec<Ins>>() {
        use Move::*;
        match mov {
            D(d) => p += d.increment() * val,
            L => face = face.rotate_left(val),
            R => face = face.rotate_right(val),
            F => p += face.increment() * val,
        }
    }
    p.row.abs() + p.col.abs()
}

fn b(filename: &str) -> i64 {
    let mut i = 0;
    let mut waypoint = Point { row: 1, col: 10 };
    let mut ship = Point::default();
    for Ins { mov, val } in read_parsed(filename).collect::<Vec<Ins>>() {
        i += 1;
        use Move::*;
        match mov {
            D(d) => waypoint += d.increment() * val,
            L => waypoint = waypoint.rotate_left(val),
            R => waypoint = waypoint.rotate_right(val),
            F => ship += waypoint * val,
        }
        println!("ship: {:?}, waypoint: {:?}", ship, waypoint);
        match i {
            1 => assert_eq!(
                (ship, waypoint),
                (Point { row: 10, col: 100 }, Point { row: 1, col: 10 })
            ),
            2 => assert_eq!(
                (ship, waypoint),
                (Point { row: 10, col: 100 }, Point { row: 4, col: 10 })
            ),
            3 => assert_eq!(
                (ship, waypoint),
                (Point { row: 38, col: 170 }, Point { row: 4, col: 10 })
            ),
            4 => assert_eq!(
                (ship, waypoint),
                (Point { row: 38, col: 170 }, Point { row: -10, col: 4 })
            ),
            5 => assert_eq!(
                (ship, waypoint),
                (Point { row: -72, col: 214 }, Point { row: -10, col: 4 })
            ),
            _ => unreachable!()
        }
    }
    ship.row.abs() + ship.col.abs()
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct Ins {
    mov: Move,
    val: i64,
}

impl FromStr for Ins {
    type Err = ();
    fn from_str(s: &str) -> Result<Ins, ()> {
        let (c, rest) = s.split_at(1);
        let mov: char = c.chars().nth(0).ok_or(())?;
        let mov: Move = mov.try_into()?;
        let val = rest.parse().map_err(|_| ())?;
        Ok(Ins { mov, val })
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Move {
    D(Direction),
    L,
    R,
    F,
}

impl TryFrom<char> for Move {
    type Error = ();
    fn try_from(c: char) -> Result<Move, ()> {
        use Direction::*;
        use Move::*;
        match c {
            'N' => Ok(D(N)),
            'S' => Ok(D(S)),
            'E' => Ok(D(E)),
            'W' => Ok(D(W)),
            'L' => Ok(L),
            'R' => Ok(R),
            'F' => Ok(F),
            _ => Err(()),
        }
    }
}
