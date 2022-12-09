use aoc2022::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s09.txt"), 13);
    assert_eq!(a("txt/e09.txt"), 5878);
    assert_eq!(b("txt/s09.txt"), 1);
    assert_eq!(b("txt/s09b.txt"), 36);
    assert_eq!(b("txt/e09.txt"), 2405);
}

fn a(s: &str) -> usize {
    let mut head_pos = Point::default();
    let mut tail_pos = Point::default();
    let mut visited = BTreeSet::from([tail_pos]);

    for Row { dir, mut count } in parse(s) {
        while count > 0 {
            head_pos += dir.increment();
            let dist = head_pos - tail_pos;
            if dist.row.abs() == 2 || dist.col.abs() == 2 {
                tail_pos += Point {
                    row: dist.row.signum(),
                    col: dist.col.signum(),
                };
                visited.insert(tail_pos);
            }
            count -= 1;
        }
    }
    visited.len()
}

fn b(s: &str) -> usize {
    let mut visited: BTreeSet<Point> = BTreeSet::new();
    let mut knots = vec![Point::default(); 10];

    for Row { dir, mut count } in parse(s) {
        while count > 0 {
            let mut prev_knot_pos: Option<Point> = None;
            for knot in &mut knots {
                if let Some(prev) = prev_knot_pos {
                    let dist = prev - *knot;
                    if dist.row.abs() == 2 || dist.col.abs() == 2 {
                        *knot += Point {
                            row: dist.row.signum(),
                            col: dist.col.signum(),
                        };
                    }
                } else {
                    *knot += dir.increment();
                }
                prev_knot_pos = Some(*knot);
            }
            visited.insert(*knots.last().unwrap());
            count -= 1;
        }
    }
    visited.len()
}

fn parse(s: &str) -> Vec<Row> {
    read_parsed_with(s, |l| {
        let v: Vec<_> = l.split(' ').collect();
        let dir = match v[0] {
            "R" => Direction::E,
            "D" => Direction::S,
            "L" => Direction::W,
            "U" => Direction::N,
            _ => unreachable!(),
        };
        let count = v[1].parse::<usize>().unwrap();
        Row { dir, count }
    })
    .collect()
}

#[allow(dead_code)]
struct Row {
    dir: Direction,
    count: usize,
}
