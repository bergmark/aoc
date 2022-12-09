use aoc2022::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s09.txt"), 13, "a sample");
    assert_eq!(a("txt/e09.txt"), 5878, "a exercise");
    assert_eq!(b("txt/s09.txt"), 1);
    assert_eq!(b("txt/s09b.txt"), 36);
    assert_eq!(b("txt/e09.txt"), 2405);
}

fn a(s: &str) -> usize {
    let mut visited: BTreeSet<Point> = BTreeSet::new();
    let mut head_pos = Point { row: 0, col: 0 };
    let mut tail_pos = Point { row: 0, col: 0 };
    visited.insert(tail_pos);

    for Row { dir, mut count } in parse(s) {
        while count > 0 {
            let old_head_pos = head_pos;
            head_pos += dir.increment();
            let dist = head_pos.distance(tail_pos);
            if dist.0 >= 2 || dist.1 >= 2 {
                tail_pos = old_head_pos;
                visited.insert(tail_pos);
            }
            count -= 1;
        }
    }
    visited.len()
}

#[derive(Clone, Debug)]
struct P(Point);

fn b(s: &str) -> usize {
    let mut visited: BTreeSet<Point> = BTreeSet::new();
    let mut knots = vec![P(Point::default()); 10];

    for Row { dir, mut count } in parse(s) {
        println!();
        println!("=== {dir:?} {count}");
        while count > 0 {
            println!("move {count}");
            let mut prev_knot_pos: Option<P> = None;
            for knot in &mut knots {
                if let Some(prev_p) = prev_knot_pos {
                    let dist = prev_p.0.distance2(knot.0);
                    if dist.0.abs() == 2 || dist.1.abs() == 2 {
                        *knot = P(knot.0
                            + Point {
                                row: dist.0.signum(),
                                col: dist.1.signum(),
                            });
                    }
                    prev_knot_pos = Some(knot.clone());
                } else {
                    let old = knot.clone();
                    *knot = P(knot.0 + dir.increment());
                    println!(
                        "moving knot 0 from {old} to {knot}",
                        old = old.0,
                        knot = knot.0
                    );
                    prev_knot_pos = Some(knot.clone());
                }
            }
            println!("## last knot visited {}", knots.last().unwrap().0);
            visited.insert(knots.last().unwrap().0);
            count -= 1;
        }
    }
    dbg!(&visited);
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
