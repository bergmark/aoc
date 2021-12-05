use aoc2021::{point::Point, *, line::Line};

use pretty_assertions::assert_eq;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s05.txt"), 5);
    assert_eq!(a("txt/e05.txt"), 5092);
    assert_eq!(b("txt/s05.txt"), 12);
    assert_eq!(b("txt/e05.txt"), 20484);
}

fn a(s: &str) -> usize {
    let mut sparse: Count<Point> = Count::default();

    for L(line) in read_parsed::<L>(s) {
        if line.is_straight() {
            for point in line.points() {
                sparse.count(point);
            }
        }
    }

    let mut gt2 = 0;
    for count in sparse.counts() {
        if count >= 2 {
            gt2 += 1;
        }
    }

    gt2
}

fn b(s: &str) -> usize {
    let mut sparse: Count<Point> = Count::default();

    for L(line) in read_parsed::<L>(s) {
        for point in line.points() {
            sparse.count(point);
        }
    }

    let mut gt2 = 0;
    for count in sparse.counts() {
        if count >= 2 {
            gt2 += 1;
        }
    }

    gt2
}

fn point_from_str(s: &str) -> Result<Point, ()> {
    let (x, y) = split2(s, ",")?;
    Ok(Point {
        row: x.parse().unwrap(),
        col: y.parse().unwrap(),
    })
}

struct L(Line);

impl FromStr for L {
    type Err = ();
    fn from_str(s: &str) -> Result<L, ()> {
        let (a, b) = split2(s, " -> ")?;
        Ok(L(Line {
            a: point_from_str(a)?,
            b: point_from_str(b)?,
        }))
    }
}
