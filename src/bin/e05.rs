use aoc2021::{point::Point, *};

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

    for line in read_parsed::<Line>(s) {
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

    for line in read_parsed::<Line>(s) {
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

#[derive(Debug, Copy, Clone)]
struct Line {
    a: Point,
    b: Point,
}

impl fmt::Display for Line {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}->{}", self.a, self.b)
    }
}

impl Line {
    fn is_straight(self) -> bool {
        self.a.row == self.b.row || self.a.col == self.b.col
    }

    fn inc(a: i64, b: i64) -> i64 {
        match a.cmp(&b) {
            Ordering::Less => 1,
            Ordering::Greater => -1,
            Ordering::Equal => 0,
        }
    }

    fn manhattan_distance(self) -> i64 {
        (self.a.row - self.b.row).abs() + (self.a.col - self.b.col).abs()
    }

    fn distance(self) -> i64 {
        let row_dist = (self.a.row - self.b.row).abs();
        if row_dist > 0 {
            row_dist
        } else {
            (self.a.col - self.b.col).abs()
        }
    }

    fn contains(self, point: Point) -> bool {
        let col_min = std::cmp::min(self.a.col, self.b.col);
        let col_max = std::cmp::max(self.a.col, self.b.col);
        let row_min = std::cmp::min(self.a.row, self.b.row);
        let row_max = std::cmp::max(self.a.row, self.b.row);
        (point.col >= col_min && point.col <= col_max)
            && (point.row >= row_min && point.row <= row_max)
    }

    fn points(self) -> Vec<Point> {
        let mut v = vec![];

        let row_inc = Self::inc(self.a.row, self.b.row);
        let col_inc = Self::inc(self.a.col, self.b.col);
        let mut row = self.a.row;
        let mut col = self.a.col;

        for _ in 0..=self.distance() {
            let p = Point { col, row };
            assert!(self.contains(p), "line: {}, point: {}", self, p);
            v.push(p);
            col += col_inc;
            row += row_inc;
        }

        v
    }

}

fn point_from_str(s: &str) -> Result<Point, ()> {
    let (x, y) = split2(s, ",")?;
    Ok(Point {
        row: x.parse().unwrap(),
        col: y.parse().unwrap(),
    })
}

impl FromStr for Line {
    type Err = ();
    fn from_str(s: &str) -> Result<Line, ()> {
        let (a, b) = split2(s, " -> ")?;
        Ok(Line {
            a: point_from_str(a)?,
            b: point_from_str(b)?,
        })
    }
}
