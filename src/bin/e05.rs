use aoc2021::{point::Point, *};

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
    assert_eq!(b("txt/e05.txt"), 0); // not 20459
}

fn a(s: &str) -> usize {
    let mut sparse: Count<Point> = Count::default();

    for line in read_parsed::<Line>(s) {
        for point in line.points_straight() {
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

fn b(s: &str) -> usize {
    let mut sparse: Count<Point> = Count::default();

    for line in read_parsed::<Line>(s) {
        for point in line.points_straight() {
            sparse.count(point);
        }
        for point in line.points_diag() {
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

impl Line {
    fn is_straight(self) -> bool {
        self.a.row == self.b.row || self.a.col == self.b.col
    }

    fn points_straight(self) -> Vec<Point> {
        let mut v = vec![];

        if self.a.row == self.b.row {
            let row = self.a.row;
            let col_start = std::cmp::min(self.a.col, self.b.col);
            let col_end = std::cmp::max(self.a.col, self.b.col);

            for col in col_start..=col_end {
                v.push(Point { row, col })
            }
        } else if self.a.col == self.b.col {
            let col = self.a.col;
            let row_start = std::cmp::min(self.a.row, self.b.row);
            let row_end = std::cmp::max(self.a.row, self.b.row);

            for row in row_start..=row_end {
                v.push(Point { row, col })
            }
        }

        v
    }

    fn points_diag(self) -> Vec<Point> {
        let mut v = vec![];

        if self.a.row != self.b.row && self.a.col != self.b.col {
            assert_eq!(
                (self.a.row - self.b.row).abs(),
                (self.a.col - self.b.col).abs()
            );

            dbg!(self);
            let col_inc = if self.a.col < self.b.col { 1 } else { -1 };
            let row_inc = if self.a.row < self.b.row { 1 } else { -1 };

            let mut col = self.a.col;
            let mut row = self.a.row;
            while col != self.b.col {
                v.push(dbg!(Point { col, row }));
                col += col_inc;
                row += row_inc;
            }
            v.push(dbg!(Point { col, row }));
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
