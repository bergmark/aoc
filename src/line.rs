use crate::{*, point::Point};

#[derive(Debug, Copy, Clone)]
pub struct Line {
    pub a: Point,
    pub b: Point,
}

impl fmt::Display for Line {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}->{}", self.a, self.b)
    }
}

impl Line {
    pub fn is_straight(self) -> bool {
        self.a.row == self.b.row || self.a.col == self.b.col
    }

    fn inc(a: i64, b: i64) -> i64 {
        match a.cmp(&b) {
            Ordering::Less => 1,
            Ordering::Greater => -1,
            Ordering::Equal => 0,
        }
    }

    pub fn manhattan_distance(self) -> i64 {
        (self.a.row - self.b.row).abs() + (self.a.col - self.b.col).abs()
    }

    pub fn distance(self) -> i64 {
        let row_dist = (self.a.row - self.b.row).abs();
        if row_dist > 0 {
            row_dist
        } else {
            (self.a.col - self.b.col).abs()
        }
    }

    pub fn contains(self, point: Point) -> bool {
        let col_min = std::cmp::min(self.a.col, self.b.col);
        let col_max = std::cmp::max(self.a.col, self.b.col);
        let row_min = std::cmp::min(self.a.row, self.b.row);
        let row_max = std::cmp::max(self.a.row, self.b.row);
        (point.col >= col_min && point.col <= col_max)
            && (point.row >= row_min && point.row <= row_max)
    }

    pub fn points(self) -> Vec<Point> {
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
