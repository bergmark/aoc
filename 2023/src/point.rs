use crate::*;

#[derive(Copy, Clone, PartialEq, Eq, Debug, Default, Hash, PartialOrd, Ord)]
pub struct Point {
    pub col: i64,
    pub row: i64,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Row(pub i64);
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Col(pub i64);

impl fmt::Display for Point {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "r{},c{}", self.row, self.col)
    }
}

impl Add for Point {
    type Output = Point;
    fn add(self, rhs: Point) -> Point {
        Point {
            row: self.row + rhs.row,
            col: self.col + rhs.col,
        }
    }
}

impl Add<Direction> for Point {
    type Output = Point;
    fn add(self, rhs: Direction) -> Point {
        self + rhs.increment()
    }
}

impl Add<Row> for Point {
    type Output = Point;
    fn add(self, rhs: Row) -> Point {
        Point {
            row: self.row + rhs.0,
            col: self.col,
        }
    }
}

impl Add<Col> for Point {
    type Output = Point;
    fn add(self, rhs: Col) -> Point {
        Point {
            row: self.row,
            col: self.col + rhs.0,
        }
    }
}

impl AddAssign for Point {
    fn add_assign(&mut self, rhs: Point) {
        *self = *self + rhs
    }
}

impl AddAssign<Direction> for Point {
    fn add_assign(&mut self, rhs: Direction) {
        *self = *self + rhs
    }
}

impl Mul<i64> for Point {
    type Output = Point;
    fn mul(self, rhs: i64) -> Point {
        Point {
            row: self.row * rhs,
            col: self.col * rhs,
        }
    }
}

impl Sub for Point {
    type Output = Point;
    fn sub(self, rhs: Point) -> Point {
        Point {
            row: self.row - rhs.row,
            col: self.col - rhs.col,
        }
    }
}

impl Sub<Row> for Point {
    type Output = Point;
    fn sub(self, rhs: Row) -> Point {
        Point {
            row: self.row - rhs.0,
            col: self.col,
        }
    }
}

impl Sub<Col> for Point {
    type Output = Point;
    fn sub(self, rhs: Col) -> Point {
        Point {
            row: self.row,
            col: self.col - rhs.0,
        }
    }
}

impl SubAssign<Row> for Point {
    fn sub_assign(&mut self, rhs: Row) {
        *self = *self - rhs;
    }
}

impl SubAssign<Col> for Point {
    fn sub_assign(&mut self, rhs: Col) {
        *self = *self - rhs;
    }
}

impl Point {
    pub fn min_both(self, other: Point) -> Point {
        Point {
            row: std::cmp::min(self.row, other.row),
            col: std::cmp::min(self.col, other.col),
        }
    }
    pub fn max_both(self, other: Point) -> Point {
        Point {
            row: std::cmp::max(self.row, other.row),
            col: std::cmp::max(self.col, other.col),
        }
    }

    pub fn rotate_left(self, deg: i64) -> Point {
        self.rotate_right(360 - deg)
    }

    pub fn rotate_right(mut self, mut deg: i64) -> Point {
        while deg != 0 {
            if deg >= 90 {
                self = Point {
                    row: -self.col,
                    col: self.row,
                };
            } else {
                todo!()
            }
            deg -= 90;
        }
        self
    }
}

#[test]
fn rotate_right() {
    assert_eq!(
        Point { row: 2, col: 1 }.rotate_right(90),
        Point { row: -1, col: 2 },
        "TR"
    );
    assert_eq!(
        Point { row: -1, col: 2 }.rotate_right(90),
        Point { row: -2, col: -1 },
        "BR"
    );
    assert_eq!(
        Point { row: -2, col: -1 }.rotate_right(90),
        Point { row: 1, col: -2 },
        "BL"
    );
    assert_eq!(
        Point { row: 1, col: -2 }.rotate_right(90),
        Point { row: 2, col: 1 },
        "TL"
    );
}
