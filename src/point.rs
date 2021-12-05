use crate::*;

#[derive(Copy, Clone, PartialEq, Eq, Debug, Default, Hash)]
pub struct Point {
    pub row: i64,
    pub col: i64,
}

pub struct Row(pub i64);
pub struct Col(pub i64);

impl Add for Point {
    type Output = Point;
    fn add(self, rhs: Point) -> Point {
        Point {
            row: self.row + rhs.row,
            col: self.col + rhs.col,
        }
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

impl Mul<i64> for Point {
    type Output = Point;
    fn mul(self, rhs: i64) -> Point {
        Point {
            row: self.row * rhs,
            col: self.col * rhs,
        }
    }
}

impl Point {
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
