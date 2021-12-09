use crate::point::*;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Direction {
    N,
    NE,
    E,
    SE,
    S,
    SW,
    W,
    NW,
}
impl Direction {
    pub const ALL: [Direction; 8] = [
        Direction::N,
        Direction::NE,
        Direction::E,
        Direction::SE,
        Direction::S,
        Direction::SW,
        Direction::W,
        Direction::NW,
    ];

    pub const STRAIGHT: [Direction; 4] = [
        Direction::N,
        Direction::E,
        Direction::S,
        Direction::W,
    ];

    pub fn all() -> impl Iterator<Item = Direction> {
        Self::ALL.iter().copied()
    }

    #[rustfmt::skip]
    pub fn increment(self) -> Point {
        use Direction::*;
        let (row, col) = match self {
            N  => (-1,  0),
            NE => (-1,  1),
            E  => ( 0,  1),
            SE => ( 1,  1),
            S  => ( 1,  0),
            SW => ( 1, -1),
            W  => ( 0, -1),
            NW => (-1, -1),
        };
        Point { row, col }
    }

    pub fn rotate_left(self, deg: i64) -> Direction {
        self.rotate_right(360 - deg)
    }

    pub fn rotate_right(mut self, mut deg: i64) -> Direction {
        use Direction::*;
        while deg != 0 {
            if deg >= 90 {
                self = match self {
                    N => E,
                    E => S,
                    S => W,
                    W => N,
                    _ => todo!(),
                };
                deg -= 90;
            } else {
                todo!()
            }
        }
        self
    }
}
