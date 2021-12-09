use crate::{Direction, Point};

#[derive(Debug)]
pub struct Grid<A = usize> {
    rows: Vec<Vec<A>>,
}

impl<A: Copy> Grid<A> {
    pub fn new(rows: Vec<Vec<A>>) -> Grid<A> {
        Grid { rows }
    }

    pub fn points<'a>(&'a self) -> impl Iterator<Item = (Point, A)> + 'a {
        PointIterator { grid: self, curr: Point { row: 0, col: 0 } }
    }

    pub fn get_unwrap(&self, point: Point) -> A {
        let row = self.rows.get(point.row as usize).unwrap();
        *row.get(point.col as usize).unwrap()
    }

    pub fn get(&self, point: Point) -> Option<A> {
        let row = self.rows.get(point.row as usize)?;
        row.get(point.col as usize).copied()
    }

    pub fn col_len(&self) -> i64 {
        self.rows[0].len() as i64
    }

    pub fn row_len(&self) -> i64 {
        self.rows.len() as i64
    }

    pub fn contains(&self, point: Point) -> bool {
        (point.row >= 0 && point.row < self.row_len())
            && (point.col >= 0 && point.col < self.col_len())
    }

    pub fn neighbors(&self, point: Point) -> Vec<(Point, A)> {
        Direction::STRAIGHT
            .iter()
            .filter_map(move |d| {
                let p = point + d.increment();
                self.get(p).map(|v| (p, v))
            })
            .collect()
    }
}

struct PointIterator<'a, A> {
    grid: &'a Grid<A>,
    curr: Point,
}

impl<'a, A: Copy> Iterator for PointIterator<'a, A> {
    type Item = (Point, A);

    fn next(&mut self) -> Option<(Point, A)> {
        if self.curr.col < self.grid.col_len() - 1 {
            self.curr.col += 1;
            Some((self.curr, self.grid.get_unwrap(self.curr)))
        } else if self.curr.row < self.grid.row_len() - 1 {
            self.curr.row += 1;
            self.curr.col = 0;
            Some((self.curr, self.grid.get_unwrap(self.curr)))
        } else {
            None
        }
    }
}

// let cols = self.col_len();
// let rows = self.row_len();
// (0..rows)
//     .map(|row| {
//         (0..cols).map(move |col| {
//             let point = Point {
//                 row: row as i64,
//                 col: col as i64,
//             };
//             (point, self.get_unwrap(point))
//         })
//     })
//     .flatten()
