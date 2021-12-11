use std::fmt::Display;

use crate::{Direction, Point};

#[derive(Debug)]
pub struct Grid<A = usize> {
    rows: Vec<Vec<A>>,
}

pub fn print_grid<A: Display>(grid: &Grid<A>) {
    for row in &grid.rows {
        for cell in row {
            print!("{}", cell);
        }
        println!();
    }
    println!();
}

impl<A: Copy> Grid<A> {
    pub fn new(rows: Vec<Vec<A>>) -> Grid<A> {
        Grid { rows }
    }

    pub fn points(&self) -> impl Iterator<Item = (Point, A)> + '_ {
        PointIterator {
            grid: self,
            curr: Point { row: 0, col: 0 },
        }
    }

    // pub fn points_mut(&mut self) -> impl Iterator<Item = (Point, &mut A)> + '_ {
    //     PointMutIterator {
    //         grid: self,
    //         curr: Point { row: 0, col: 0 },
    //     }
    // }

    pub fn get_unwrap(&self, point: Point) -> A {
        self.get(point).unwrap()
    }

    pub fn get(&self, point: Point) -> Option<A> {
        let row = self.rows.get(point.row as usize)?;
        row.get(point.col as usize).copied()
    }

    pub fn len(&self) -> Point {
        Point {
            row: self.rows.len() as i64,
            col: self.rows[0].len() as i64,
        }
    }

    pub fn contains(&self, point: Point) -> bool {
        (point.row >= 0 && point.row < self.len().row)
            && (point.col >= 0 && point.col < self.len().col)
    }

    pub fn straight_neighbors(&self, point: Point) -> impl Iterator<Item = (Point, A)> + '_ {
        StraightNeighborIterator {
            grid: self,
            point,
            i: 0,
        }
    }

    pub fn insert(&mut self, point: Point, val: A) {
        self.rows[point.row as usize][point.col as usize] = val;
    }
}

struct PointIterator<'a, A> {
    grid: &'a Grid<A>,
    curr: Point,
}

impl<'a, A: Copy> Iterator for PointIterator<'a, A> {
    type Item = (Point, A);

    fn next(&mut self) -> Option<(Point, A)> {
        let PointIterator { grid, curr } = self;
        let len = grid.len();
        if grid.contains(*curr) {
            let val = (*curr, grid.get_unwrap(*curr));
            if curr.col < len.col - 1 {
                curr.col += 1;
            } else {
                curr.row += 1;
                curr.col = 0;
            }
            Some(val)
        } else {
            None
        }
    }
}

//struct PointMutIterator<'a, A> {
//    grid: &'a mut Grid<A>,
//    curr: Point,
//}
//
//impl<'a, A: Copy> Iterator for PointMutIterator<'a, A> {
//    type Item = (Point, &'a mut A);
//
//    fn next(&mut self) -> Option<(Point, &mut A)> {
//        let PointMutIterator { grid, curr } = self;
//        let len = grid.len();
//        if curr.col < len.col - 1 {
//            curr.col += 1;
//            Some((*curr, grid.get_unwrap(*curr)))
//        } else if curr.row < len.row - 1 {
//            curr.row += 1;
//            curr.col = 0;
//            Some((*curr, grid.get_unwrap(*curr)))
//        } else {
//            None
//        }
//    }
//}

struct StraightNeighborIterator<'a, A> {
    grid: &'a Grid<A>,
    point: Point,
    i: usize,
}

impl<'a, A: Copy> Iterator for StraightNeighborIterator<'a, A> {
    type Item = (Point, A);

    fn next(&mut self) -> Option<(Point, A)> {
        loop {
            if self.i < Direction::STRAIGHT.len() {
                let dir = Direction::STRAIGHT[self.i];
                self.i += 1;
                let point = self.point + dir.increment();
                if let Some(v) = self.grid.get(point) {
                    return Some((point, v));
                }
            } else {
                return None;
            }
        }
    }
}
