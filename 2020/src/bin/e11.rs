use aoc2020::point::*;
use aoc2020::direction::*;
use aoc2020::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s11.txt"), 37);
    assert_eq!(a("txt/e11.txt"), 2329);
    assert_eq!(b("txt/s11.txt"), 26);
    assert_eq!(b("txt/e11.txt"), 2138);
}

fn a(filename: &str) -> usize {
    let mut old: Board = Board {
        rows: read_parsed(filename).collect(),
    };
    loop {
        let mut new = old.clone();
        for (point, cell) in old.points() {
            match cell {
                Cell::Unoccupied => {
                    if old.adjacent(point).filter(|c| c.is_occupied()).count() == 0 {
                        new.set(point, Cell::Occupied)
                    }
                }
                Cell::Floor => {}
                Cell::Occupied => {
                    if old.adjacent(point).filter(|c| c.is_occupied()).count() >= 4 {
                        new.set(point, Cell::Unoccupied)
                    }
                }
            }
        }
        if old == new {
            break;
        }
        old = new;
    }

    old.cells().filter(|c| c.is_occupied()).count()
}

fn b(filename: &str) -> usize {
    let mut old: Board = Board {
        rows: read_parsed(filename).collect(),
    };
    loop {
        let mut new = old.clone();
        // println!("{}", new);
        // println!();
        for (point, cell) in old.points() {
            match cell {
                Cell::Unoccupied => {
                    if old.visible(point) == 0 {
                        new.set(point, Cell::Occupied)
                    }
                }
                Cell::Floor => {}
                Cell::Occupied => {
                    if old.visible(point) >= 5 {
                        new.set(point, Cell::Unoccupied)
                    }
                }
            }
        }
        if old == new {
            break;
        }
        old = new;
    }

    old.cells().filter(|c| c.is_occupied()).count()
}

#[derive(Clone, Eq, PartialEq)]
struct Board {
    rows: Vec<Line>,
}

impl Board {
    fn ix(&self, point: Point) -> Option<Cell> {
        if !self.contains(point) {
            None
        } else {
            self.rows
                .get(point.row as usize)
                .and_then(|row| row.cells.get(point.col as usize))
                .copied()
        }
    }

    fn contains(&self, point: Point) -> bool {
        point.row >= 0 && point.row >= 0 && point.row < self.row_len() && point.col < self.col_len()
    }

    fn set(&mut self, point: Point, cell: Cell) {
        self.rows[point.row as usize].cells[point.col as usize] = cell;
    }

    fn adjacent(&self, point: Point) -> impl Iterator<Item = Cell> {
        let mut v = vec![];

        for direction in Direction::all() {
            v.push(self.ix(point + direction.increment()));
        }

        v.into_iter().flatten()
    }

    fn visible_occupied(&self, mut point: Point, inc: Point) -> bool {
        loop {
            point += inc;
            let cell = self.ix(point);
            match cell {
                None => return false,
                Some(Cell::Unoccupied) => return false,
                Some(Cell::Floor) => {}
                Some(Cell::Occupied) => return true,
            }
        }
    }

    fn visible(&self, point: Point) -> usize {
        let mut count = 0;
        for direction in Direction::all() {
            if self.visible_occupied(point, direction.increment()) {
                count += 1;
            }
        }
        count
    }

    fn row_len(&self) -> i64 {
        self.rows.len() as i64
    }

    fn col_len(&self) -> i64 {
        self.rows[0].cells.len() as i64
    }

    fn points<'a>(&'a self) -> impl Iterator<Item = (Point, Cell)> + 'a {
        BoardPointIterator::new(
            self,
            Point {
                row: self.row_len(),
                col: self.col_len(),
            },
        )
    }

    fn cells(&self) -> impl Iterator<Item = Cell> + '_ {
        self.rows
            .iter()
            .map(|row| row.cells.iter())
            .flatten()
            .copied()
    }
}

impl fmt::Display for Board {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        for row in &self.rows {
            write!(f, "{}\n", row)?;
        }
        Ok(())
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
struct BoardPointIterator<'a> {
    board: &'a Board,
    curr: Point,
    max: Point,
}

impl<'a> BoardPointIterator<'a> {
    fn new(board: &'a Board, max: Point) -> BoardPointIterator<'a> {
        Self {
            board,
            curr: Point { col: 0, row: 0 },
            max,
        }
    }
}

impl Iterator for BoardPointIterator<'_> {
    type Item = (Point, Cell);
    fn next(&mut self) -> Option<Self::Item> {
        let res = if self.curr.row == self.max.row {
            return None;
        } else {
            self.curr
        };
        if self.curr.col == self.max.col - 1 {
            self.curr.col = 0;
            self.curr.row += 1;
        } else {
            self.curr.col += 1;
        }
        Some((res, self.board.ix(res).unwrap()))
    }
}

struct BoardDirectionIterator<'a> {
    curr: Point,
    direction: Direction,
    board: &'a Board,
}

impl Iterator for BoardDirectionIterator<'_> {
    type Item = Cell;
    fn next(&mut self) -> Option<Cell> {
        self.curr += self.direction.increment();
        self.board.ix(self.curr)
    }
}

#[derive(Clone, PartialEq, Eq)]
struct Line {
    cells: Vec<Cell>,
}

impl fmt::Display for Line {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        for cell in &self.cells {
            write!(f, "{}", cell)?;
        }
        Ok(())
    }
}

impl FromStr for Line {
    type Err = char;
    fn from_str(s: &str) -> Result<Line, char> {
        Ok(Line {
            cells: s
                .chars()
                .map(Cell::try_from)
                .collect::<Result<Vec<_>, _>>()?,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Hash, Copy, Clone)]
enum Cell {
    Floor,
    Occupied,
    Unoccupied,
}

impl Cell {
    fn is_occupied(self) -> bool {
        use Cell::*;
        match self {
            Floor => false,
            Occupied => true,
            Unoccupied => false,
        }
    }
}

impl TryFrom<char> for Cell {
    type Error = char;
    fn try_from(c: char) -> Result<Cell, char> {
        match c {
            'L' => Ok(Cell::Unoccupied),
            '.' => Ok(Cell::Floor),
            c => Err(c),
        }
    }
}

impl fmt::Display for Cell {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        use Cell::*;
        write!(
            f,
            "{}",
            match self {
                Floor => ".",
                Occupied => "#",
                Unoccupied => "L",
            }
        )?;
        Ok(())
    }
}
