use aoc2020::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s03.txt"), 7);
    assert_eq!(a("txt/e03.txt"), 250);
    assert_eq!(b("txt/s03.txt"), 336);
    assert_eq!(b("txt/e03.txt"), 1592662500);
}

fn a(s: &str) -> usize {
    sol(s, &[Movement { right: 3, down: 1 }])
}

fn b(s: &str) -> usize {
    sol(
        s,
        &[
            Movement { right: 1, down: 1 },
            Movement { right: 3, down: 1 },
            Movement { right: 5, down: 1 },
            Movement { right: 7, down: 1 },
            Movement { right: 1, down: 2 },
        ],
    )
}

fn sol(s: &str, movements: &[Movement]) -> usize {
    let mut v = Vec::new();
    for line in read_lines(s) {
        let mut row: Vec<Tile> = Vec::new();
        for c in line.unwrap().chars() {
            row.push(Tile::from(c));
        }
        v.push(row)
    }
    let mut board = Board {
        v,
        pos: Coord::default(),
    };
    let mut product = 1;
    for &movement in movements {
        let mut trees = 0;
        while let Some(tile) = board.move_(movement) {
            if tile == Tile::Tree {
                trees += 1;
            }
        }
        product *= trees;
        board.pos = Coord::default();
    }
    product
}

#[derive(Default)]
struct Coord {
    row: usize,
    col: usize,
}
#[derive(Copy, Clone)]
struct Movement {
    down: usize,
    right: usize,
}

struct Board {
    v: Vec<Vec<Tile>>,
    pos: Coord,
}

impl Board {
    //fn at_bottom(&self) -> bool {
    //    self.pos.row >= self.v.len() - 1
    //}
    fn move_(&mut self, movement: Movement) -> Option<Tile> {
        self.pos.row += movement.down;
        self.pos.col += movement.right;
        let row = self.v.get(self.pos.row)?;
        let &col = row.get(self.pos.col % row.len())?;
        Some(col)
    }
}

#[derive(PartialEq, Eq, Copy, Clone)]
pub enum Tile {
    Open,
    Tree,
}

impl Tile {
    fn from(c: char) -> Tile {
        match c {
            '.' => Tile::Open,
            '#' => Tile::Tree,
            _ => unreachable!(),
        }
    }
}
