use aoc2021::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s23.txt"), 0);
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum Cell {
    Wall,
    Floor,
    Amp(Amp),
}

#[derive(Copy, Clone, PartialEq, Eq)]
struct Amp {
    c: char,
    moved: Moved,
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum Moved {
    No,
    Once,
    Twice,
}

fn can_move(g: &Grid) -> Vec<(Point, Amp)> {

    vec![]
}

fn a(s: &str) -> i64 {
    let mut grid = {
        let mut rows = vec![];
        for line in read_parsed::<String>(s) {
            let mut row = vec![];
            for c in line.chars() {
                row.push(match c {
                    ' ' | '#' => Cell::Wall,
                    '.' => Cell::Floor,
                    'A' | 'B' | 'C' | 'D' => Cell::Amp(Amp {
                        c,
                        moved: Moved::No,
                    }),
                    _ => unreachable!(),
                });
            }
            rows.push(row);
        }
        Grid::new(rows)
    };

    let mut amp_positions: BTreeSet<Point> = Default::default();

    for (p, a) in grid.points() {
        if let Cell::Amp(amp) = a {
            amp_positions.insert(p);
        }
    }


    1
}
