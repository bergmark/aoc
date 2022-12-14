use aoc2022::point::{Col, Row};
use aoc2022::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(sol("txt/s14.txt", false), 24);
    assert_eq!(sol("txt/e14.txt", false), 1072);
    assert_eq!(sol("txt/s14.txt", true), 93);
    assert_eq!(sol("txt/e14.txt", true), 24659);
}

fn sol(s: &str, infinite_bottom: bool) -> usize {
    let paths = parse(s);
    let mut min_col = 1000;
    let mut max_col = 0;
    let min_row = 0;
    let mut max_row = 0;
    for path in &paths {
        for point in path {
            min_col = std::cmp::min(min_col, point.col);
            max_col = std::cmp::max(max_col, point.col);
            max_row = std::cmp::max(max_row, point.row);
        }
    }
    if infinite_bottom {
        max_row += 1;
    }
    let row_size = max_row - min_row + 1;
    let size = Point {
        row: max_row - min_row + 1,
        col: max_col - min_col + 1 + if infinite_bottom { row_size * 2 } else { 0 },
    };

    let offset = Point {
        row: min_row,
        col: min_col - if infinite_bottom { row_size } else { 0 },
    };

    let mut grid = Grid::init(size, '.');

    for path in &paths {
        for (&a, &b) in path.iter().zip(path.iter().skip(1)) {
            let line = Line {
                a: a - offset,
                b: b - offset,
            };
            for point in line.points() {
                grid.insert(point, '#').unwrap();
            }
        }
    }

    let start_point = Point { col: 500, row: 0 } - offset;

    let mut units = 0;
    'oob: loop {
        let mut sand = start_point;
        'next: loop {
            let attempts = [
                sand + Row(1),
                sand + Row(1) - Col(1),
                sand + Row(1) + Col(1),
            ];
            for attempt in attempts {
                if let Some('.') = grid.get(attempt) {
                    sand = attempt;
                    continue 'next;
                } else if !infinite_bottom && grid.get(attempt).is_none() {
                    break 'oob;
                }
            }
            units += 1;
            grid.insert(sand, 'o').unwrap();
            if sand == start_point {
                break 'oob;
            }
            continue 'oob;
        }
    }

    units
}

fn parse(s: &str) -> Vec<Vec<Point>> {
    read_parsed_with(s, |s| {
        s.split(" -> ")
            .map(|s: &str| {
                let mut i = s.split(',');
                let col = i.next().unwrap().parse().unwrap();
                let row = i.next().unwrap().parse().unwrap();
                Point { col, row }
            })
            .collect()
    })
    .collect()
}
