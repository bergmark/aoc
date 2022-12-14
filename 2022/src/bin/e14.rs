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
    assert_eq!(a("txt/s14.txt"), 24);
    assert_eq!(a("txt/e14.txt"), 1072);
    assert_eq!(b("txt/s14.txt"), 93);
    assert_eq!(b("txt/e14.txt"), 24659);
}

fn a(s: &str) -> usize {
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
    //dbg!((min_row, max_row));
    //dbg!((min_col, max_col));
    let size = Point {
        row: max_row - min_row + 1,
        col: max_col - min_col + 1,
    };
    //dbg!(size);

    let offset = Point {
        row: min_row,
        col: min_col,
    };
    //dbg!(offset);

    let mut grid = Grid::init(size, '.');

    for path in &paths {
        for (&a, &b) in path.iter().zip(path.iter().skip(1)) {
            let a = a - offset;
            let b = b - offset;
            let line = Line { a, b };
            for point in line.points() {
                grid.insert(point, '#').unwrap();
            }
        }
    }

    //grid.print();

    let mut units = 0;
    'oob: loop {
        let sand_0 = Point { col: 500, row: 0 };
        let mut sand = sand_0 - offset;
        //println!("sand @ {sand_0} ({sand})");
        'next: loop {
            //println!("fall from {sand}");
            let attempts = [sand + Row(1), sand + Row(1) + Col(-1), sand + Row(1) + Col(1)];
            for attempt in attempts {
                //println!("attempt {attempt}");
                if let Some('.') = grid.get(attempt) {
                    //println!("fall to {attempt}");
                    sand = attempt;
                    continue 'next;
                } else if grid.get(attempt).is_none() {
                    //println!("oob at {attempt}");
                    break 'oob;
                }
            }
            //println!("resting @ {sand}");
            units += 1;
            grid.insert(sand, 'o').unwrap();
            //grid.print();
            continue 'oob;
        }
    }

    units
}

fn b(s: &str) -> usize {
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
    max_row += 1;
    //dbg!((min_row, max_row));
    //dbg!((min_col, max_col));
    let row_size = max_row - min_row + 1;
    let size = Point {
        row: max_row - min_row + 1,
        col: max_col - min_col + 1 + (row_size * 2),
    };
    //dbg!(size);

    let offset = Point {
        row: min_row,
        col: min_col - row_size,
    };
    //dbg!(offset);

    let mut grid = Grid::init(size, '.');

    for path in &paths {
        for (&a, &b) in path.iter().zip(path.iter().skip(1)) {
            let a = a - offset;
            let b = b - offset;
            let line = Line { a, b };
            for point in line.points() {
                grid.insert(point, '#').unwrap();
            }
        }
    }
    //for col in (min_col - offset.col)..=(max_col - offset) {
    //    grid.insert(Point { col, row: max_row }, '#');
    //}

    //dbg!(grid.len());
    //grid.print();

    let start_point = Point { col: 500, row: 0 } - offset;

    let mut units = 0;
    'oob: loop {
        let mut sand = start_point;
        //println!("Placing sand @ {} ({sand})", sand + offset);
        'next: loop {
            //println!("fall from {sand}");
            let attempts = [sand + Row(1), sand + Row(1) + Col(-1), sand + Row(1) + Col(1)];
            for attempt in attempts {
                //println!("attempt {attempt}");
                if let Some('.') = grid.get(attempt) {
                    //println!("fall to {attempt}");
                    sand = attempt;
                    continue 'next;
                } else if grid.get(attempt).is_none() {
                    if attempt.col < 0 { panic!("underflow") }
                    if attempt.col >= grid.max_point().col { panic!("overflow") }
                    //println!("oob at {attempt}");
                    //unreachable!();
                }
            }
            //println!("resting @ {sand}");
            units += 1;
            grid.insert(sand, 'o').unwrap();
            //grid.print();
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
                let mut i = s.split(",");
                let col = i.next().unwrap().parse().unwrap();
                let row = i.next().unwrap().parse().unwrap();
                Point { col, row }
            })
            .collect()
    })
    .collect()
}
