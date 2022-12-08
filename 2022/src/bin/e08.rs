use aoc2022::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s08.txt"), 21);
    assert_eq!(a("txt/e08.txt"), 1779);
    assert_eq!(b("txt/s08.txt"), 8);
    assert_eq!(b("txt/e08.txt"), 172224);
}

fn a(s: &str) -> usize {
    let grid = parse(s);
    grid.points()
        .filter(|&(point, v)| is_visible(&grid, point, v))
        .count()
}
fn is_visible(grid: &Grid<i64>, point: Point, v: i64) -> bool {
    Direction::STRAIGHT
        .iter()
        .any(|&dir| grid.path_to_point(point, dir).all(|(_, v0)| v0 < v))
}

fn b(s: &str) -> usize {
    let grid = parse(s);
    grid.points()
        .map(|(point, v)| score(&grid, point, v))
        .max()
        .unwrap()
}

fn score(grid: &Grid<i64>, point: Point, v: i64) -> usize {
    Direction::STRAIGHT
        .iter()
        .map(|&dir| {
            let mut count = 0;
            for (_, v0) in grid.line_of_sight(point, dir) {
                if v0 >= v {
                    return count + 1;
                } else {
                    count += 1;
                }
            }
            count
        })
        .product()
}

fn parse(s: &str) -> Grid<i64> {
    Grid {
        rows: read_lines(s)
            .map(|s| {
                s.unwrap()
                    .chars()
                    .map(|c| c.to_string().parse::<i64>().unwrap())
                    .collect::<Vec<_>>()
            })
            .collect(),
    }
}
