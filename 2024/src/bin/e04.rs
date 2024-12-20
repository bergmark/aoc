use aoc2024::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s04.txt"), 18);
    assert_eq!(a("txt/e04.txt"), 2545);
    assert_eq!(b("txt/s04.txt"), 9);
    assert_eq!(b("txt/e04.txt"), 1886);
}

fn a(s: &str) -> i64 {
    let grid = Grid::new(read_parsed::<Chars>(s).map(|v| v.0).collect());
    let mut count = 0;
    for (point, v) in grid.points() {
        if v == 'X' {
            for dir in Direction::ALL {
                if grid
                    .line_of_sight(point, dir)
                    .take(3)
                    .map(|v| v.1)
                    .collect::<String>()
                    == "MAS"
                {
                    count += 1;
                }
            }
        }
    }
    count
}

fn b(s: &str) -> usize {
    let grid = Grid::new(read_parsed::<Chars>(s).map(|v| v.0).collect());
    grid.points()
        .filter(|(point, v)| *v == 'A' && is_mas(&grid, *point).unwrap_or(false))
        .count()
}

fn is_mas(grid: &Grid<char>, point: Point) -> Option<bool> {
    let nw = grid.get(point + Direction::NW)?;
    let ne = grid.get(point + Direction::NE)?;
    let se = grid.get(point + Direction::SE)?;
    let sw = grid.get(point + Direction::SW)?;
    let a = (nw, se) == ('S', 'M') || (nw, se) == ('M', 'S');
    let b = (sw, ne) == ('S', 'M') || (sw, ne) == ('M', 'S');
    Some(a && b)
}
