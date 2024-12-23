use aoc2024::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s10.txt"), 36);
    assert_eq!(a("txt/e10.txt"), 552);
}

fn a(s: &str) -> usize {
    let grid = Grid::digs_from_file(s);
    grid.points()
        .filter(|v| v.1 == 0)
        .map(|(trailhead, _)| trailhead_score_start(&grid, trailhead))
        .sum()
}

fn trailhead_score_start(grid: &Grid<u32>, p: Point) -> usize {
    let mut visited: BTreeSet<Point> = Default::default();
    trailhead_score(grid, &mut visited, p)
}

fn trailhead_score(grid: &Grid<u32>, visited: &mut BTreeSet<Point>, p: Point) -> usize {
    if !visited.insert(p) {
        return 0;
    }
    let v = grid.get_unwrap(p);
    if v == 9 {
        return 1;
    }

    let mut hits = 0;
    for (_, p2, v2) in grid.straight_neighbors(p) {
        if v + 1 == v2 {
            hits += trailhead_score(grid, visited, p2);
        }
    }
    hits
}
