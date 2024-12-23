use aoc2024::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s08.txt"), 14);
    assert_eq!(a("txt/e08.txt"), 400);
    assert_eq!(b("txt/s08.txt"), 34);
    assert_eq!(b("txt/e08.txt"), 1280);
}

fn b(s: &str) -> usize {
    let grid = Grid::from_file(s);
    let mut antennae: BTreeMap<char, BTreeSet<Point>> = BTreeMap::new();
    let mut antinodes: BTreeSet<Point> = Default::default();
    for (p, v) in grid.points() {
        if v != '.' {
            assert!(antennae.entry(v).or_default().insert(p));
        }
    }
    for ps in antennae.values() {
        for perm in ps.iter().permutations(2) {
            add_antinodes(&grid, &mut antinodes, *perm[0], *perm[1]);
        }
    }
    antinodes.len()
}

fn add_antinodes(grid: &Grid<char>, antinodes: &mut BTreeSet<Point>, a: Point, b: Point) {
    for i in 0.. {
        let x = a + (a - b) * i;
        let y = a - (a - b) * i;
        if grid.contains(x) {
            antinodes.insert(x);
        }
        if grid.contains(y) {
            antinodes.insert(y);
        }
        if !(grid.contains(x) || grid.contains(y)) {
            return;
        }
    }
}

fn a(s: &str) -> usize {
    let grid = Grid::from_file(s);
    let mut antennae: BTreeMap<char, BTreeSet<Point>> = BTreeMap::new();
    let mut antinodes: BTreeSet<Point> = Default::default();
    for (p, v) in grid.points() {
        if v != '.' {
            assert!(antennae.entry(v).or_default().insert(p));
        }
    }
    for ps in antennae.values() {
        for perm in ps.iter().permutations(2) {
            add_antinode(&grid, &mut antinodes, *perm[0], *perm[1]);
            add_antinode(&grid, &mut antinodes, *perm[1], *perm[0]);
        }
    }
    antinodes.len()
}

fn add_antinode(grid: &Grid<char>, antinodes: &mut BTreeSet<Point>, a: Point, b: Point) {
    let x = a * 2 - b;
    if grid.contains(x) {
        antinodes.insert(x);
    }
}
