use aoc2023::*;
pub use std::collections::hash_map::Entry;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s10.txt"), 4);
    assert_eq!(a("txt/s10b.txt"), 8);
    assert_eq!(a("txt/e10.txt"), 6806);
    assert_eq!(b("txt/s10c.txt"), 4);
    assert_eq!(b("txt/s10d.txt"), 8);
    assert_eq!(b("txt/s10e.txt"), 10);
    assert_eq!(b("txt/e10.txt"), 449);
}

fn parse(s: &str) -> Grid<char> {
    Grid::new(read_parsed::<Chars>(s).map(|v| v.0).collect())
}

type Distance = usize;

fn a(s: &str) -> usize {
    let grid = parse(s);

    let mut visited: HashMap<Point, Distance> = Default::default();
    let start = grid.points().find(|&(_, a)| a == 'S').unwrap().0;
    let mut distance = 0;
    let mut currs: Vec<Point> = vec![start];

    'loo: loop {
        let mut new_currs = vec![];
        for curr in std::mem::take(&mut currs) {
            if let Entry::Vacant(e) = visited.entry(curr) {
                e.insert(distance);
                new_currs.extend(find_connections(&grid, curr, grid.get(curr).unwrap()));
            }
        }
        if !new_currs.is_empty() {
            distance += 1;
            currs = new_currs;
        } else {
            break 'loo;
        }
    }

    visited.values().copied().max().unwrap()
}

fn find_connections(grid: &Grid<char>, point: Point, curr: char) -> Vec<Point> {
    grid.straight_neighbors(point)
        .filter(|&(dir, _, v)| {
            (match dir {
                Direction::N => ['|', 'L', 'J', 'S'],
                Direction::E => ['-', 'L', 'F', 'S'],
                Direction::S => ['|', '7', 'F', 'S'],
                Direction::W => ['-', 'J', '7', 'S'],
                _ => unreachable!(),
            })
            .contains(&curr)
                && (match dir {
                    Direction::N => ['|', '7', 'F', 'S'],
                    Direction::E => ['-', 'J', '7', 'S'],
                    Direction::S => ['|', 'L', 'J', 'S'],
                    Direction::W => ['-', 'L', 'F', 'S'],
                    _ => unreachable!(),
                })
                .contains(&v)
        })
        .map(|v| v.1)
        .collect()
}

fn find_loop_tiles(grid: &Grid<char>, start: Point) -> Vec<Point> {
    let mut visited: Vec<Point> = Default::default();
    let mut currs: Option<Point> = Some(start);

    while let Some(curr) = currs {
        let mut new_currs = None;
        if !visited.contains(&curr) {
            visited.push(curr);
            let connections = find_connections(grid, curr, grid.get(curr).unwrap());
            'f: for connection in connections {
                if !visited.contains(&connection) {
                    new_currs = Some(connection);
                    break 'f;
                }
            }
        }
        currs = new_currs;
    }
    visited
}

fn next_to_two_connecting_tiles(new_grid: &Grid<char>, loop_tiles: &[Point], point: Point) -> bool {
    let neig_pipes: Vec<Point> = new_grid
        .straight_neighbors(point)
        .filter(|&(_dir, _p, v)| v == 'x')
        .map(|v| v.1)
        .collect();
    assert!(neig_pipes.len() <= 2);
    if neig_pipes.len() == 2 {
        let neig_a = neig_pipes[0];
        let neig_b = neig_pipes[1];
        if neig_a.row == neig_b.row || neig_a.col == neig_b.col {
            let a_i = loop_tiles.iter().position(|&p| p == neig_a).unwrap();
            let b_i = loop_tiles.iter().position(|&p| p == neig_b).unwrap();
            let (min, max) = if a_i < b_i { (a_i, b_i) } else { (b_i, a_i) };
            max - min == 1 || (min == 0 && max == loop_tiles.len() - 1)
        } else {
            false
        }
    } else {
        false
    }
}

fn b(s: &str) -> usize {
    let grid = parse(s);
    let start = grid.points().find(|&(_, a)| a == 'S').unwrap().0;
    let loop_tiles = find_loop_tiles(&grid, start);
    let new_loop_tiles: Vec<Point> = loop_tiles.iter().copied().map(|p| p * 2).collect();

    let mut new_grid = Grid::init(grid.len() * 2 - Point { row: 1, col: 1 }, '?');
    let new_grid_points: Vec<Point> = new_grid.points().map(|v| v.0).collect();

    for &point in &new_grid_points {
        if point.row % 2 == 0 && point.col % 2 == 0 {
            let orig = point / 2;
            let new_val = if loop_tiles.contains(&orig) { 'x' } else { '.' };
            new_grid.insert(point, new_val).unwrap();
        }
    }

    for &point in &new_grid_points {
        if point.row % 2 != 0 || point.col % 2 != 0 {
            let is_todo = new_grid.get(point).unwrap() == '?';
            if is_todo {
                let c = if next_to_two_connecting_tiles(&new_grid, &new_loop_tiles, point) {
                    'y'
                } else {
                    '_'
                };
                *new_grid.get_mut(point).unwrap() = c;
            }
        }
    }

    let to_checks: Vec<Point> = new_grid
        .points()
        .filter(|&v| v.1 == '.')
        .map(|v| v.0)
        .collect();
    let mut visited: HashSet<Point> = Default::default();

    for point in to_checks {
        let mut group: HashSet<Point> = Default::default();
        let mut next_in_group = vec![point];
        while !next_in_group.is_empty() {
            let mut new_next_in_group = vec![];
            for point in std::mem::take(&mut next_in_group) {
                if visited.contains(&point) {
                    continue;
                }
                group.insert(point);
                visited.insert(point);
                for (_, neig_p, neig_v) in new_grid.straight_neighbors(point) {
                    if !visited.contains(&neig_p) && ['_', '.'].contains(&neig_v) {
                        new_next_in_group.push(neig_p);
                    }
                }
            }
            next_in_group = new_next_in_group;
        }

        let edge = group.iter().any(|&p| new_grid.is_edge(p));
        for p in group {
            if p.row % 2 == 0 && p.col % 2 == 0 {
                new_grid.insert(p, if edge { 'O' } else { 'I' }).unwrap();
            }
        }
    }

    new_grid.points().filter(|&v| v.1 == 'I').count()
}
