use aoc2021::{grid::print_grid, *};

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s11b.txt", 2), 9);
    assert_eq!(a("txt/s11.txt", 2), 35);
    assert_eq!(a("txt/s11.txt", 100), 1656);
    assert_eq!(a("txt/e11.txt", 100), 1721);
    //assert_eq!(b("txt/s11.txt"), 0);
    //assert_eq!(b("txt/e11.txt"), 0);
}

struct Row(Vec<u32>);

impl FromStr for Row {
    type Err = ();
    fn from_str(s: &str) -> Result<Row, ()> {
        Ok(Row(s.chars().map(|c| c.to_digit(10).unwrap()).collect()))
    }
}

fn a(s: &str, iterations: usize) -> usize {
    let mut grid = Grid::new(read_parsed::<Row>(s).map(|r| r.0).collect());
    let points: Vec<Point> = grid.points().map(|(p, _)| p).collect();
    let mut flashes = 0; // BTreeSet::new();

    print_grid(&grid);
    for i in 1..=iterations {
        println!("=== step {}", i);
        println!();

        for point in &points {
            let point = *point;
            let new = grid.get_unwrap(point) + 1;
            grid.insert(point, new);
        }

        let mut to_check: BTreeSet<Point> = points.iter().copied().collect();
        let mut to_check_len = to_check.len();
        let mut has_flashed = BTreeSet::new();

        while to_check_len != 0 {

            let mut new_to_check: BTreeSet<Point> = BTreeSet::new();

            for point in &to_check {
                let point = *point;
                let val = grid.get_unwrap(point);
                if val > 9 && !has_flashed.contains(&point) {
                    has_flashed.insert(point);
                    let mut neighbors = vec![];
                    for dir in Direction::ALL {
                        let neighbor = point + dir.increment();
                        if grid.contains(neighbor) {
                            neighbors.push(neighbor);
                            grid.insert(neighbor, grid.get_unwrap(neighbor) + 1);
                            if !has_flashed.contains(&neighbor) {
                                new_to_check.insert(neighbor);
                            }
                        }
                    }
                    println!(
                        "{} -> {}",
                        point,
                        neighbors.into_iter().map(|x| format!("{}", x)).join("; ")
                    );
                }
            }

            to_check = new_to_check;
            to_check_len = to_check.len();
        }

        println!();

        for point in &points {
            let point = *point;
            let val = grid.get_unwrap(point);
            if val > 9 {
                println!("flash {}", point);
                flashes += 1;
                //flashes_it.insert(point);
                grid.insert(point, 0);
            }
        }
        println!("\nAfter step {}:", i);
        print_grid(&grid);
    }

    flashes

    //    let basins = low_points(&grid).map(|(low_point, low_val)| {
    //        let mut basin: BTreeMap<Point, u32> = BTreeMap::from([(low_point, low_val)]);
    //        let mut checked: BTreeSet<Point> = BTreeSet::new();
    //        let mut check: BTreeSet<(Point, u32)> = BTreeSet::from([(low_point, low_val)]);
    //        let mut check_len = 1;
    //
    //        while check_len != 0 {
    //            let mut new_check = BTreeSet::new();
    //            for (point, low) in check {
    //                let val = grid.get_unwrap(point);
    //                if val != 9 && val >= low {
    //                    basin.insert(point, val);
    //                    for (neighbor_point, _) in grid.straight_neighbors(point) {
    //                        new_check.insert((neighbor_point, val));
    //                    }
    //                }
    //                checked.insert(point);
    //            }
    //            check = new_check;
    //            for checked_ in &checked {
    //                check = check.into_iter().filter(|(p, _)| p != checked_).collect();
    //            }
    //            check_len = check.len();
    //        }
    //
    //        basin
    //    });
    //
    //    let mut basins: Vec<usize> = basins.map(|basin| basin.len()).collect();
    //    basins.sort_unstable();
    //    basins.into_iter().rev().take(3).product()
}
