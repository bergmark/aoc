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
    assert_eq!(b("txt/s11.txt"), 195);
    assert_eq!(b("txt/e11.txt"), 298);
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

        struct State<'a> {
            grid: &'a mut Grid<u32>,
            has_flashed: BTreeSet<Point>,
        }

        let to_check = JobQueueSet::from_iterator(
            State {
                grid: &mut grid,
                has_flashed: BTreeSet::new(),
            },
            points.iter().copied(),
        );

        to_check.run(|point: Point, state: &mut State| {
            let State { grid, has_flashed } = state;

            let mut new_to_check: BTreeSet<Point> = BTreeSet::new();

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

            new_to_check
        });

        println!();

        for point in &points {
            let point = *point;
            let val = grid.get_unwrap(point);
            if val > 9 {
                println!("flash {}", point);
                flashes += 1;
                grid.insert(point, 0);
            }
        }
        println!("\nAfter step {}:", i);
        print_grid(&grid);
    }

    flashes
}

fn b(s: &str) -> usize {
    let mut grid = Grid::new(read_parsed::<Row>(s).map(|r| r.0).collect());
    let points: Vec<Point> = grid.points().map(|(p, _)| p).collect();

    print_grid(&grid);
    let mut i = 0;
    loop {
        i += 1;
        println!("=== step {}", i);
        println!();

        for point in &points {
            let point = *point;
            let new = grid.get_unwrap(point) + 1;
            grid.insert(point, new);
        }

        struct State<'a> {
            grid: &'a mut Grid<u32>,
            has_flashed: BTreeSet<Point>,
        }

        let to_check = JobQueueSet::from_iterator(
            State {
                grid: &mut grid,
                has_flashed: BTreeSet::new(),
            },
            points.iter().copied(),
        );

        let State {
            grid: _,
            has_flashed,
        } = to_check.run(|point: Point, state: &mut State| {
            let State { grid, has_flashed } = state;
            let mut new_to_check = BTreeSet::new();
            let val = grid.get_unwrap(point);
            if val > 9 && !has_flashed.contains(&point) {
                has_flashed.insert(point);
                for dir in Direction::ALL {
                    let neighbor = point + dir.increment();
                    if grid.contains(neighbor) {
                        grid.insert(neighbor, grid.get_unwrap(neighbor) + 1);
                        if !has_flashed.contains(&neighbor) {
                            new_to_check.insert(neighbor);
                        }
                    }
                }
            }
            new_to_check
        });

        println!();

        for point in &points {
            let point = *point;
            let val = grid.get_unwrap(point);
            if val > 9 {
                //println!("flash {}", point);
                grid.insert(point, 0);
            }
        }
        println!("\nAfter step {}:", i);
        print_grid(&grid);

        println!("{} flashes", has_flashed.len());
        if has_flashed.len() == 100 {
            return i;
        }
    }
}
