use aoc2021::*;

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

enum Either<A, B> {
    Left(A),
    Right(B),
}

fn a(s: &str, iterations: usize) -> usize {
    match sol(s, Some(iterations)) {
        Either::Right(r) => r,
        _ => unreachable!(),
    }
}

fn b(s: &str) -> usize {
    match sol(s, None) {
        Either::Left(l) => l,
        _ => unreachable!(),
    }
}

fn sol(s: &str, iterations: Option<usize>) -> Either<usize, usize> {
    let mut grid = Grid::new(read_parsed::<Row>(s).map(|r| r.0).collect());
    let points: Vec<Point> = grid.points().map(|(p, _)| p).collect();
    let mut flashes = 0;

    grid.print();
    let mut i = 0;
    while iterations.map(|j| i < j).unwrap_or(true) {
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

        let to_check = JobQueueSet::new(
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
            let val = grid.get_unwrap(point);
            if val > 9 && !has_flashed.contains(&point) {
                has_flashed.insert(point);

                Direction::ALL
                    .into_iter()
                    .filter_map(|dir| {
                        let neighbor = point + dir.increment();
                        if grid.contains(neighbor) {
                            grid.insert(neighbor, grid.get_unwrap(neighbor) + 1);
                            if has_flashed.contains(&neighbor) {
                                None
                            } else {
                                Some(neighbor)
                            }
                        } else {
                            None
                        }
                    })
                    .collect()
            } else {
                Default::default()
            }
        });

        println!();

        for point in &points {
            let point = *point;
            let val = grid.get_unwrap(point);
            if val > 9 {
                flashes += 1;
                //println!("flash {}", point);
                grid.insert(point, 0);
            }
        }
        println!("\nAfter step {}:", i);
        grid.print();

        println!("{} flashes", has_flashed.len());
        if has_flashed.len() == 100 {
            return Either::Left(i);
        }
    }

    Either::Right(flashes)
}
