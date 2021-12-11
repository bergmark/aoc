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
