use aoc2023::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s03.txt"), 4361);
    assert_eq!(a("txt/e03.txt"), 532331);
    assert_eq!(b("txt/s03b.txt"), 467835);
    assert_eq!(b("txt/e03.txt"), 82301120);
}

fn a(s: &str) -> usize {
    let v: Vec<Vec<char>> = read_parsed::<String>(s)
        .map(|s| s.chars().collect())
        .collect();
    let grid = Grid::new(v);

    grid.points()
        .filter(|&(_, v)| v.is_digit(10))
        .filter(|&(point, _)| {
            grid.get(point + Direction::W)
                .map_or(true, |v| !v.is_digit(10))
        })
        .map(|(point, _)| part_number(&grid, point))
        .sum::<usize>()
}

fn part_number(grid: &Grid<char>, start: Point) -> usize {
    let digits: Vec<(Point, usize)> = {
        let mut digits: Vec<(Point, usize)> = vec![];
        let mut p = start;
        'loo: loop {
            if let Some(v) = grid.get(p) {
                if let Some(v) = v.to_digit(10) {
                    digits.push((p, v as usize));
                } else {
                    break 'loo;
                }
            } else {
                break 'loo;
            }
            p += Direction::E;
        }
        digits
    };

    let is_part_number: bool = digits.iter().any(|&(p, _)| {
        grid.all_neighbors(p)
            .any(|(_, v)| !v.is_digit(10) && v != '.')
    });

    if is_part_number {
        digits
            .iter()
            .rev()
            .enumerate()
            .map(|(i, (_, n))| n * 10_usize.pow(i as u32))
            .sum::<usize>()
    } else {
        0
    }
}

fn b(s: &str) -> usize {
    let v: Vec<Vec<char>> = read_parsed::<String>(s)
        .map(|s| s.chars().collect())
        .collect();
    let grid = Grid::new(v);

    grid.points()
        .filter(|&(_, v)| v == '*')
        .map(|(point, _)| {
            let adj = adjacent_numbers(&grid, point);
            if adj.len() == 2 {
                adj.into_iter().product::<usize>()
            } else {
                0
            }
        })
        .sum()
}

fn adjacent_numbers(grid: &Grid<char>, gear: Point) -> Vec<usize> {
    let mut numbers: Vec<usize> = vec![];
    let mut starts: BTreeSet<Point> = BTreeSet::new();

    for (p, v) in grid.all_neighbors(gear) {
        if v.is_digit(10) {
            let first_digit = first_digit_of_number(grid, p);
            if !starts.contains(&first_digit) {
                starts.insert(first_digit);
                numbers.push(number_starting_at(grid, first_digit));
            }
        }
    }

    numbers
}

fn first_digit_of_number(grid: &Grid<char>, digit_point: Point) -> Point {
    let mut first_digit = digit_point;
    loop {
        let pw = first_digit + Direction::W;
        if let Some(v) = grid.get(pw) {
            if v.is_digit(10) {
                first_digit = pw;
            } else {
                return first_digit;
            }
        } else {
            return first_digit;
        }
    }
}

fn number_starting_at(grid: &Grid<char>, start: Point) -> usize {
    let numbers: Vec<(Point, usize)> = {
        let mut numbers: Vec<(Point, usize)> = vec![];
        let mut p = start;
        'loo: loop {
            if let Some(v) = grid.get(p) {
                if let Some(v) = v.to_digit(10) {
                    numbers.push((p, v as usize));
                } else {
                    break 'loo;
                }
            } else {
                break 'loo;
            }
            p += Direction::E;
        }
        numbers
    };

    numbers
        .iter()
        .rev()
        .enumerate()
        .map(|(i, (_, n))| n * 10_usize.pow(i as u32))
        .sum::<usize>()
}
