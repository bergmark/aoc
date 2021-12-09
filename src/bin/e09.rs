use aoc2021::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s09.txt"), 15);
    assert_eq!(a("txt/e09.txt"), 439);
    assert_eq!(b("txt/s09.txt"), 1134);
    assert_eq!(b("txt/e09.txt"), 900900);
}

struct Row(Vec<u32>);

impl FromStr for Row {
    type Err = ();
    fn from_str(s: &str) -> Result<Row, ()> {
        Ok(Row(s.chars().map(|c| c.to_digit(10).unwrap()).collect()))
    }
}

fn a(s: &str) -> u32 {
    let grid = Grid::new(read_parsed::<Row>(s).map(|r| r.0).collect());

    let mut low_points = vec![];
    for (point, v) in grid.points() {
        if grid.neighbors(point).into_iter().all(|(_p, n)| n > v) {
            low_points.push(v);
        }
    }

    low_points.iter().map(|l| l + 1).sum()
}

fn b(s: &str) -> usize {
    let grid = Grid::new(read_parsed::<Row>(s).map(|r| r.0).collect());

    let low_points: Vec<_> = grid.points().into_iter().filter_map(|(point, v)| {
        if grid.neighbors(point).into_iter().all(|(_p, n)| n > v) {
            Some((point, v))
        } else {
            None
        }
    }).collect();

    let mut basins: Vec<_> = vec![];

    for (low_point, low_val) in low_points.into_iter() {
        let mut basin: BTreeMap<Point, u32> = BTreeMap::new();
        basin.insert(low_point, low_val);
        let mut checked: BTreeSet<Point> = BTreeSet::new();
        let mut check: BTreeSet<(Point, u32)> = BTreeSet::new();
        check.insert((low_point, low_val));
        let mut check_len = 1;

        while check_len != 0 {
            let mut new_check = BTreeSet::new();
            for (point, low) in &check {
                let point = *point;
                let low = *low;
                let val = grid.get_unwrap(point);
                if val != 9 && val >= low {
                    basin.insert(point, val);
                    checked.insert(point);
                    for (neighbor_point, _) in grid.neighbors(point).into_iter() {
                        new_check.insert((neighbor_point, val));
                    }
                }
            }
            check = new_check;
            for checked in &checked {
                check = check.into_iter().filter(|(p, _)| p != checked).collect();
            }
            check_len = check.len();
        }

        basins.push(basin);
    }

    let mut basins: Vec<usize> = basins.into_iter().map(|basin| basin.len()).collect();
    basins.sort();
    basins.into_iter().rev().take(3).product()
}
