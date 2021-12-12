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

    low_points(&grid).map(|(_, l)| l + 1).sum()
}

fn b(s: &str) -> usize {
    let grid = Grid::new(read_parsed::<Row>(s).map(|r| r.0).collect());

    let basins = low_points(&grid).map(|(low_point, low_val)| {
        let basin: BTreeMap<Point, u32> = BTreeMap::from([(low_point, low_val)]);
        let checked: BTreeSet<Point> = BTreeSet::new();
        struct State {
            basin: BTreeMap<Point, u32>,
            checked: BTreeSet<Point>,
        }
        let check = JobQueueSet::from_iterator(State { basin, checked }, [(low_point, low_val)]);

        let State { basin, checked: _ } = check.run(|(point, low), state| {
            let State { basin, checked } = state;

            let mut new_check = BTreeSet::new();
            let val = grid.get_unwrap(point);
            if val != 9 && val >= low {
                basin.insert(point, val);
                for (neighbor_point, _) in grid.straight_neighbors(point) {
                    new_check.insert((neighbor_point, val));
                }
            }
            checked.insert(point);
            new_check
                .into_iter()
                .filter(|(p, _)| !checked.contains(p))
                .collect()
        });

        basin
    });

    let mut basins: Vec<usize> = basins.map(|basin| basin.len()).collect();
    basins.sort_unstable();
    basins.into_iter().rev().take(3).product()
}

fn low_points<A: Copy + PartialOrd>(grid: &Grid<A>) -> impl Iterator<Item = (Point, A)> + '_ {
    grid.points().filter_map(|(point, v)| {
        if grid.straight_neighbors(point).all(|(_p, n)| n > v) {
            Some((point, v))
        } else {
            None
        }
    })
}
