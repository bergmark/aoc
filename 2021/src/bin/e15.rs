use aoc2021::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s15.txt"), 40);
    assert_eq!(a("txt/e15.txt"), 717);
}

#[derive(Debug, Copy, Clone)]
struct Cell {
    cost: u32,
    total_cost: Option<u32>,
}

impl GridDisplay for Cell {
    fn grid_display(&self) -> String {
        format!(
            "{}/{:02}",
            self.cost,
            self.total_cost
                .map_or_else(|| "?".to_owned(), |v| v.to_string())
        )
    }
}

impl Cell {
    fn new(cost: u32) -> Cell {
        Cell {
            cost,
            total_cost: None,
        }
    }

    fn set_total_cost(&mut self, v: u32) -> bool {
        match self.total_cost {
            None => {
                self.total_cost = Some(v + self.cost);
                true
            }
            Some(old) => {
                if v + self.cost < old {
                    self.total_cost = Some(std::cmp::min(old, v + self.cost));
                    true
                } else {
                    false
                }
            }
        }
    }
}

fn a(s: &str) -> u32 {
    let mut grid: Grid<Cell> = Grid::new(
        read_parsed::<String>(s)
            .map(|l| {
                l.chars()
                    .map(|c| Cell::new(c.to_digit(10).unwrap()))
                    .collect()
            })
            .collect(),
    );

    let start = Point { row: 0, col: 0 };

    {
        let mut v = grid.get_unwrap(start);
        v.cost = 0;
        v.total_cost = Some(0);
        grid.insert(start, v);
    }

    grid.print();

    let mut jobs = BTreeSet::from([start]);
    let mut jobs_len = jobs.len();

    while jobs_len > 0 {
        let curr_jobs = std::mem::take(&mut jobs);
        println!("jobs: {}", curr_jobs.iter().map(|s| s.to_string()).join(", "));
        for point in curr_jobs {
            let curr = grid.get_unwrap(point);
            for dir in Direction::STRAIGHT {
                let neighbor_p = point + dir.increment();
                if grid.contains(neighbor_p) {
                    let mut neighbor_v = grid.get_unwrap(neighbor_p);
                    if neighbor_v.set_total_cost(curr.total_cost.unwrap()) {
                        grid.insert(neighbor_p, neighbor_v);
                        jobs.insert(neighbor_p);
                    }
                }
            }
        }
        jobs_len = jobs.len();
    }

    grid.print();

    grid.get_unwrap(grid.max_point()).total_cost.unwrap()
}
