use aoc2022::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s08.txt"), 21);
    assert_eq!(a("txt/e08.txt"), 1779);
    assert_eq!(b("txt/s08.txt"), 8);
    assert_eq!(b("txt/e08.txt"), 172224);
}

fn a(s: &str) -> usize {
    let grid: Grid<i64> = Grid {
        rows: read_lines(s)
            .map(|s| {
                s.unwrap()
                    .chars()
                    .map(|c| c.to_string().parse::<i64>().unwrap())
                    .collect::<Vec<_>>()
            })
            .collect(),
    };
    let grid = grid.rows;

    let mut visible: BTreeSet<(usize, usize)> = Default::default();

    for i in 0..grid.len() {
        for j in 0..grid[0].len() {
            if is_visible(&grid, i, j) {
                visible.insert((i, j));
            }
        }
    }

    visible.len()
}

fn is_visible(grid: &Vec<Vec<i64>>, i: usize, j: usize) -> bool {
    let v = grid[i][j];
    if i == 0 || j == 0 || i == grid.len() - 1 || j == grid[0].len() - 1 {
        return true;
    }
    let a = (|| {
        for i0 in 0..i {
            if grid[i0][j] >= v {
                return false;
            }
        }
        true
    })();
    let b = (|| {
        for j0 in 0..j {
            if grid[i][j0] >= v {
                return false;
            }
        }
        true
    })();
    let c = (|| {
        for i0 in (i + 1)..grid.len() {
            if grid[i0][j] >= v {
                return false;
            }
        }
        true
    })();
    let d = (|| {
        for j0 in (j + 1)..grid[0].len() {
            if grid[i][j0] >= v {
                return false;
            } else {
            }
        }
        true
    })();
    a || b || c || d
}

fn b(s: &str) -> i64 {
    let grid: Grid<i64> = Grid {
        rows: read_lines(s)
            .map(|s| {
                s.unwrap()
                    .chars()
                    .map(|c| c.to_string().parse::<i64>().unwrap())
                    .collect::<Vec<_>>()
            })
            .collect(),
    };
    let grid = grid.rows;

    let mut max_score = 0;
    for i in 0..grid.len() {
        for j in 0..grid[0].len() {
            max_score = std::cmp::max(max_score, score(&grid, i, j));
        }
    }

    max_score
}

fn score(grid: &Vec<Vec<i64>>, i: usize, j: usize) -> i64 {
    if i == 0 || j == 0 || i == grid.len() - 1 || j == grid[0].len() - 1 {
        return 0;
    }
    let v = grid[i][j];
    let a = (|| {
        let mut count = 0;
        let max = v;
        for i0 in (0..i).rev() {
            let v0 = grid[i0][j];
            if v0 >= max {
                return count + 1;
            } else {
                count += 1;
            }
        }
        count
    })();
    let b = (|| {
        let mut count = 0;
        let max = v;
        for i0 in (i + 1)..grid.len() {
            let v0 = grid[i0][j];
            if v0 >= max {
                return count + 1;
            } else {
                count += 1;
            }
        }
        count
    })();
    let c = (|| {
        let mut count = 0;
        let max = v;
        for j0 in (0..j).rev() {
            let v0 = grid[i][j0];
            if v0 >= max {
                return count + 1;
            } else {
                count += 1;
            }
        }
        count
    })();
    let d = (|| {
        let mut count = 0;
        let max = v;
        for j0 in (j + 1)..grid[0].len() {
            let v0 = grid[i][j0];
            if v0 >= max {
                return count + 1;
            } else {
                count += 1;
            }
        }
        count
    })();
    a * b * c * d
}

#[derive(Debug, Clone)]
struct Row {}

impl FromStr for Row {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, ()> {
        let _ = s;
        if let Ok(_cap) = Captures::new(regex!("^.+$"), s) {
            Ok(Row {})
        } else {
            Ok(Row {})
        }
    }
}
