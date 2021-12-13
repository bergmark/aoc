use aoc2021::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s13.txt"), 17);
    assert_eq!(a("txt/e13.txt"), 814);
    assert_eq!(b("txt/e13.txt"), include_str!("../../txt/e13.out.txt"));
}

#[derive(Copy, Debug, Clone)]
enum Line {
    Point(Point),
    Fold(Fold),
    Empty,
}

#[derive(Copy, Debug, Clone)]
struct Fold {
    xory: XorY,
    val: i64,
}

#[derive(Copy, Debug, Clone)]
enum XorY {
    X,
    Y,
}

fn parse_point(s: &str) -> Result<Point, ()> {
    let (col, row) = split2(s, ",")?;
    Ok(Point {
        row: row.parse().unwrap(),
        col: col.parse().unwrap(),
    })
}

impl FromStr for Line {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, ()> {
        if s.is_empty() {
            Ok(Line::Empty)
        } else if s.chars().next().map_or(false, |c| c.is_digit(10)) {
            Ok(Line::Point(parse_point(s)?))
        } else {
            let s: Vec<_> = s.split(" ").collect();
            assert_eq!(s[0], "fold");
            assert_eq!(s[1], "along");
            assert_eq!(s.len(), 3);
            let (xy, v) = split2(s[2], "=")?;
            let v = v.parse().unwrap();
            if xy == "x" {
                Ok(Line::Fold(Fold {
                    xory: XorY::X,
                    val: v,
                }))
            } else if xy == "y" {
                Ok(Line::Fold(Fold {
                    xory: XorY::Y,
                    val: v,
                }))
            } else {
                unreachable!()
            }
        }
    }
}

fn a(s: &str) -> usize {
    let lines: Vec<_> = read_parsed::<Line>(s).collect();

    let mut points: BTreeSet<Point> = BTreeSet::new();
    let mut folds: Vec<Fold> = vec![];
    let mut rows = 0;
    let mut cols = 0;
    for line in lines {
        match line {
            Line::Point(p) => {
                rows = std::cmp::max(p.row + 1, rows);
                cols = std::cmp::max(p.col + 1, cols);
                points.insert(p);
            }
            Line::Fold(f) => folds.push(f),
            Line::Empty => {}
        }
    }

    for Fold { xory, val } in folds.into_iter().take(1) {
        match xory {
            XorY::Y => {
                let mut new_points: BTreeSet<Point> = Default::default();
                for point in points {
                    if point.row == val {
                        // noop
                    } else if point.row < val {
                        new_points.insert(point);
                    } else {
                        let new_point = Point {
                            row: val - (point.row - val),
                            col: point.col,
                        };
                        new_points.insert(new_point);
                    }
                }
                points = new_points;
            }
            XorY::X => {
                let mut new_points: BTreeSet<Point> = Default::default();
                for point in points {
                    if point.col == val {
                        // noop
                    } else if point.col < val {
                        new_points.insert(point);
                    } else {
                        let new_point = Point {
                            row: point.row,
                            col: val - (point.col - val),
                        };
                        new_points.insert(new_point);
                    }
                }
                points = new_points;
            }
        }
    }

    points.len()
}

fn b(s: &str) -> String {
    let mut points: BTreeSet<Point> = BTreeSet::new();
    let mut folds: Vec<Fold> = vec![];
    let mut size = Point { row: 0, col: 0 };
    for line in read_parsed::<Line>(s) {
        match line {
            Line::Point(p) => {
                size = size.max_both(Point {
                    row: p.row + 1,
                    col: p.col + 1,
                });
                points.insert(p);
            }
            Line::Fold(f) => folds.push(f),
            Line::Empty => {}
        }
    }

    struct State {
        points: BTreeSet<Point>,
        size: Point,
    }

    let jobs: JobQueue<Fold, State> = JobQueue::new(State { points, size }, folds);
    let State { points, size } = jobs.run(|Fold { xory, val }, state: &mut State| {
        let State { points, size } = state;
        let mut new_points: BTreeSet<Point> = Default::default();
        let new_size = match xory {
            XorY::Y => {
                for &point in points.iter() {
                    if point.row == val {
                        // noop
                    } else if point.row < val {
                        new_points.insert(point);
                    } else {
                        let new_point = Point {
                            row: val - (point.row - val),
                            col: point.col,
                        };
                        new_points.insert(new_point);
                    }
                }
                Point {
                    row: (size.row - 1) / 2,
                    col: size.col,
                }
            }
            XorY::X => {
                for &point in points.iter() {
                    if point.col == val {
                        // noop
                    } else if point.col < val {
                        new_points.insert(point);
                    } else {
                        let new_point = Point {
                            row: point.row,
                            col: val - (point.col - val),
                        };
                        new_points.insert(new_point);
                    }
                }
                Point {
                    col: (size.col - 1) / 2,
                    row: size.col,
                }
            }
        };
        state.points = new_points;
        state.size = new_size;
        vec![]
    });

    display_grid(&points, size)
}

fn display_grid(points: &BTreeSet<Point>, size: Point) -> String {
    let mut s = "".to_owned();
    for row in 0..size.row {
        for col in 0..size.col {
            if points.contains(&Point { row, col }) {
                s.push('#');
            } else {
                s.push('.');
            }
        }
        s.push('\n');
    }
    s
}
