use std::collections::HashMap;
use std::collections::HashSet;
use std::fmt;
use std::fs;
use std::collections::hash_map::Entry;
use std::cmp;
use std::iter::FromIterator;

#[derive(Eq, PartialEq, Hash, Debug)]
enum Dir {
    R(i32),
    D(i32),
    U(i32),
    L(i32)
}

#[derive(Eq, PartialEq, Hash, Debug, Copy, Clone)]
pub struct Point {
    x: i32,
    y: i32
}


impl fmt::Display for Point {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{},{}", self.x, self.y)
    }
}

fn parse(s: &str) -> Dir {
    match s.split_at(1) {
        ("R", i) => Dir::R(i.parse().unwrap()),
        ("D", i) => Dir::D(i.parse().unwrap()),
        ("U", i) => Dir::U(i.parse().unwrap()),
        ("L", i) => Dir::L(i.parse().unwrap()),
        (s, i) => panic!("Couldn't parse {} {}", s, i)
    }
}

pub fn write(board: &mut HashMap<Point, HashMap<usize, usize>>, s: S, p: Point, log: bool) -> () {
    let e = board.entry(p).or_insert(HashMap::new());
    e.entry(s.color).and_modify(|e2| { *e2 = cmp::min(*e2, s.steps) }).or_insert(s.steps);
    if log || e.len() >= 2 {
       println!("write {}: {:?}", p, e);
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct S {
    color: usize,
    steps: usize
}

fn main() {
    let prog: Vec<Vec<Dir>> = fs::read_to_string("input.txt").expect("Could not read file").split("\n").map(|l|l.split(",").map(|s| parse(s)).collect()).collect();

    let mut board: HashMap<Point, HashMap<usize, usize>> = HashMap::new();

    for (color, x) in prog.iter().enumerate() {
        println!("START LINE");
        let mut p = Point { x : 0, y : 0 };
        let mut steps = 0;
        for dir in x {
            match dir {
                Dir::R(i) => {
                    println!("R({})", i);
                    for j in 0..*i {
                        steps += 1;
                        p = Point { x : p.x + 1, y : p.y };
                        write(&mut board, S { color: color, steps : steps }, p, j == 0 || j == (i-1))
                    }
                },
                Dir::L(i) => {
                    println!("L({})", i);
                    for j in 0..*i {
                        steps += 1;
                        p = Point { x : p.x - 1, y : p.y };
                        write(&mut board, S { color: color, steps: steps }, p, j == 0 || j == (i-1))
                    }
                },
                Dir::U(i) => {
                    println!("U({})", i);
                    for j in 0..*i {
                        steps += 1;
                        p = Point { y : p.y + 1, x : p.x };
                        write(&mut board, S{color:color,steps:steps}, p, j == 0 || j == (i-1))
                    }
                },
                Dir::D(i) => {
                    println!("D({})", i);
                    for j in 0..*i {
                        steps += 1;
                        p = Point { y : p.y - 1, x : p.x };
                        write(&mut board, S{color:color,steps:steps}, p, j == 0 || j == (i-1))
                    }
                }
            }
        }
        // println!("Board {:?}", board);
    }

    let mut min_distance: Option<usize> = None;
    for (p, hm) in board {
        let dist = hm.iter().map(|x|x.1).sum();
        let count = hm.len();
        if count >= 2 {
            println!("overlap: {} {} {}", p, count, dist);
            if &min_distance.map_or_else(|| std::usize::MAX, |t| t) > &dist {
                min_distance = Some(dist)
            }
        }
    }
    println!("manhat: {:?}", min_distance)
}

// 806
