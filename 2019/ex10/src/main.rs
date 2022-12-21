use float_cmp::*;
use num::integer::gcd;
use std::cmp::Ordering;
use std::collections::HashMap;
use std::collections::HashSet;
use std::env;
use std::fmt;
use std::fmt::Display;
use std::fs;
use std::f64::consts::PI;

fn read_file(fp: &str) -> String {
    fs::read_to_string(fp)
        .expect("Could not read file")
        .trim()
        .to_string()
}

fn main() {
    let res = part1();
    part2(&res.0, res.1);
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub enum Cell {
    Empty,
    Asteroid,
}

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
struct Point {
    x: i32,
    y: i32,
}
impl Point {
    fn add(&self, v: &Vector) -> Point {
        Point {
            x: self.x + v.x_len,
            y: self.y + v.y_len,
        }
    }

    fn with_origo(&self, origo: &Point) -> Point {
        assert!(self.x != origo.x || self.y != origo.y);
        Point {
            x: self.x - origo.x,
            y: self.y - origo.y
        }
    }
}
impl Display for Point {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "x={},y={}", self.x, self.y)
    }
}

fn point_to_polar(p: &Point) -> Polar {
    let x = p.x as f64;
    let y = p.y as f64;
    let radius = (x.powf(2.0) + y.powf(2.0)).sqrt();
    assert!(!approx_eq!(f64, radius, 0.0, epsilon = 0.0000001), format!("{}", p));
    let angle = y.atan2(x);
    let angle = if angle < 0.0 {
        angle + 2.0*PI
    } else { angle };
    assert!(angle >= 0.0);
    Polar {
        r: radius,
        p: angle,
    }
}

#[derive(Debug, Clone)]
struct Polar {
    r: f64,
    p: f64,
}

impl Polar {
    fn rotate90(&self) -> Polar {
        let p1 = self.p + PI;
        println!("p1 {} {:?}", p1, pcmp(p1, 2.0*PI));
        let p = match pcmp(p1, 2.0*PI) {
            Ordering::Greater | Ordering::Equal => p1 - 2.0*PI,
            _ => p1,
        };
        println!("p1 = {}, p = {}", p1, p);
        Polar {
            r: self.r,
            p
        }
    }
}

#[derive(Debug, PartialEq, Eq, Hash)]
struct Vector {
    x_len: i32,
    y_len: i32,
}
impl Vector {
    fn scale(&self, u: i32) -> Vector {
        Vector {
            x_len: self.x_len * u,
            y_len: self.y_len * u,
        }
    }

    fn is_zero(&self) -> bool {
        self.x_len == 0 && self.y_len == 0
    }

    fn can_downscale(&self) -> bool {
        let g = gcd(self.x_len, self.y_len);
        g >= 2
    }
}

impl Display for Vector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "x={},y={}", self.x_len, self.y_len)
    }
}

fn part1() -> (Point, HashSet<Point>) {
    assert!(Vector { x_len: 3, y_len: 3 }.can_downscale());
    assert!(Vector { x_len: 0, y_len: 3 }.can_downscale());
    assert!(Vector { x_len: 2, y_len: 4 }.can_downscale());
    assert!(Vector { x_len: 3, y_len: 6 }.can_downscale());

    assert!(pcmp(1.0, 2.0) == Ordering::Less);
    assert!(pcmp(1.0, 1.0) == Ordering::Equal);
    assert!(pcmp(1.0, -1.0) == Ordering::Greater);

    let args = env::args().collect::<Vec<String>>();
    let input_file = &args[1];
    let input = read_file(input_file);

    let space: Vec<Vec<Cell>> = input
        .lines()
        .map(|l| {
            l.chars()
                .map(|c| match c {
                    '.' => Cell::Empty,
                    '#' => Cell::Asteroid,
                    c => panic!("Bad char {}", c),
                })
                .collect()
        })
        .collect();

    let mut stations: HashMap<Point, HashSet<Point>> = HashMap::new();

    pr(&space);

    for (y, w) in (0..).zip(&space) {
        for (x, c) in (0..).zip(w) {
            match c {
                Cell::Empty => {}
                Cell::Asteroid => {
                    println!("\nChecking asteroid at {}", Point { y: y, x: x });
                    let r =
                        stations.insert(Point { y, x }, find_asteroids(&Point { y, x }, &space));
                    assert!(r.is_none())
                }
            }
        }
    }

    let m = stations
        .iter()
        .max_by(|a, b| a.1.len().cmp(&b.1.len()))
        .unwrap();
    println!("{} {}", m.0, m.1.len());

    (m.0.clone(), m.1.clone())
}

fn find_asteroids(p: &Point, space: &[Vec<Cell>]) -> HashSet<Point> {
    let mut asteroids = HashSet::new();

    let max_y = space.len() as i32;
    let max_x = space[0].len() as i32;
    let mut vectors = Vec::new();
    for y in (-max_y)..max_y {
        for x in (-max_x)..max_x {
            let v = Vector { x_len: x, y_len: y };
            if !(v.is_zero() || v.can_downscale()) {
                vectors.push(v)
            }
        }
    }

    for vector in vectors {
        // println!("new base vector {}", vector);
        let mut i = 1;
        'running: loop {
            let scaled = vector.scale(i);
            //println!("scaled = {}", scaled);
            let new_p = p.add(&scaled);
            //println!("new_p = {}", new_p);
            let prefix = format!("scaled: {}, new_p: {}", scaled, new_p);
            match space
                .get(new_p.y as usize)
                .map(|o| o.get(new_p.x as usize))
                .flatten()
            {
                Some(Cell::Asteroid) => {
                    if asteroids.insert(new_p.clone()) {
                        println!("{} Found new asteroid @ {}", prefix, new_p);
                    } else {
                        println!("{} Already handled asteroid @ {}", prefix, new_p);
                    }
                    break 'running;
                }
                Some(Cell::Empty) => {
                    println!("{} Empty space @ {}", prefix, new_p);
                }
                None => {
                    // println!("{} Out of bounds @ {}", prefix, new_p);
                    break 'running;
                }
            }
            i += 1
        }
    }

    println!("Asteroids found: {}", asteroids.len());
    asteroids
}

fn pr(space: &[Vec<Cell>]) {
    for w in space {
        println!();
        for c in w {
            print!(
                "{}",
                match c {
                    Cell::Empty => '.',
                    Cell::Asteroid => '#',
                }
            )
        }
    }
    println!();
}

fn peq(a: f64, b: f64) -> bool {
    approx_eq!(f64, a, b, epsilon = 0.0000001)
}

fn pcmp(a: f64, b: f64) -> Ordering {
    if peq(a, b) {
        Ordering::Equal
    } else {
        if a < b {
            Ordering::Less
        } else {
            Ordering::Greater
        }
    }
}

fn part2(origo: &Point, h: HashSet<Point>) {

    let polar = Polar { r: 1.0, p: 0.0 };
    assert!(
        approx_eq!(
            f64,
            polar.rotate90().p,
            Polar { r: 1.0, p: PI }.p,
            epsilon = 0.0000001
        )
    );
    println!("{:?} {:?}", polar, polar.rotate90().rotate90().rotate90().rotate90());
    let rotated = polar.rotate90().rotate90().rotate90().rotate90();
    assert!(
        approx_eq!(
            f64,
            polar.p,
            rotated.p,
            epsilon = 0.0000001
        ),
        format!("{:?} {:?}", polar, rotated)
    );

    let mut remaining: Vec<(Point, Polar)> = h
        .iter()
        .map(|p| (p.clone(), point_to_polar(&p.with_origo(origo))))
        .collect::<Vec<(Point, Polar)>>();

    remaining.sort_by(|a, b| {
        let ap = a.1.p;
        let ar = a.1.r;
        let bp = b.1.p;
        let br = b.1.r;
        match pcmp(ap, bp) {
            Ordering::Equal =>
                match pcmp(ar, br) {
                    Ordering::Equal => panic!("Points should not be equal, got {:?} {:?}", a, b),
                    o => o,
                }
            o => o
        }
    });

    let mut destroyed: Vec<(Point, Polar)> = Vec::new();
    while !remaining.is_empty() {
        println!("remaining: {}", remaining.len());
        let clone = remaining.clone();
        let mut removed = 0;
        for (p, i) in clone.iter().zip(0..) {
            let should_destroy = match destroyed.last() {
                None => true,
                Some(prev_p) => !peq(prev_p.1.p, p.1.p),
            };
            if should_destroy {
                println!("destroy {}, {:?}", i, p);
                remaining.remove(i - removed);
                removed += 1;
                destroyed.push(p.clone())
            }
        }
    }

    println!("{:?}", destroyed[199])
}
