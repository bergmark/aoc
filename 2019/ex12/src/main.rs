use std::env;
use std::fs;
use regex::Regex;
use std::cmp::Ordering;
use lazy_static::lazy_static;


fn read_file(fp: &str) -> String {
    fs::read_to_string(fp)
        .expect("Could not read file")
        .trim()
        .to_string()
}

fn parse(s: String) -> Vec<Moon> {
    lazy_static! {
        static ref RE: Regex =
            Regex::new(
                r"(?x)<
                  x=(?P<x>(?:-)?\d+),\s*
                  y=(?P<y>(?:-)?\d+),\s*
                  z=(?P<z>(?:-)?\d+)
                >$"
            ).unwrap();
    }
    let mut id = 0;
    s.lines().map(|l| {
        id += 1;
        RE.captures(l).map(|cap| {
            Moon::new(
                id,
                Position {
                    x: X(cap.name("x").unwrap().as_str().parse().unwrap()),
                    y: Y(cap.name("y").unwrap().as_str().parse().unwrap()),
                    z: Z(cap.name("z").unwrap().as_str().parse().unwrap())
                }
            )
        }).unwrap()
    }).collect()
}

fn main() {
    let args = env::args().collect::<Vec<String>>();
    let input_file = &args[1];
    let input = read_file(input_file);

    let mut moons = parse(input);
    print_moons(&moons);
    for i in 0..100 {
        step(&mut moons);
        println!("After {} steps", i+1);
        print_moons(&moons)
    }
}

fn print_moons(moons: &Vec<Moon>) {
    for moon in moons {
        println!("{}", show_moon(moon))
    };
    println!()
}

fn show_moon(moon: &Moon) -> String {
    format!("{}: {}, {}", moon.id, show_position(&moon.position), show_velocity(&moon.velocity))
}

fn show_position(p: &Position) -> String {
    format!("pos=<x={}, y={}, z={}>", p.x.0, p.y.0, p.z.0)
}

fn show_velocity(p: &Velocity) -> String {
    format!("vel=<x={}, y={}, z={}>", p.x.0, p.y.0, p.z.0)
}


fn step(moons: &mut Vec<Moon>) {
    let prev = moons.clone();
    for moon in moons.iter_mut() {
        for other_moon in &prev {
            if moon.id != other_moon.id {
                moon.velocity.x = moon.velocity.x.add(&compare(&moon.position.x, &other_moon.position.x));
                moon.velocity.y = moon.velocity.y.add(&compare(&moon.position.y, &other_moon.position.y));
                moon.velocity.z = moon.velocity.z.add(&compare(&moon.position.z, &other_moon.position.z));
            }
        }
        moon.position.x = moon.position.x.add(&moon.velocity.x);
        moon.position.y = moon.position.y.add(&moon.velocity.y);
        moon.position.z = moon.position.z.add(&moon.velocity.z);

    }
}

fn compare<C: Coord>(a: &C, b: &C) -> C {
    match a.val().cmp(&b.val()) {
        Ordering::Less => C::new(1),
        Ordering::Equal => C::new(0),
        Ordering::Greater => C::new(-1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compare() {
        assert_eq!(compare(&X(3), &X(5)), X(1));
        assert_eq!(compare(&X(5), &X(3)), X(-1));
    }
}

#[derive(Debug, Clone)]
pub struct Moon {
    id: i32,
    position: Position,
    velocity: Velocity,
}

impl Moon {
    fn new(id: i32, position: Position) -> Moon {
        Moon {
            id,
            position,
            velocity: Velocity::zero()
        }
    }
}

#[derive(Debug, Clone)]
pub struct Position {
    x: X,
    y: Y,
    z: Z,
}

#[derive(Debug, Clone)]
pub struct Velocity {
    x: X,
    y: Y,
    z: Z,
}

impl Velocity {
    fn zero() -> Velocity {
        Velocity { x: X(0), y: Y(0), z: Z(0) }
    }
}

trait Coord {
    fn new(i: i32) -> Self;
    fn val(&self) -> i32;
    fn add(&self, i: &Self) -> Self;
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct X(i32);
impl Coord for X {
    fn new(i: i32) -> X {
        X(i)
    }
    fn val(&self) -> i32 {
        self.0
    }
    fn add(&self, i: &X) -> X {
        X(self.0 + i.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Y(i32);
impl Coord for Y {
    fn new(i: i32) -> Y {
        Y(i)
    }
    fn val(&self) -> i32 {
        self.0
    }
    fn add(&self, i: &Y) -> Y {
        Y(self.0 + i.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Z(i32);
impl Coord for Z {
    fn new(i: i32) -> Z {
        Z(i)
    }
    fn val(&self) -> i32 {
        self.0
    }
    fn add(&self, i: &Z) -> Z {
        Z(self.0 + i.0)
    }
}
