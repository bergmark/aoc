use aoc2021::*;
use lazy_regex::regex;
use std::ops::RangeInclusive;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s22a.txt"), 39);
    assert_eq!(a("txt/s22b.txt"), 590784);
    assert_eq!(a("txt/e22.txt"), 610196);
}

#[derive(Debug, Clone)]
struct Line {
    state: State,
    x: RangeInclusive<i64>,
    y: RangeInclusive<i64>,
    z: RangeInclusive<i64>,
}

impl FromStr for Line {
    type Err = ();
    fn from_str(s: &str) -> Result<Line, ()> {
        let r = regex!("^(on|off) x=([^,]+),y=([^,]+),z=([^,]+)$");
        let captures = r.captures(s).unwrap();
        let state = match &captures[1] {
            "on" => State::On,
            "off" => State::Off,
            s => panic!("{}", s),
        };
        let x = parse_range(&captures[2]);
        let y = parse_range(&captures[3]);
        let z = parse_range(&captures[4]);

        Ok(Line { state, x, y, z })
    }
}

fn parse_range(s: &str) -> RangeInclusive<i64> {
    let (start, end) = split2(s, "..").unwrap();
    RangeInclusive::new(
        std::cmp::max(-50, start.parse().unwrap()),
        std::cmp::min(50, end.parse().unwrap()),
    )
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum State {
    On,
    Off,
}

fn a(s: &str) -> usize {
    let mut cubes: BTreeMap<(i64, i64, i64), State> = Default::default();
    for Line { state, x, y, z } in read_parsed::<Line>(s) {
        for x in x.clone() {
            for y in y.clone() {
                for z in z.clone() {
                    cubes.insert((x, y, z), state);
                }
            }
        }
    }
    cubes.into_iter().filter(|(_, v)| *v == State::On).count()
}
