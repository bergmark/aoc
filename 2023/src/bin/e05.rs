use aoc2023::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s05.txt"), 35);
    assert_eq!(a("txt/e05.txt"), 26273516);
    assert_eq!(b("txt/s05.txt"), 46);
    //assert_eq!(b("txt/e05.txt"), 34039469);
}

#[derive(Debug)]
struct Almanac {
    seeds: Vec<usize>,
    maps: Vec<Map>,
}

#[derive(Debug)]
struct Map {
    mappings: Vec<(usize, usize, usize)>,
}

fn a(s: &str) -> usize {
    let almanac = parse(s);
    almanac
        .seeds
        .into_iter()
        .map(|seed| seed_to_location(&almanac.maps, seed))
        .min()
        .unwrap()
}

fn b(s: &str) -> usize {
    let almanac = parse(s);
    (0..almanac.seeds.len())
        .step_by(2)
        .flat_map(|i| almanac.seeds[i]..almanac.seeds[i] + almanac.seeds[i + 1])
        .map(|seed| seed_to_location(&almanac.maps, seed))
        .min()
        .unwrap()
}

fn seed_to_location(maps: &[Map], mut input: usize) -> usize {
    for map in maps {
        input = map.resolve(input);
    }
    input
}

impl Map {
    fn resolve(&self, input: usize) -> usize {
        for &(dst, src, len) in &self.mappings {
            if input >= src && input < src + len {
                return dst + input - src;
            }
        }
        input
    }
}

#[derive(Debug)]
enum Lex {
    Seeds(Vec<usize>),
    MapHead,
    Values(usize, usize, usize),
    Empty,
}

impl FromStr for Lex {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, ()> {
        if s.trim().is_empty() {
            Ok(Lex::Empty)
        } else if let Ok(cap) = Captures::new(regex!(r#"^seeds: (?P<seeds>[\d ]+)$"#), s) {
            Ok(Lex::Seeds(
                cap.parse_name::<SpaceSep<usize>>("seeds").unwrap().0,
            ))
        } else if regex!(r#"map:$"#).is_match(s) {
            Ok(Lex::MapHead)
        } else if regex!(r#"^[\d ]+$"#).is_match(s) {
            let digs: Vec<usize> = s.parse::<SpaceSep<_>>().unwrap().0;
            Ok(Lex::Values(digs[0], digs[1], digs[2]))
        } else {
            panic!("{s}")
        }
    }
}

fn parse(s: &str) -> Almanac {
    let mut almanac = Almanac {
        seeds: vec![],
        maps: vec![],
    };
    let mut map: Option<Map> = None;
    for line in read_parsed::<Lex>(s) {
        match line {
            Lex::Seeds(seeds) => almanac.seeds = seeds,
            Lex::Empty => {
                if let Some(map) = map {
                    almanac.maps.push(map);
                }
                map = None;
            }
            Lex::MapHead => {
                assert!(map.is_none());
                map = Some(Map { mappings: vec![] });
            }
            Lex::Values(dst, src, len) => {
                let mut x = map.unwrap();
                x.mappings.push((dst, src, len));
                map = Some(x);
            }
        }
    }
    if let Some(map) = map {
        almanac.maps.push(map);
    }
    almanac
}
