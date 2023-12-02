use aoc2023::*;
use regex_captures::{CaptureError, Captures};

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    let limits = ColorSet::new([(Color::Red, 12), (Color::Green, 13), (Color::Blue, 14)]);
    assert_eq!(a("txt/s02.txt", &limits), 8);
    assert_eq!(a("txt/e02.txt", &limits), 2149);
    assert_eq!(b("txt/s02.txt"), 2286);
    assert_eq!(b("txt/e02.txt"), 71274);
}

fn a(s: &str, limits: &ColorSet) -> usize {
    read_parsed::<Game>(s)
        .filter(|game| {
            game.sets
                .iter()
                .all(|set| set.iter().all(|(color, count)| limits.get(color) >= count))
        })
        .map(|game| game.id)
        .sum()
}

fn b(s: &str) -> usize {
    read_parsed::<Game>(s)
        .map(|game| {
            let mut minimums: BTreeMap<Color, usize> = Default::default();
            for set in game.sets {
                for (color, count) in set.iter() {
                    let e = minimums.entry(color).or_insert(count);
                    *e = std::cmp::max(*e, count);
                }
            }
            minimums.values().product::<usize>()
        })
        .sum()
}

#[derive(Debug)]
struct Game {
    id: usize,
    sets: Vec<ColorSet>,
}

#[derive(Debug, Copy, Clone)]
struct ColorSet([(Color, usize); 3]);

impl ColorSet {
    fn new(set: [(Color, usize); 3]) -> Self {
        Self(set)
    }
    fn get(&self, color: Color) -> usize {
        *self
            .0
            .iter()
            .find(|(clr, _)| *clr == color)
            .map(|(_, count)| count)
            .unwrap()
    }
    fn iter(&self) -> impl Iterator<Item = (Color, usize)> + '_ {
        self.0.iter().copied()
    }
    fn from_iter(iter: impl Iterator<Item = (Color, usize)>) -> Self {
        let mut arr: [(Color, usize); 3] = [(Color::Red, 0), (Color::Green, 0), (Color::Blue, 0)];
        for (color, count) in iter {
            let i = match color {
                Color::Red => 0,
                Color::Green => 1,
                Color::Blue => 2,
            };
            arr[i] = (color, count);
        }
        Self(arr)
    }
}

#[derive(Debug, Copy, Clone, PartialOrd, Ord, Eq, PartialEq, strum::EnumString)]
#[strum(serialize_all = "lowercase")]
enum Color {
    Red,
    Green,
    Blue,
}

impl FromStr for Game {
    type Err = CaptureError;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let cap = Captures::new(regex!(r#"^Game (?P<id>\d+): (?P<sets>.+)$"#), s)?;
        let id: usize = cap.parse_name("id")?;
        let sets: Vec<ColorSet> = cap
            .name::<&str>("sets")?
            .split("; ")
            .map(|set| {
                ColorSet::from_iter(set.split(", ").map(|color_count| {
                    let mut color_count = color_count.split(' ');
                    let count: usize = color_count.next().unwrap().parse().unwrap();
                    let color = color_count.next().unwrap().parse().unwrap();
                    (color, count)
                }))
            })
            .collect();

        Ok(Self { id, sets })
    }
}
