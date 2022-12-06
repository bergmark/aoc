use aoc2022::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s04.txt"), 2);
    assert_eq!(a("txt/e04.txt"), 513);
    assert_eq!(b("txt/s04.txt"), 4);
    assert_eq!(b("txt/e04.txt"), 878);
}

fn a(s: &str) -> usize {
    read_parsed(s)
        .filter(|Section { a, b }| {
            (a.contains(b.start()) && a.contains(b.end()))
                || (b.contains(a.start()) && b.contains(a.end()))
        })
        .count()
}

fn b(s: &str) -> usize {
    read_parsed(s)
        .filter(|Section { a, b }| {
            a.contains(b.start())
                || a.contains(b.end())
                || b.contains(a.start())
                || b.contains(a.end())
        })
        .count()
}

struct Section {
    a: RangeInclusive<i64>,
    b: RangeInclusive<i64>,
}

impl FromStr for Section {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, ()> {
        let v: Vec<_> = s
            .split(',')
            .map(ToString::to_string)
            .map(|v| {
                v.split('-')
                    .map(|v| v.parse::<i64>().unwrap())
                    .collect::<Vec<_>>()
            })
            .collect();
        Ok(Section {
            a: v[0][0]..=v[0][1],
            b: v[1][0]..=v[1][1],
        })
    }
}
