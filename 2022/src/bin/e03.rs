use aoc2022::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s03.txt"), 157);
    assert_eq!(a("txt/e03.txt"), 8053);
    assert_eq!(b("txt/s03.txt"), 70);
    assert_eq!(b("txt/e03.txt"), 2425);
}

fn a(s: &str) -> i64 {
    read_parsed(s)
        .map(|line: String| {
            let chars: Vec<char> = line.chars().collect();
            let (a, b) = chars.split_at(chars.len() / 2);
            prio(*a.iter().find(|a| b.contains(a)).unwrap())
        })
        .sum()
}

fn b(s: &str) -> i64 {
    let mut prios = 0;
    fn g(x: &mut impl Iterator<Item = String>) -> Vec<char> {
        x.next().unwrap().chars().collect()
    }
    for mut group in &read_parsed::<String>(s).chunks(3) {
        let a = g(&mut group);
        let b = g(&mut group);
        let c = g(&mut group);
        prios += prio(*a.iter().find(|a| b.contains(a) && c.contains(a)).unwrap())
    }
    prios
}

fn prio(c: char) -> i64 {
    c as i64 - if ('a'..='z').contains(&c) { 96 } else { 38 }
}

#[test]
fn test_prio() {
    assert_eq!(prio('a'), 1, "a");
    assert_eq!(prio('z'), 26, "z");
    assert_eq!(prio('A'), 27, "A");
    assert_eq!(prio('Z'), 52, "Z");
}
