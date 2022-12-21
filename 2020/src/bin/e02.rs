use aoc2020::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s02.txt"), 2);
    assert_eq!(a("txt/e02.txt"), 456);
    assert_eq!(b("txt/s02.txt"), 1);
    assert_eq!(b("txt/e02.txt"), 308);
}

fn a(s: &str) -> usize {
    sol(s, validate_a)
}

fn b(s: &str) -> usize {
    sol(s, validate_b)
}

fn sol(s: &str, f: impl Fn(&Line) -> bool) -> usize {
    read_parsed::<Line, _>(s).filter(f).count()
}

fn validate_a(l: &Line) -> bool {
    let count = l.password.iter().filter(|&&c| c == l.rule.letter).count();
    count >= l.rule.min && count <= l.rule.max
}

fn validate_b(l: &Line) -> bool {
    (l.password[l.rule.min - 1] == l.rule.letter) ^ (l.password[l.rule.max - 1] == l.rule.letter)
}

struct Line {
    rule: Rule,
    password: Vec<char>,
}

impl FromStr for Line {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (rule, password) = split2(s, ": ")?;
        let rule = Rule::from_str(rule).unwrap();
        let password = password.chars().collect();
        Ok(Line { rule, password })
    }
}

struct Rule {
    min: usize,
    max: usize,
    letter: char,
}

impl FromStr for Rule {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (range, letter) = split2(s, " ")?;
        let (min, max) = split2(range, "-")?;
        let letter = letter.parse().unwrap();
        let min = min.parse().unwrap();
        let max = max.parse().unwrap();
        Ok(Rule { min, max, letter })
    }
}
