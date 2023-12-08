use aoc2023::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s08a.txt"), 6);
    assert_eq!(a("txt/s08b.txt"), 2);
    assert_eq!(a("txt/e08.txt"), 24253);
    assert_eq!(b("txt/s08c.txt"), 6);
    assert_eq!(b("txt/e08.txt"), 12357789728873);
}

struct S {
    directions: Vec<char>,
    paths: BTreeMap<Node, (Node, Node)>,
}

fn parse(s: Vec<String>) -> S {
    let directions = s[0].chars().collect();

    let paths = s[2..]
        .into_iter()
        .map(|path| {
            let cap = Captures::new(regex!(r#"^([^ ]+) = \(([^,]+), ([^\)]+)\)$"#), path).unwrap();
            let from = parse_node_cap(&cap, 1);
            let left = parse_node_cap(&cap, 2);
            let right = parse_node_cap(&cap, 3);
            (from, (left, right))
        })
        .collect();
    S { directions, paths }
}

type Node = (char, char, char);
fn parse_node_cap(cap: &Captures, n: usize) -> Node {
    parse_node(&cap.get::<String>(n).unwrap())
}
fn parse_node(s: &str) -> Node {
    let v: Vec<char> = s.chars().collect();
    (v[0], v[1], v[2])
}

fn a(s: &str) -> usize {
    let s: S = parse(read_parsed::<String>(s).collect());

    let mut steps = 0;
    let mut pos = parse_node("AAA");
    let end = parse_node("ZZZ");
    'loo: loop {
        let dir = s.directions[steps % s.directions.len()];
        if pos == end {
            break 'loo steps;
        }
        let &(left, right) = s.paths.get(&pos).unwrap();
        pos = if dir == 'L' { left } else { right };
        steps += 1;
    }
}

fn b(s: &str) -> usize {
    let s: S = parse(read_parsed::<String>(s).collect());

    let mut steps = 0;
    let mut rem_pos: Vec<Node> = s.paths.keys().copied().filter(|&v| v.2 == 'A').collect();
    let mut final_pos: Vec<usize> = vec![];
    while !rem_pos.is_empty() {
        let dir = s.directions[steps % s.directions.len()];
        steps += 1;
        for p in std::mem::take(&mut rem_pos) {
            let (left, right) = s.paths.get(&p).unwrap();
            let &next = if dir == 'L' { left } else { right };
            if next.2 == 'Z' {
                final_pos.push(steps);
            } else {
                rem_pos.push(next);
            }
        }
    }
    let mut lcm: Option<usize> = None;
    for zi in final_pos {
        lcm = Some(if let Some(lcm) = lcm {
            num::integer::lcm(lcm, zi)
        } else {
            zi
        });
    }
    lcm.unwrap()
}
