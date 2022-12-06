use aoc2022::*;
use lazy_regex::regex;
use std::collections::VecDeque;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s05.txt"), "CMZ");
    assert_eq!(a("txt/e05.txt"), "FCVRLMVQP");
    assert_eq!(b("txt/s05.txt"), "MCD");
    assert_eq!(b("txt/e05.txt"), "RWLWGJGFD");
}

fn a(s: &str) -> String {
    let rows: Vec<Row> = read_parsed::<Row>(s).collect();
    let mut stacks: Vec<Stack> = vec![Default::default(); 10];
    let mut instructions: Vec<Move> = vec![];
    for row in &rows {
        if let Row::Buckets { v } = row {
            let mut i = 0;
            for bucket in v {
                i += 1;
                if let Some(c) = bucket {
                    stacks[i].push_front(*c);
                }
            }
        } else if let Row::Move(m) = row {
            instructions.push(m.clone());
        }
    }

    for Move { amount, src, dest } in instructions {
        for _ in 1..=amount {
            let crat = stacks[src].pop_back().unwrap();
            stacks[dest].push_back(crat);
        }
    }

    let mut res: Vec<char> = vec![];
    for stack in stacks {
        if !stack.is_empty() {
            res.push(*stack.back().unwrap());
        }
    }

    res.iter().collect()
}

fn b(s: &str) -> String {
    let rows: Vec<Row> = read_parsed::<Row>(s).collect();
    let mut stacks: Vec<Stack> = vec![Default::default(); 10];
    let mut instructions: Vec<Move> = vec![];
    for row in &rows {
        if let Row::Buckets { v } = row {
            let mut i = 0;
            for bucket in v {
                i += 1;
                if let Some(c) = bucket {
                    stacks[i].push_front(*c);
                }
            }
        } else if let Row::Move(m) = row {
            instructions.push(m.clone());
        }
    }

    for Move { amount, src, dest } in instructions {
        println!("move {amount} from {src} to {dest}");
        let mut intermed = vec![];
        for _ in 1..=amount {
            let crat = stacks[src].pop_back().unwrap();
            intermed.push(crat);
        }
        for i in intermed.iter().rev() {
            stacks[dest].push_back(*i);
        }
    }

    let mut res: Vec<char> = vec![];
    for stack in stacks {
        if !stack.is_empty() {
            res.push(*stack.back().unwrap());
        }
    }

    res.iter().collect()
}

type Stack = VecDeque<char>;

#[derive(Debug, Clone)]
enum Row {
    Empty,
    Buckets { v: Vec<Option<char>> },
    Move(Move),
}

#[derive(Debug, Clone)]
struct Move {
    amount: usize,
    src: usize,
    dest: usize,
}

impl FromStr for Row {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, ()> {
        if regex!(r#"^[ \d]*$"#).is_match(s) {
            Ok(Row::Empty)
        } else if s.starts_with("move") {
            let cap = Captures::new(regex!(r#"move (\d+) from (\d+) to (\d+)"#), s).unwrap();
            let amount = cap.parse(1).unwrap();
            let src = cap.parse(2).unwrap();
            let dest = cap.parse(3).unwrap();
            Ok(Row::Move(Move { amount, src, dest }))
        } else {
            let chunks = s.chars().chunks(4);
            let mut res = vec![];
            for chunk in &chunks {
                let chunk: String = chunk.collect();
                if regex!("^ +$").is_match(&chunk) {
                    res.push(None);
                } else if regex!(r#"^\[([A-Z])\]"#).is_match(&chunk) {
                    let cap = Captures::new(regex!(r#"^\[([A-Z])\]"#), &chunk).unwrap();
                    res.push(Some(cap.parse(1).unwrap()));
                } else {
                    panic!("chunk error on: {chunk:?}")
                }
            }
            Ok(Row::Buckets { v: res })
        }
    }
}
