use aoc2021::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s10.txt"), 26397);
    assert_eq!(a("txt/e10.txt"), 288291);
    assert_eq!(b("txt/s10.txt"), 288957);
    assert_eq!(b("txt/e10.txt"), 820045242);
}

fn a(s: &str) -> usize {
    read_parsed::<String>(s)
        .filter_map(|line| {
            if let Parsed::Invalid(c) = parse(&line) {
                Some(match c {
                    ')' => 3,
                    ']' => 57,
                    '}' => 1197,
                    '>' => 25137,
                    _ => unreachable!(),
                })
            } else {
                None
            }
        })
        .sum()
}

fn b(s: &str) -> usize {
    let mut scores: Vec<_> = read_parsed::<String>(s)
        .filter_map(|line| {
            if let Parsed::Incomplete(stack) = parse(&line) {
                let mut score = 0;
                for c in stack.into_iter().rev() {
                    score *= 5;
                    score += match c {
                        '(' => 1,
                        '[' => 2,
                        '{' => 3,
                        '<' => 4,
                        _ => unreachable!(),
                    }
                }
                Some(score)
            } else {
                None
            }
        })
        .collect();

    scores.sort_unstable();

    scores[(scores.len() - 1) / 2]
}

enum Parsed {
    Valid,
    Incomplete(Vec<char>),
    Invalid(char),
}

fn parse(line: &str) -> Parsed {
    let mut stack = vec![];
    for c in line.chars() {
        match c {
            '[' | '(' | '<' | '{' => stack.push(c),
            ']' | ')' | '>' | '}' => {
                let last = stack.last();
                match last {
                    None => {
                        return Parsed::Invalid(c);
                    }
                    Some(last) => match (last, c) {
                        ('[', ']') | ('(', ')') | ('<', '>') | ('{', '}') => {
                            stack.pop();
                        }
                        _ => {
                            return Parsed::Invalid(c);
                        }
                    },
                }
            }
            _ => unreachable!(),
        }
    }
    if stack.is_empty() {
        Parsed::Valid
    } else {
        Parsed::Incomplete(stack)
    }
}
