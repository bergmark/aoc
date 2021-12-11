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
    let mut first_invalids = vec![];

    for line in read_parsed::<String>(s) {
        let mut stack = vec![];
        'line: for c in line.chars() {
            match c {
                '[' | '(' | '<' | '{' => stack.push(c),
                ']' | ')' | '>' | '}' => {
                    let last = stack.last();
                    match last {
                        None => {
                            first_invalids.push(c);
                            break 'line;
                        }
                        Some(last) => match (last, c) {
                            ('[', ']') | ('(', ')') | ('<', '>') | ('{', '}') => {
                                stack.pop();
                            }
                            _ => {
                                first_invalids.push(c);
                                break 'line;
                            }
                        },
                    }
                }
                _ => unreachable!(),
            }
        }
    }

    first_invalids
        .into_iter()
        .map(|c| match c {
            ')' => 3,
            ']' => 57,
            '}' => 1197,
            '>' => 25137,
            _ => unreachable!(),
        })
        .sum()
}

fn b(s: &str) -> usize {
    let mut scores = vec![];

    for line in read_parsed::<String>(s) {
        if let Some(stack) = incomplete_line(&line) {
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
            scores.push(score);
        }
    }

    scores.sort_unstable();

    let mid = (scores.len() - 1) / 2;

    dbg!(&scores);
    dbg!(mid);
    scores[mid]
}

fn incomplete_line(line: &str) -> Option<Vec<char>> {
    let mut stack = vec![];
    for c in line.chars() {
        match c {
            '[' | '(' | '<' | '{' => stack.push(c),
            ']' | ')' | '>' | '}' => {
                let last = stack.last();
                match last {
                    None => {
                        return None;
                    }
                    Some(last) => match (last, c) {
                        ('[', ']') | ('(', ')') | ('<', '>') | ('{', '}') => {
                            stack.pop();
                        }
                        _ => {
                            return None;
                        }
                    },
                }
            }
            _ => unreachable!(),
        }
    }
    if stack.is_empty() {
        None
    } else {
        Some(stack)
    }
}
