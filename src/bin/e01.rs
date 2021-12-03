use aoc2021::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s01.txt"), 7);
    assert_eq!(a("txt/e01.txt"), 1688);
    assert_eq!(b("txt/s01.txt"), 5);
    assert_eq!(b("txt/e01.txt"), 1728);
}

fn a(s: &str) -> i64 {
    sol(s)
}

fn sol(s: &str) -> i64 {
    let mut increased = 0;
    let mut prev = None;
    let lines: Vec<i64> = read_parsed::<i64>(s).collect();
    for curr in lines {
        match prev {
            None => {}
            Some(prev) => {
                if prev < curr {
                    increased += 1;
                }
            }
        }
        prev = Some(curr);
    }
    increased
}

fn b(s: &str) -> i64 {
    let mut windows: Vec<i64> = vec![];
    let lines: Vec<i64> = read_parsed(s).collect();
    for (i, _) in lines.iter().enumerate() {
        if i < lines.len() - 2 {
            windows.push(lines[i] + lines[i + 1] + lines[i + 2]);
        }
    }

    windows
        .iter()
        .zip(windows.iter().skip(1))
        .map(|(a, b)| if a < b { 1 } else { 0 })
        .sum()
}
