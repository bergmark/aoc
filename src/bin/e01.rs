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
    sol(s, 1)
}

fn b(s: &str) -> i64 {
    sol(s, 3)
}

fn sol(s: &str, win: usize) -> i64 {
    let lines: Vec<_> = read_parsed(s).collect();
    let windows: Vec<i64> = lines.windows(win).map(|w| w.iter().sum()).collect();

    windows
        .iter()
        .zip(windows.iter().skip(1))
        .map(|(a, b)| if a < b { 1 } else { 0 })
        .sum()
}
