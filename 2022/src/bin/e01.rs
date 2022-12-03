use aoc2021::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s01.txt"), 24000);
    assert_eq!(a("txt/e01.txt"), 69795);
    assert_eq!(b("txt/s01.txt"), 45000);
    assert_eq!(b("txt/e01.txt"), 208437);
}

fn a(s: &str) -> i64 {
    sol(s)[0]
}

fn b(s: &str) -> i64 {
    sol(s).iter().take(3).sum()
}

fn sol(s: &str) -> Vec<i64> {
    let lines: Vec<String> = read_parsed(s).collect();
    let mut elves = vec![];
    let mut this_elf = 0;

    for line in lines {
        if line == "" {
            elves.push(this_elf);
            this_elf = 0;
        } else {
            let kcal: i64 = line.parse().unwrap();
            this_elf += kcal;
        }
    }
    if this_elf != 0 {
        elves.push(this_elf);
    }

    elves.sort_by(|a, b| b.cmp(a));
    elves
}
