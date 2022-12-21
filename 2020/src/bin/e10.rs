use aoc2020::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s10a.txt"), 7 * 5);
    assert_eq!(a("txt/s10b.txt"), 22 * 10);
    assert_eq!(a("txt/e10.txt"), 1656);
    // assert_eq!(b("txt/s10.txt"), 0);
    // assert_eq!(b("txt/e10.txt"), 0);
}

fn a(filename: &str) -> usize {
    let mut joltages: Vec<usize> = read_parsed(filename).collect();
    joltages.sort_unstable();
    joltages.push(joltages.last().unwrap() + 3);

    let mut last = 0;
    let mut diffs = Count::<usize>::default();
    for joltage in joltages.iter() {
        let diff = joltage - last;
        assert!(diff >= 1 && diff <= 3);
        diffs.count(diff);
        last = *joltage;
    }

    diffs.get(&1) * diffs.get(&3)
}

fn _b(_filename: &str) -> usize {
    todo!()
}
