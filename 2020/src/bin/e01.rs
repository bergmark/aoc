use aoc2020::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s01.txt"), (vec![299, 1721], 514579));
    assert_eq!(a("txt/e01.txt"), (vec![861, 1159], 997899));
    assert_eq!(b("txt/s01.txt"), (vec![366, 675, 979], 241861950));
    assert_eq!(b("txt/e01.txt"), (vec![277, 337, 1406], 131248694));
}

fn a(s: &str) -> (Vec<usize>, usize) {
    sol(s, 2)
}
fn b(s: &str) -> (Vec<usize>, usize) {
    sol(s, 3)
}

fn sol(s: &str, perms: usize) -> (Vec<usize>, usize) {
    for mut xs in read_parsed(s).permutations(perms) {
        if eq(xs.iter().sum(), 2020) {
            let p = xs.iter().product();
            xs.sort_unstable();
            return (xs, p);
        }
    }
    unreachable!()
}
