use aoc2022::*;
use std::collections::VecDeque;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s06.txt"), 7);
    assert_eq!(a("txt/e06.txt"), 1802);
    assert_eq!(b("txt/s06.txt"), 19);
    assert_eq!(b("txt/e06.txt"), 3551);
}

fn a(s: &str) -> i64 {
    sol(4, &read_to_string(s))
}

fn b(s: &str) -> i64 {
    sol(14, &read_to_string(s))
}

fn sol(n: usize, s: &str) -> i64 {
    let mut buf = VecDeque::new();

    for (c, i) in s.chars().zip(1..) {
        if buf.len() == n {
            buf.pop_back();
        }

        buf.push_front(c);

        if buf.iter().collect::<BTreeSet<_>>().len() == n {
            return i;
        }
    }
    todo!()
}

#[test]
fn test_sol() {
    assert_eq!(sol(4, "mjqjpqmgbljsphdztnvjfqwrcgsmlb"), 7);
    assert_eq!(sol(4, "bvwbjplbgvbhsrlpgdmjqwftvncz"), 5);
    assert_eq!(sol(4, "nppdvjthqldpwncqszvftbrmjlhg"), 6);
    assert_eq!(sol(4, "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"), 10);
    assert_eq!(sol(4, "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"), 11);
    assert_eq!(sol(14, "mjqjpqmgbljsphdztnvjfqwrcgsmlb"), 19);
    assert_eq!(sol(14, "bvwbjplbgvbhsrlpgdmjqwftvncz"), 23);
    assert_eq!(sol(14, "nppdvjthqldpwncqszvftbrmjlhg"), 23);
    assert_eq!(sol(14, "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"), 29);
    assert_eq!(sol(14, "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"), 26);
}
