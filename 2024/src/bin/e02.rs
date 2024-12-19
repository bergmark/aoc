use aoc2024::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s02.txt"), 2);
    assert_eq!(a("txt/e02.txt"), 359);
    assert_eq!(b("txt/s02.txt"), 4);
    assert_eq!(b("txt/e02.txt"), 418);
}

fn a(s: &str) -> usize {
    read_parsed::<SpaceSep<i64>>(s)
        .filter(|SpaceSep(v)| is_ok(v))
        .count()
}

fn b(s: &str) -> usize {
    read_parsed::<SpaceSep<i64>>(s)
        .filter(|SpaceSep(v)| is_ok(v) || is_dampened(v))
        .count()
}

fn is_ok(v: &[i64]) -> bool {
    let pairs = || v.iter().zip(v.iter().skip(1));
    let increasing = pairs().all(|(a, b)| a <= b);
    let decreasing = pairs().all(|(a, b)| a >= b);
    let differ = pairs().all(|(a, b)| (1..=3).contains(&(a - b).abs()));
    // println!("{v:?} increasing? {increasing}, decreasing? {decreasing}, differ {differ}");
    (increasing || decreasing) && differ
}

fn is_dampened(v: &[i64]) -> bool {
    (0..v.len()).any(|skip| {
        let new_v: Vec<_> = v
            .iter()
            .copied()
            .take(skip)
            .chain(v.iter().copied().skip(skip + 1))
            .collect();
        is_ok(&new_v)
    })
}
