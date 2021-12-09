use aoc2021::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s07.txt"), 37);
    assert_eq!(a("txt/e07.txt"), 328262);
    assert_eq!(b("txt/s07.txt"), 168);
    assert_eq!(b("txt/e07.txt"), 90040997);
}

fn a(s: &str) -> i64 {
    let pos: Vec<i64> = read_lines(s)
        .next()
        .unwrap()
        .unwrap()
        .split(',')
        .map(|s| s.parse::<i64>().unwrap())
        .collect();

    let mut min_cost = 99999999;

    for i in *pos.iter().min().unwrap()..=*pos.iter().max().unwrap() {
        //dbg!(i);
        let mut cost = 0;
        for j in 0..pos.len() {
            cost += (i - pos[j]).abs()
        }
        min_cost = std::cmp::min(cost, min_cost);
    }

    min_cost
}

fn b(s: &str) -> i64 {
    let pos: Vec<i64> = read_lines(s)
        .next()
        .unwrap()
        .unwrap()
        .split(',')
        .map(|s| s.parse::<i64>().unwrap())
        .collect();

    let mut min_cost = None;

    for i in *pos.iter().min().unwrap()..=*pos.iter().max().unwrap() {
        let mut cost = 0;
        for j in 0..pos.len() {
            cost += summer((i - pos[j]).abs());
        }
        match min_cost {
            None => min_cost = Some(cost),
            Some(m) => min_cost = Some(std::cmp::min(cost, m)),
        }
    }

    min_cost.unwrap()
}

fn summer(i: i64) -> i64 {
    i * (1 + i) / 2
}
