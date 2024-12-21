use aoc2024::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s05.txt"), 143);
    assert_eq!(a("txt/e05.txt"), 5248);
    //assert_eq!(b("txt/s05.txt"), 0);
    //assert_eq!(b("txt/e05.txt"), 0);
}

fn a(s: &str) -> i64 {
    let s = read_to_string(s);
    let Input { orderings, rows } = parse(&s);
    let mut sum = 0;
    for row in rows {
        if let Some(x) = check_row(&orderings, &row) {
            sum += x;
        }
    }
    sum
}

fn check_row(orderings: &[Ord], row: &[i64]) -> Option<i64> {
    for i in 0..(row.len() - 1) {
        for j in (i + 1)..row.len() {
            if !is_ordered(&orderings, row[i], row[j]) {
                return None;
            }
        }
    }
    Some(row[row.len() / 2])
}

fn is_ordered(ords: &[Ord], a: i64, b: i64) -> bool {
    if ords.contains(&(a, b)) {
        true
    } else if ords.contains(&(b, a)) {
        false
    } else {
        true
    }
}

type Ord = (i64, i64);

struct Input {
    orderings: Vec<Ord>,
    rows: Vec<Vec<i64>>,
}

fn parse(s: &str) -> Input {
    let mut s = s.split("\n\n");
    let orderings = s.next().unwrap();
    let orderings = orderings
        .split('\n')
        .map(|v| {
            let v: Vec<&str> = v.split('|').collect::<Vec<_>>();
            (v[0].parse().unwrap(), v[1].parse().unwrap())
        })
        .collect();
    let rows = s
        .next()
        .unwrap()
        .trim()
        .split("\n")
        .map(|v| v.split(',').map(|s| s.parse().unwrap()).collect())
        .collect();
    Input { orderings, rows }
}
