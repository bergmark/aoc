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
    assert_eq!(b("txt/s05.txt"), 123);
    assert_eq!(b("txt/e05.txt"), 4507);
}

fn a(s: &str) -> i64 {
    let Input { orderings, rows } = parse(s);
    rows.iter()
        .map(|row| {
            if is_ordered(&orderings, row) {
                row[row.len() / 2]
            } else {
                0
            }
        })
        .sum()
}

fn b(s: &str) -> i64 {
    let Input { orderings, rows } = parse(s);
    rows.into_iter()
        .map(|mut row| {
            if is_ordered(&orderings, &row) {
                0
            } else {
                row.sort_by(|&a, &b| cmp(&orderings, a, b));
                row[row.len() / 2]
            }
        })
        .sum()
}

fn is_ordered(orderings: &[Ord], row: &[i64]) -> bool {
    for (i, &row_i) in row.iter().enumerate() {
        for &row_j in row.iter().skip(i + 1) {
            if cmp(orderings, row_i, row_j) == Ordering::Greater {
                return false;
            }
        }
    }
    true
}

fn cmp(ords: &[Ord], a: i64, b: i64) -> Ordering {
    if ords.contains(&(a, b)) {
        Ordering::Less
    } else if ords.contains(&(b, a)) {
        Ordering::Greater
    } else {
        unreachable!()
    }
}

type Ord = (i64, i64);

struct Input {
    orderings: Vec<Ord>,
    rows: Vec<Vec<i64>>,
}

fn parse(s: &str) -> Input {
    let s = read_to_string(s);
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
