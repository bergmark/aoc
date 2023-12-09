use aoc2023::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s09.txt"), 114);
    assert_eq!(a("txt/e09.txt"), 2005352194);
    assert_eq!(b("txt/s09.txt"), 2);
    assert_eq!(b("txt/e09.txt"), 1077);
}

fn a(s: &str) -> i64 {
    read_parsed::<SpaceSep<i64>>(s).map(|v| v.0).map(p1).sum()
}
fn b(s: &str) -> i64 {
    read_parsed::<SpaceSep<i64>>(s).map(|v| v.0).map(p2).sum()
}

fn p2(vs: Vec<i64>) -> i64 {
    let mut rows: Vec<Vec<i64>> = vec![vec![0]];
    rows[0].extend(vs);
    populate(&mut rows, true);
    for row_i in (1..rows.len()).rev() {
        rows[row_i - 1][0] = rows[row_i - 1][1] - rows[row_i][0];
    }
    *rows[0].first().unwrap()
}

fn p1(vs: Vec<i64>) -> i64 {
    let mut rows = vec![vs];
    populate(&mut rows, false);
    for row_i in (1..rows.len()).rev() {
        let last_of_prev_row = *rows[row_i - 1].last().unwrap();
        let last_of_next_row = *rows[row_i].last().unwrap();
        rows[row_i - 1].push(last_of_prev_row + last_of_next_row);
    }
    *rows[0].last().unwrap()
}

fn populate(rows: &mut Vec<Vec<i64>>, offset: bool) {
    let mut all_zero = false;
    let mut row_i = 0;
    while !all_zero {
        let mut all_zero2 = true;
        rows.push(if offset { vec![0] } else { vec![] });
        for j in (if offset { 1 } else { 0 })..(rows[row_i].len() - 1) {
            let diff: i64 = rows[row_i][j + 1] - rows[row_i][j];
            rows.last_mut().unwrap().push(diff);
            if diff != 0 {
                all_zero2 = false;
            }
        }
        row_i += 1;
        all_zero = all_zero2;
    }
}
