use aoc2023::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(b("txt/s11.txt", 2), 374);
    assert_eq!(b("txt/e11.txt", 2), 9565386);
    assert_eq!(b("txt/s11.txt", 10), 1030);
    assert_eq!(b("txt/s11.txt", 100), 8410);
    assert_eq!(b("txt/e11.txt", 1_000_000), 857986849428);
}

fn b(s: &str, expansion: usize) -> usize {
    let v: Vec<Vec<char>> = read_parsed::<Chars>(s).map(|v| v.0).collect();
    let mut empty_rows: BTreeSet<usize> = Default::default();
    let mut empty_cols: BTreeSet<usize> = Default::default();
    for (row_i, row) in v.iter().enumerate() {
        if row.iter().all(|&c| c == '.') {
            empty_rows.insert(row_i);
        }
    }
    for (col_i, _) in v[0].iter().enumerate() {
        let mut all_empty = true;
        for row in v.iter() {
            if row[col_i] != '.' {
                all_empty = false;
            }
        }
        if all_empty {
            empty_cols.insert(col_i);
        }
    }

    let mult =
        |v: &BTreeSet<usize>, p: i64| -> usize { v.get(&(p as usize)).map_or(1, |_| expansion) };

    Grid::new(v)
        .points()
        .filter_map(|(p, v)| (v == '#').then_some(p))
        .combinations(2)
        .map(|v| {
            let mut a = v[0];
            let b = v[1];
            let mut dist = 0;
            while a != b {
                dist += if a.row != b.row {
                    a.row += (b.row - a.row).signum();
                    mult(&empty_rows, a.row)
                } else {
                    a.col += (b.col - a.col).signum();
                    mult(&empty_cols, a.col)
                }
            }
            dist
        })
        .sum()
}
