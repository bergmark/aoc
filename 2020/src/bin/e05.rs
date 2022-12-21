use aoc2020::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    tests();
    assert_eq!(a(), 866);
    assert_eq!(b(), Some(583));
}

fn a() -> usize {
    read_parsed("txt/e05.txt")
        .map(|l: Line| l.seat_id)
        .max()
        .unwrap()
}

fn b() -> Option<usize> {
    let mut ids: Vec<_> = read_parsed("txt/e05.txt")
        .map(|l: Line| l.seat_id)
        .collect();
    ids.sort_unstable();
    for (i, id) in ids.iter().enumerate() {
        if ids[i + 1] != id + 1 {
            return Some(id + 1);
        }
    }
    None
}

#[derive(Debug, PartialEq, Eq)]
struct Line {
    row: usize,
    col: usize,
    seat_id: usize,
}

impl FromStr for Line {
    type Err = ();
    fn from_str(s: &str) -> Result<Line, ()> {
        let (row, col) = s.split_at(7);
        let row =
            usize::from_str_radix(&row.replace('F', "0").replace('B', "1"), 2).map_err(|_| ())?;
        let col =
            usize::from_str_radix(&col.replace('L', "0").replace('R', "1"), 2).map_err(|_| ())?;
        let seat_id = row * 8 + col;
        Ok(Line { row, col, seat_id })
    }
}

fn tests() {
    assert_eq!(
        Line::from_str("FBFBBFFRLR").unwrap(),
        Line {
            row: 44,
            col: 5,
            seat_id: 357
        }
    );
    // BFFFBBFRRR: row 70, column 7, seat ID 567.
    assert_eq!(
        Line::from_str("BFFFBBFRRR").unwrap(),
        Line {
            row: 70,
            col: 7,
            seat_id: 567,
        }
    );
    // FFFBBBFRRR: row 14, column 7, seat ID 119.
    assert_eq!(
        Line::from_str("FFFBBBFRRR").unwrap(),
        Line {
            row: 14,
            col: 7,
            seat_id: 119,
        }
    );
    // BBFFBBFRLL: row 102, column 4, seat ID 820.
    assert_eq!(
        Line::from_str("BBFFBBFRLL").unwrap(),
        Line {
            row: 102,
            col: 4,
            seat_id: 820,
        }
    );
}
