use aoc2020::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s13.txt"), 295);
    assert_eq!(a("txt/e13.txt"), 3997);
}

fn a(filename: &str) -> i64 {
    let lines: Vec<String> = read_lines(filename).map(|r| r.unwrap()).collect();
    let earliest: i64 = lines[0].parse().unwrap();
    let ids: Vec<i64> = lines[1].split(',').filter_map(|id| id.parse().ok()).collect();

    struct Res {
        id: i64,
        offset: i64,
    }

    let mut min = Res { id: 0, offset: 0 };

    for id in ids {
        let i = earliest / id;
        let offset = (i+1)*id - earliest;
        if min.id == 0 || min.offset > offset {
            min = Res { id, offset };
        }
    }

    min.id * min.offset
}

fn _b(_filename: &str) -> i64 {
    todo!()
}
