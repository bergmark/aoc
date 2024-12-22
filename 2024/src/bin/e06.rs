use aoc2024::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s06.txt"), 41);
    assert_eq!(a("txt/e06.txt"), 5305);
}

fn a(s: &str) -> i64 {
    let mut grid = Grid::from_file(s);
    let (mut pos, guard_c) = grid
        .points()
        .find(|(_, v)| ['^', '>', 'v', '<'].contains(v))
        .unwrap();
    grid.print();
    let mut dir = match guard_c {
        '^' => Direction::N,
        '>' => Direction::E,
        'v' => Direction::S,
        '<' => Direction::W,
        _ => unreachable!(),
    };
    let mut visited = BTreeSet::from([]);
    'loo: loop {
        visited.insert(pos);
        grid.insert(pos, 'X').unwrap();
        let new_pos = pos + dir;
        let new_v = grid.get(new_pos);
        match new_v {
            None => break 'loo,
            Some('#') => {
                dir = dir.rotate_right(90);
            }
            Some('.' | 'X') => {
                pos = new_pos;
            }
            Some(c) => panic!("{c}"),
        };
    }
    visited.len() as i64
}
