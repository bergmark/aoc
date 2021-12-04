use aoc2021::{board::Board, cell::Cell, *};

use lazy_regex::regex;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s04.txt"), 4512);
    assert_eq!(a("txt/e04.txt"), 28082);
    assert_eq!(b("txt/s04.txt"), 1924);
    assert_eq!(b("txt/e04.txt"), 8224);
}

struct Input {
    draws: Vec<usize>,
    boards: Vec<Board>,
}

fn draws_from_str(s: &str) -> Vec<usize> {
    s.split(',').map(|s| s.parse().unwrap()).collect()
}

fn line_from_str(s: &str) -> Vec<Cell> {
    regex!(" +")
        .split(s.trim())
        .map(|s| Cell::unchecked(s.parse().unwrap()))
        .collect()
}

fn a(s: &str) -> usize {
    let Input { draws, mut boards } = parse(s);

    for draw in draws {
        for board in &mut boards {
            if board.check(draw) && board.is_complete() {
                return score(board, draw);
            }
        }
    }

    panic!()
}

fn b(s: &str) -> usize {
    let Input { draws, boards } = parse(s);

    let mut last_win: Option<(Board, usize)> = None;

    let mut next_boards = boards;
    for draw in draws {
        let boards = next_boards;
        next_boards = vec![];
        for mut board in boards.into_iter() {
            if board.check(draw) {
                if board.is_complete() {
                    last_win = Some((board, draw));
                } else {
                    next_boards.push(board);
                }
            } else {
                next_boards.push(board);
            }
        }
    }

    let (a, b) = last_win.unwrap();
    score(&a, b)
}

fn score(b: &Board, draw: usize) -> usize {
    let mut unchecked = 0;
    for v in &b.board {
        for Cell { number, checked } in v {
            if !checked {
                unchecked += number;
            }
        }
    }
    unchecked * draw
}

fn parse(s: &str) -> Input {
    let lines: Vec<_> = read_parsed::<String>(s).collect();
    let draws = draws_from_str(&lines[0]);
    let mut boards: Vec<Board> = vec![];
    let mut i = 2;
    while i < lines.len() {
        let mut j = 0;
        let mut board = vec![];
        while j < 5 && i < lines.len() {
            board.push(line_from_str(&lines[i]));
            j += 1;
            i += 1;
        }
        assert_eq!(board.len(), 5);
        boards.push(Board { board });
        i += 1;
    }
    Input { draws, boards }
}
