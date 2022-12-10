use aoc2022::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a0("txt/s10.txt"), 0);
    assert_eq!(a0("txt/s10b.txt"), 13140);
    assert_eq!(a0("txt/e10.txt"), 14420);
    assert_eq!(
        b("txt/e10.txt")
            .chunks(40)
            .map(|c| c.iter().join(""))
            .collect::<Vec<_>>(),
        vec![
            "###...##..#....###..###..####..##..#..#.",
            "#..#.#..#.#....#..#.#..#....#.#..#.#..#.",
            "#..#.#....#....#..#.###....#..#..#.#..#.",
            "###..#.##.#....###..#..#..#...####.#..#.",
            "#.#..#..#.#....#.#..#..#.#....#..#.#..#.",
            "#..#..###.####.#..#.###..####.#..#..##..",
        ]
    );
}

fn a0(s: &str) -> i64 {
    a(s).into_iter()
        .filter(|&(i, _)| i % 40 == 20)
        .take(6)
        .map(|(i, x)| i as i64 * x)
        .sum::<i64>()
}

fn a(s: &str) -> Vec<(usize, i64)> {
    let mut cycles: Vec<(usize, i64)> = vec![];
    let mut x: i64 = 1;
    let mut cycle: usize = 0;
    let run_cycle = |cycles: &mut Vec<(usize, i64)>, cycle: &mut usize, x: &mut i64| {
        *cycle += 1;
        cycles.push((*cycle, *x));
    };

    for row in parse(s) {
        run_cycle(&mut cycles, &mut cycle, &mut x);
        if let Row::AddX(i) = row {
            run_cycle(&mut cycles, &mut cycle, &mut x);
            x += i;
        }
    }

    cycles
}

fn b(s: &str) -> Vec<char> {
    struct C {
        drawing: Vec<char>,
        x: i64,
        cycle: usize,
    }

    let mut c = C {
        drawing: vec![],
        x: 1,
        cycle: 0,
    };

    fn run_cycle(c: &mut C) {
        c.cycle += 1;
        c.drawing.push(
            if ((c.x - 1)..=(c.x + 1)).contains(&(((c.cycle - 1) % 40) as i64)) {
                '#'
            } else {
                '.'
            },
        );
    }

    for row in parse(s) {
        run_cycle(&mut c);
        if let Row::AddX(i) = row {
            run_cycle(&mut c);
            c.x += i;
        }
    }

    c.drawing
}

fn parse(s: &str) -> impl Iterator<Item = Row> + '_ {
    read_parsed_with(s, |l| {
        if l == "noop" {
            Row::Noop
        } else {
            let v: Vec<_> = l.split(' ').collect();
            assert_eq!(v[0], "addx");
            let x = v[1].parse::<i64>().unwrap();
            Row::AddX(x)
        }
    })
}

#[derive(Copy, Clone, Debug)]
#[allow(dead_code)]
enum Row {
    Noop,
    AddX(i64),
}
