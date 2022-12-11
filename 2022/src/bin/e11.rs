use aoc2022::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s11.txt", 20, true), 10605);
    assert_eq!(a("txt/e11.txt", 20, true), 62491);
    assert_eq!(a("txt/s11.txt", 10_000, false), 2713310158);
    assert_eq!(a("txt/e11.txt", 10_000, false), 17408399184);
}

fn a(s: &str, iters: usize, div: bool) -> usize {
    let mut monkeys = parse(s);

    for _ in 0..iters {
        for i in 0..monkeys.len() {
            let curr = monkeys[i].clone();
            let Operation { lhs, op, rhs } = curr.operation;

            // Shouldn't be pop, but doesn't matter.
            while let Some(mut item) = monkeys[i].items.pop() {
                match (lhs, op, rhs) {
                    (lhs, '+', rhs) => item = lhs.eval(item) + rhs.eval(item),
                    (lhs, '*', rhs) => item = lhs.eval(item) * rhs.eval(item),
                    (lhs, op, rhs) => panic!("Unhandled operation {lhs:?} {op:?} {rhs:?}"),
                }

                if div {
                    item /= 3;
                } else {
                    item %= monkeys.iter().map(|m| m.test).product::<u64>();
                }

                let j = if item % curr.test == 0 {
                    curr.if_true
                } else {
                    curr.if_false
                };
                monkeys[j].items.push(item);
                monkeys[i].inspections += 1;
            }
        }
    }

    let mut m: Vec<_> = monkeys.into_iter().map(|m| m.inspections).collect();
    m.sort_unstable();
    m.into_iter().rev().take(2).product::<usize>()
}

#[allow(dead_code)]
#[derive(Clone, Debug)]
struct Row {
    monkey: usize,
    items: Vec<u64>,
    operation: Operation,
    test: u64,
    if_true: usize,
    if_false: usize,
    inspections: usize,
}
#[derive(Clone, Debug, Copy)]
struct Operation {
    lhs: X,
    op: char,
    rhs: X,
}
#[derive(Copy, Clone, Debug)]
enum X {
    Old,
    Num(u64),
}
impl X {
    fn eval(self, old: u64) -> u64 {
        match self {
            X::Old => old,
            X::Num(n) => n,
        }
    }
}

impl FromStr for X {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, ()> {
        if s == "old" {
            Ok(X::Old)
        } else {
            Ok(X::Num(s.parse::<u64>().unwrap()))
        }
    }
}

fn parse(s: &str) -> Vec<Row> {
    let v: String = read_to_string(s);

    v.split("\n\n")
        .enumerate()
        .map(|(monkey, v): (usize, &str)| {
            let v: Vec<_> = v.split('\n').collect();
            let items: Vec<u64> = Captures::new(regex!("(\\d[\\d, ]+)$"), v[1])
                .unwrap()
                .get::<String>(1)
                .unwrap()
                .split(", ")
                .map(|v| v.parse().unwrap())
                .collect();
            let operation = Captures::new(regex!("(old|\\d+) ([*+]) (old|\\d+)$"), v[2]).unwrap();
            let operation = Operation {
                lhs: operation.parse(1).unwrap(),
                op: operation.parse(2).unwrap(),
                rhs: operation.parse(3).unwrap(),
            };
            let test = Captures::new(regex!("(\\d+)$"), v[3])
                .unwrap()
                .parse(1)
                .unwrap();
            let if_true = Captures::new(regex!("(\\d+)$"), v[4])
                .unwrap()
                .parse(1)
                .unwrap();
            let if_false = Captures::new(regex!("(\\d+)$"), v[5])
                .unwrap()
                .parse(1)
                .unwrap();
            Row {
                monkey,
                items,
                operation,
                test,
                if_true,
                if_false,
                inspections: 0,
            }
        })
        .collect()
}
