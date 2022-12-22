use aoc2022::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/e01.txt"), 525);
    assert_eq!(b("txt/s01d.txt"), 14);
    assert_eq!(b("txt/e01.txt"), 75749);
}

fn a(s: &str) -> i64 {
    let nums = parse(s);
    let mut sum = 0;
    for Num { sign, num } in nums {
        sum += match sign {
            '+' => num,
            '-' => -num,
            c => panic!("{c}"),
        };
    }
    sum
}

fn b(s: &str) -> i64 {
    let mut i = 0;
    let nums = parse(s);
    let mut sum = 0;
    let mut frequencies = HashSet::from([0]);
    loop {
        for Num { sign, num } in &nums {
            i += 1;
            if i % 1000 == 0 {
                dbg!(&i);
            }
            sum += match sign {
                '+' => *num,
                '-' => -num,
                c => panic!("{c}"),
            };
            if frequencies.contains(&sum) {
                return sum;
            }
            frequencies.insert(sum);
        }
    }
}

struct Num {
    sign: char,
    num: i64,
}

fn parse(s: &str) -> Vec<Num> {
    read_parsed_with(s, |s| {
        let sign = s.chars().next().unwrap();
        let num = s.chars().skip(1).join("");
        let num = num.parse::<i64>().unwrap();
        Num { sign, num }
    })
    .collect()
}
