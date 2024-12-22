use aoc2024::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s03.txt"), 161);
    assert_eq!(a("txt/e03.txt"), 170807108);
    assert_eq!(b("txt/s03b.txt"), 48);
    assert_eq!(b("txt/e03.txt"), 74838033);
}

fn a(s: &str) -> i64 {
    regex!(r#"mul\((\d+),(\d+)\)"#)
        .captures_iter(&read_to_string(s))
        .map(|cap| cap[1].to_string().parse::<i64>().unwrap() * cap[2].parse::<i64>().unwrap())
        .sum()
}

fn b(s: &str) -> i64 {
    let s = read_to_string(s);
    let caps = regex!(r#"(?:mul\((\d+),(\d+)\))|(?:(do(?:n't)?)\(\))"#).captures_iter(&s);
    let mut doing = true;
    let mut sum = 0;
    for cap in caps {
        if cap.get(3).is_some() {
            let d = cap[3].to_string();
            doing = d == "do";
        } else if doing {
            sum += cap[1].to_string().parse::<i64>().unwrap() * cap[2].parse::<i64>().unwrap();
        }
    }
    sum
}
