use aoc2023::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s01.txt"), 142);
    assert_eq!(a("txt/e01.txt"), 54605);
    assert_eq!(b("txt/s01b.txt"), 281);
    assert_eq!(b("txt/e01.txt"), 55429);
}

fn a(s: &str) -> usize {
    read_parsed::<String>(s)
        .map(|line| {
            line.chars()
                .filter_map(|c| c.to_string().parse::<usize>().ok())
                .collect()
        })
        .map(|ns: Vec<usize>| ns[0] * 10 + ns[ns.len() - 1])
        .sum()
}

fn b(s: &str) -> usize {
    let arr: [&str; 10] = [
        "zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
    ];
    read_parsed::<String>(s)
        .map(|line| {
            let cs: Vec<char> = line.chars().collect();
            cs.iter()
                .enumerate()
                .filter_map(|(i, &c)| {
                    if let Some(digit) = c.to_digit(10) {
                        Some(digit as usize)
                    } else {
                        let rest: &str = &cs[i..].iter().collect::<String>();
                        if let Some(digstr) = arr.iter().find(|digstr| rest.starts_with(**digstr)) {
                            arr.iter().position(|v| digstr == v)
                        } else {
                            None
                        }
                    }
                })
                .collect()
        })
        .map(|ns: Vec<usize>| ns[0] * 10 + ns[ns.len() - 1])
        .sum()
}
