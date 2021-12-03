use aoc2021::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s03.txt"), 198);
    assert_eq!(a("txt/e03.txt"), 3242606);
    assert_eq!(b("txt/s03.txt"), 230);
    assert_eq!(b("txt/e03.txt"), 4856080);
}

fn a(s: &str) -> i64 {
    let mut count: Vec<Count<char>> = vec![];

    for s in read_parsed::<String>(s) {
        for (i, c) in s.chars().enumerate() {
            if count.len() == i {
                count.push(Count::default());
            }
            count[i].count(c)
        }
    }

    let gamma: String = count
        .into_iter()
        .map(|count| *count.max().unwrap().0)
        .collect();
    let epsilon: String = gamma
        .chars()
        .map(|c| if c == '0' { '1' } else { '0' })
        .collect();

    let gamma = i64::from_str_radix(&gamma, 2).unwrap();
    let epsilon = i64::from_str_radix(&epsilon, 2).unwrap();

    gamma * epsilon
}

fn b(s: &str) -> i64 {
    let input: Vec<String> = read_parsed::<String>(s).collect();

    let mut next_oxygen = input.clone();

    let mut char_i = 0;
    while next_oxygen.len() > 1 {
        let oxygen = next_oxygen;
        next_oxygen = vec![];

        let count = count_bits(&oxygen);

        let dig = if count[char_i].maxes().unwrap().0.len() == 2 {
            '1'
        } else {
            *count[char_i].max().unwrap().0
        };

        for l in oxygen {
            if l.chars().nth(char_i).unwrap() == dig {
                println!("keep {}", l);
                next_oxygen.push(l)
            }
        }

        char_i += 1;
    }

    let mut next_co2 = input.clone();

    let mut char_i = 0;
    while next_co2.len() > 1 {
        let co2 = next_co2;
        next_co2 = vec![];

        let count = count_bits(&co2);

        let dig = if count[char_i].mins().unwrap().0.len() == 2 {
            '0'
        } else {
            *count[char_i].min().unwrap().0
        };

        for l in co2 {
            if l.chars().nth(char_i).unwrap() == dig {
                println!("keep {}", l);
                next_co2.push(l)
            }
        }

        char_i += 1;
    }

    let oxygen = i64::from_str_radix(&next_oxygen[0], 2).unwrap();
    let co2 = i64::from_str_radix(&next_co2[0], 2).unwrap();
    oxygen * co2
}

fn count_bits(v: &[String]) -> Vec<Count<char>> {
    let mut count = vec![];
    for s in v {
        for (i, c) in s.chars().enumerate() {
            if count.len() == i {
                count.push(Count::default());
            }
            count[i].count(c)
        }
    }
    count
}
