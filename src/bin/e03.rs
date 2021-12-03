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
    let count: Vec<Count<char>> = count_bits(&read_parsed::<String>(s).collect::<Vec<_>>());

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

    let oxygen = filter(input.clone(), '1', Ordering::Greater);
    let co2 = filter(input, '0', Ordering::Less);

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

fn filter(mut next_co2: Vec<String>, def: char, ord: Ordering) -> i64 {
    let mut char_i = 0;
    while next_co2.len() > 1 {
        let co2 = next_co2;
        next_co2 = vec![];

        let count = count_bits(&co2);

        let picked = count[char_i].pick(ord).unwrap().0;

        let dig = if picked.len() == 2 { def } else { *picked[0] };

        for l in co2 {
            if l.chars().nth(char_i).unwrap() == dig {
                next_co2.push(l)
            }
        }

        char_i += 1;
    }
    i64::from_str_radix(&next_co2[0], 2).unwrap()
}
