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
    let gamma: String = count_bits(&read_parsed::<String>(s).collect::<Vec<_>>())
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

fn count_bits(v: &[String]) -> impl Iterator<Item = Count<char>> + '_ {
    let char_count = v[0].len();

    (0..char_count).map(move |i| count_bit(v, i))
}

fn b(s: &str) -> i64 {
    let input: Vec<String> = read_parsed::<String>(s).collect();

    let oxygen = filter(input.clone(), '1', Ordering::Greater);
    let co2 = filter(input, '0', Ordering::Less);

    oxygen * co2
}

fn count_bit(v: &[String], bit: usize) -> Count<char> {
    let mut count = Count::default();
    for s in v {
        count.count(s.chars().nth(bit).unwrap());
    }
    count
}

fn filter(mut kept: Vec<String>, def: char, ord: Ordering) -> i64 {
    let mut char_i = 0;
    while kept.len() > 1 {
        let prev_kept = kept;
        kept = vec![];

        let count = count_bit(&prev_kept, char_i);
        let picked = count.pick(ord).unwrap().0;

        let dig = if picked.len() == 2 { def } else { *picked[0] };

        for l in prev_kept {
            if l.chars().nth(char_i).unwrap() == dig {
                kept.push(l)
            }
        }

        char_i += 1;
    }
    i64::from_str_radix(&kept[0], 2).unwrap()
}
