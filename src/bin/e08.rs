use aoc2021::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s08b.txt"), 26);
    assert_eq!(a("txt/e08.txt"), 493);
    assert_eq!(b("txt/s08a.txt"), 5353);
    assert_eq!(b("txt/s08b.txt"), 61229);
    assert_eq!(b("txt/e08.txt"), 1010460);
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Line {
    inputs: Vec<Vec<char>>,
    outputs: Vec<Vec<char>>,
}

impl FromStr for Line {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, ()> {
        let (inputs, outputs) = split2(s, " | ")?;
        let inputs = inputs.split(' ').map(|s| s.chars().collect()).collect();
        let outputs = outputs.split(' ').map(|s| s.chars().collect()).collect();
        Ok(Line { inputs, outputs })
    }
}

fn a(s: &str) -> usize {
    read_parsed::<Line>(s)
        .into_iter()
        .map(|line| {
            line.outputs
                .into_iter()
                .filter(|output| {
                    [2, 4, 3, 7].contains(&output.len())
                    // output == &one || output == &four || output == &seven || output == &eight
                })
                .count()
        })
        .sum()
}

fn find(l: &Line, len: usize) -> impl Iterator<Item = char> + '_ {
    l.inputs
        .iter()
        .chain(l.outputs.iter())
        .find(|output| output.len() == len)
        .unwrap()
        .iter()
        .copied()
}

fn b(s: &str) -> usize {
    let lines: Vec<Line> = read_parsed(s).collect();

    let mut sum = 0;

    for line in lines {
        fn none() -> Option<Vec<char>> {
            None
        }
        let mut numbers = vec![none(); 10];

        fn all() -> Vec<char> {
            ('a'..='g').collect()
        }

        let one: Vec<_> = find(&line, 2).collect();
        numbers[1] = Some(one.clone());
        let four: Vec<_> = find(&line, 4).collect();
        numbers[4] = Some(four.clone());
        let seven: Vec<_> = find(&line, 3).collect();
        numbers[7] = Some(seven.clone());
        let eight = all();
        numbers[8] = Some(eight.clone());

        for input in line.inputs {
            if input.len() == 5 {
                if contains(&input, &one) {
                    numbers[3] = Some(input);
                } else if contains(&input, &remove_common(&four, &one)) {
                    numbers[5] = Some(input);
                } else {
                    numbers[2] = Some(input);
                }
            } else if input.len() == 6 {
                if !contains(&input, &one) {
                    numbers[6] = Some(input);
                } else if contains(&input, &four) {
                    numbers[9] = Some(input);
                } else {
                    numbers[0] = Some(input);
                }
            }
        }

        let mut new_numbers = vec![];
        for n in numbers.into_iter() {
            let mut n = n.unwrap();
            n.sort_unstable();
            new_numbers.push(n);
        }

        let mut value = "".to_owned();
        for output in line.outputs {
            let mut output = output.clone();
            output.sort_unstable();
            for (j, n) in new_numbers.iter().enumerate() {
                if &output == n {
                    value += &format!("{}", j);
                }
            }
        }

        dbg!(&value);
        sum += value.parse::<usize>().unwrap();
    }

    sum
}

fn contains(a: &[char], b: &[char]) -> bool {
    a.iter().filter(|c| b.contains(c)).count() == b.len()
}

fn remove_common(a: &[char], b: &[char]) -> Vec<char> {
    a.iter().filter(|c| !b.contains(c)).copied().collect()
}
