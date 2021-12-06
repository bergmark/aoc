use aoc2021::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(sol("txt/s06.txt", 18), 26);
    assert_eq!(sol("txt/s06.txt", 80), 5934);
    assert_eq!(sol("txt/e06.txt", 80), 350605);
    assert_eq!(sol("txt/s06.txt", 256), 26984457539);
    assert_eq!(sol("txt/e06.txt", 256), 1592778185024);
}

fn sol(s: &str, n: usize) -> usize {
    let input: Vec<String> = read_parsed::<String>(s).collect();
    let input = input[0].clone();
    let input: Vec<usize> = input.split(',').map(|s| s.parse().unwrap()).collect();

    let mut z: Vec<usize> = vec![0; 9];
    for i in input {
        z[i] += 1;
    }

    for _ in 1..=n {
        let new = z[0];
        for zi in 0..(z.len() - 1) {
            z[zi] = z[zi + 1];
        }
        z[6] += new;
        z[8] = new;
    }

    z.iter().sum()
}
