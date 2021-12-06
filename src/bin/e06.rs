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
    let mut z: [usize; 9] = [0; 9];
    let z_len = z.len() - 2;
    for i in read_lines(s)
        .next()
        .unwrap()
        .unwrap()
        .split(',')
        .map(|s| s.parse::<usize>().unwrap())
    {
        z[i] += 1;
    }

    for i in 0..n {
        let new = z[i % z_len];
        z[i % z_len] = 0;
        z[(i + z_len) % z_len] += new + z[7];
        z[7] = z[8];
        z[8] = new;
    }

    z.iter().sum()
}
