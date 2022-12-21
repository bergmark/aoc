use std::collections::HashMap;
use std::fs;

fn read_file(fp: &str) -> String {
    fs::read_to_string(fp)
        .expect("Could not read file")
        .trim()
        .to_string()
}

fn main() {
    part1();
    part2();
}

pub fn part1() {
    let input = read_file("input.txt");

    let digits: Vec<u32> = input.chars().map(|c| c.to_digit(10).unwrap()).collect();

    let layers: Vec<&[u32]> = digits.chunks(25 * 6).collect();

    let mut count: Vec<HashMap<u32, u32>> = Vec::new();

    for layer in layers {
        assert!(layer.len() == 25 * 6);
        let mut h: HashMap<u32, u32> = HashMap::new();
        for digit in layer {
            if *digit <= 2 {
                h.entry(*digit).and_modify(|c| *c += 1).or_insert(1);
            }
        }
        count.push(h)
    }

    let min = count.iter().min_by(|a, b| a[&0].cmp(&b[&0])).unwrap();
    let answer = min[&1] * min[&2];
    println!("{} zeroes, {} * {} = {}", min[&0], min[&1], min[&2], answer)
}

pub fn part2() {
    let input = read_file("input.txt");

    let digits: Vec<u32> = input.chars().map(|c| c.to_digit(10).unwrap()).collect();

    let layers: Vec<&[u32]> = digits.chunks(25 * 6).collect();

    let mut res: [u32; 150] = [2; 25 * 6];

    for layer in layers {
        assert!(layer.len() == 25 * 6);
        for (digit, r) in layer.iter().zip(res.iter_mut()) {
            *r = match r {
                0 => 0,
                1 => 1,
                2 => *digit,
                i => panic!("Invalid digit {}", i),
            }
        }
    }

    for row in res.chunks(25) {
        println!(
            "{}",
            row.iter()
                .map(|d| {
                    match d {
                        0 => ' ',
                        1 => 'X',
                        i => panic!("Invalid color {}", i),
                    }
                })
                .collect::<String>()
        )
    }
}
