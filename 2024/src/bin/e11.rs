fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("0 1 10 99 999", 1).len(), 7);
    assert_eq!(a("125 17", 25).len(), 55312);
    assert_eq!(a("28591 78 0 3159881 4254 524155 598 1", 25).len(), 220722);
}

fn a(s: &str, blinks: usize) -> Vec<i64> {
    //println!("\n###\n");
    let mut stones: Vec<i64> = s.split(' ').map(|s| s.parse::<i64>().unwrap()).collect();
    for _ in 0..blinks {
        blink(&mut stones);
    }
    stones
}

fn blink(stones: &mut Vec<i64>) {
    //println!("== {}", stones.iter().join(","));
    let mut i = 0;
    while i < stones.len() {
        if stones[i] == 0 {
            //println!("0 -> 1");
            stones[i] = 1;
            i += 1;
        } else {
            let s = stones[i].to_string();
            if s.len() % 2 == 0 {
                let mid = s.len() / 2;
                stones[i] = s[0..mid].parse().unwrap();
                stones.insert(i + 1, s[mid..s.len()].parse().unwrap());
                //println!("split {s} into {},{}", stones[i], stones[i + 1]);
                i += 2;
            } else {
                //let v = stones[i];
                stones[i] *= 2024;
                //println!("{v} * 2024 = {}", stones[i]);
                i += 1;
            }
        }
    }
    //println!("== {}", stones.iter().join(","));
}
