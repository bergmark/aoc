use aoc2020::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s09.txt", 5), 127);
    assert_eq!(a("txt/e09.txt", 25), 167829540);
    assert_eq!(b("txt/s09.txt", 127), 62);
    assert_eq!(b("txt/e09.txt", 167829540), 28045630);
}

fn a(filename: &str, preamble: usize) -> usize {
    let mut nums: Vec<usize> = vec![0; preamble];
    for (i, v) in read_parsed(filename).enumerate() {
        if i < preamble {
            nums[i] = v;
        } else {
            let mut found = false;
            'permute: for perm in nums.iter().permutations(2) {
                if perm[0] + perm[1] == v {
                    found = true;
                    break 'permute;
                }
            }
            if !found {
                return v;
            }
            nums[i % preamble] = v;
        }
    }
    unreachable!()
}

fn b(filename: &str, target_sum: usize) -> usize {
    let nums: Vec<usize> = read_parsed(filename).collect();
    'outer: for start in 0..nums.len() {
        let mut min = nums[start];
        let mut max = nums[start];
        let mut sum = nums[start];
        for n in nums.iter().skip(start + 1) {
            sum += n;
            min = std::cmp::min(min, *n);
            max = std::cmp::max(max, *n);
            match sum.cmp(&target_sum) {
                Ordering::Equal => return min + max,
                Ordering::Greater => continue 'outer,
                Ordering::Less => {}
            }
        }
    }
    unreachable!()
}
