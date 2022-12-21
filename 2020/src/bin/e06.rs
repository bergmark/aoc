use aoc2020::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s06.txt"), 11);
    assert_eq!(a("txt/e06.txt"), 6335);
    assert_eq!(b("txt/s06.txt"), 6);
    assert_eq!(b("txt/e06.txt"), 3392);
}

fn a(file: &str) -> usize {
    let mut total: usize = 0;
    let mut current_group: HashSet<char> = Default::default();
    for line in read_parsed::<String, _>(file) {
        if line.is_empty() {
            total += current_group.len();
            current_group = Default::default();
        } else {
            for c in line.chars() {
                current_group.insert(c);
            }
        }
    }
    current_group.len() + total
}

fn b(file: &str) -> usize {
    let mut total = 0;
    let mut current_group_count = 0;
    let mut current_group: Count<char> = Default::default();
    for line in read_parsed::<String, _>(file) {
        if line.is_empty() {
            total += current_group
                .counts()
                .filter(|&x| x == current_group_count)
                .count();
            current_group = Default::default();
            current_group_count = 0;
        } else {
            current_group_count += 1;
            for c in line.chars() {
                current_group.count(c)
            }
        }
    }
    current_group
        .counts()
        .filter(|&x| x == current_group_count)
        .count()
        + total
}
