use aoc2020::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s07.txt"), 4);
    assert_eq!(a("txt/e07.txt"), 316);
    assert_eq!(b("txt/s07.txt"), 32);
    assert_eq!(b("txt/e07.txt"), 11310);
}

fn a(file: &str) -> usize {
    let bags = parse_file(file);
    let mut gold_count: HashSet<Hsh> = HashSet::new();
    for name in bags.keys() {
        contains_gold(*name, &bags, &mut gold_count, hash(&"shinygold".to_owned()));
    }
    gold_count.len()
}

fn b(file: &str) -> usize {
    let bags = parse_file(file);
    let mut bag_count: HashMap<Hsh, usize> = HashMap::new();
    count_bags(hash(&"shinygold".to_owned()), &bags, &mut bag_count) - 1
}

fn parse_file(file: &str) -> HashMap<Hsh, Bag> {
    read_parsed(file)
        .map(|Bag { name, sub_bags }| (name, Bag { name, sub_bags }))
        .collect()
}

fn contains_gold(
    name: Hsh,
    bags: &HashMap<Hsh, Bag>,
    gold_count: &mut HashSet<Hsh>,
    shiny_hash: Hsh,
) -> bool {
    if gold_count.contains(&name) {
        true
    } else {
        let bag = &bags.get(&name).unwrap();
        if bag.sub_bags.contains_key(&shiny_hash) {
            gold_count.insert(name.to_owned());
            true
        } else {
            for sub_name in bag.sub_bags.keys() {
                if contains_gold(*sub_name, bags, gold_count, shiny_hash) {
                    gold_count.insert(name.to_owned());
                    return true;
                }
            }
            false
        }
    }
}

fn count_bags(name: Hsh, bags: &HashMap<Hsh, Bag>, bag_count: &mut HashMap<Hsh, usize>) -> usize {
    match bag_count.get(&name) {
        Some(count) => *count,
        None => {
            let mut count = 1;
            for (sub_name, quantity) in &bags.get(&name).unwrap().sub_bags {
                count += quantity * count_bags(*sub_name, bags, bag_count);
            }
            bag_count.insert(name, count);
            count
        }
    }
}

#[derive(Debug)]
struct Bag {
    name: Hsh,
    sub_bags: HashMap<Hsh, usize>,
}

impl FromStr for Bag {
    type Err = ();
    fn from_str(s: &str) -> Result<Bag, ()> {
        let x: Vec<_> = s.split(' ').collect();
        let name = hash(&(x[0].to_owned() + x[1]));
        let sub_bags: HashMap<Hsh, usize> = x[4..]
            .join(" ")
            .split(',')
            .filter_map(|content| {
                let content: Vec<_> = content.trim().split(' ').collect();
                if content[0] == "no" {
                    None
                } else {
                    let quantity: usize = content[0].parse().unwrap();
                    let name = &content[1..=2];
                    Some((hash(&name.join("")), quantity))
                }
            })
            .collect();
        Ok(Bag { name, sub_bags })
    }
}
