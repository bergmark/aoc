use aoc2022::*;
use serde_json::{json, Value};

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s13.txt"), 13);
    assert_eq!(a("txt/e13.txt"), 6101);
    assert_eq!(b("txt/s13.txt"), 140);
    assert_eq!(b("txt/e13.txt"), 21909);
}

fn a(s: &str) -> usize {
    parse(s)
        .chunks(2)
        .enumerate()
        .filter_map(|(i, v)| (check(&v[0], &v[1]) == Ordering::Less).then_some(i + 1))
        .sum()
}

fn b(s: &str) -> usize {
    let mut vec: Vec<Value> = parse(s);
    let two = json!([[2]]);
    let six = json!([[6]]);
    vec.push(two.clone());
    vec.push(six.clone());
    vec.sort_by(check);

    vec.into_iter()
        .enumerate()
        .filter_map(|(i, v)| [&two, &six].iter().contains(&&v).then_some(i + 1))
        .product()
}

fn check(a: &Value, b: &Value) -> Ordering {
    use Value::*;
    match (a, b) {
        (Number(a), Number(b)) => a.as_i64().unwrap().cmp(&b.as_i64().unwrap()),
        (Array(a), Array(b)) => check_vec(a, b),
        (Number(a), Array(b)) => check_vec(&[Number(a.clone())], b),
        (Array(a), Number(b)) => check_vec(a, &[Number(b.clone())]),
        (_, _) => todo!(),
    }
}

fn check_vec(a: &[Value], b: &[Value]) -> Ordering {
    for (a, b) in a.iter().zip(b.iter()) {
        let r = check(a, b);
        if r != Ordering::Equal {
            return r;
        }
    }
    a.len().cmp(&b.len())
}

fn parse(s: &str) -> Vec<Value> {
    read_lines(s)
        .filter_map(|l| serde_json::from_str::<Value>(&l.unwrap()).ok())
        .collect()
}
