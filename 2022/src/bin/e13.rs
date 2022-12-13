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
        .filter(|(_, v)| [&two, &six].iter().contains(&v))
        .map(|(i, _)| i + 1)
        .product()
}

fn check(a: &Value, b: &Value) -> Ordering {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => a.as_i64().unwrap().cmp(&b.as_i64().unwrap()),
        (Value::Array(a), Value::Array(b)) => {
            for (a, b) in a.iter().zip(b.iter()) {
                let r = check(a, b);
                if r != Ordering::Equal {
                    return r;
                }
            }
            a.len().cmp(&b.len())
        }
        (Value::Number(a), Value::Array(b)) => check(
            &Value::Array(vec![Value::Number(a.clone())]),
            &Value::Array(b.to_vec()),
        ),
        (Value::Array(a), Value::Number(b)) => check(
            &Value::Array(a.to_vec()),
            &Value::Array(vec![Value::Number(b.clone())]),
        ),
        (_, _) => todo!(),
    }
}

fn parse(s: &str) -> Vec<Value> {
    read_lines(s)
        .filter_map(|l| serde_json::from_str::<Value>(&l.unwrap()).ok())
        .collect()
}
