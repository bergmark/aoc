use aoc2023::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s06.txt"), 288);
    assert_eq!(a("txt/e06.txt"), 800280);
    assert_eq!(a("txt/e06b.txt"), 0);
}

#[derive(Debug, Copy, Clone)]
struct S {
    time: usize,
    distance: usize,
}

fn digs(s: &str) -> Vec<usize> {
    regex!(r#" +"#)
        .split(s.trim())
        .map(|s| s.parse().ok().unwrap_or_else(|| panic!("{s:?}")))
        .collect()
}

fn parse(s: &str) -> Vec<S> {
    let lines: Vec<String> = read_parsed::<String>(s).collect();
    let times: Vec<usize> = digs(&lines[0].split(':').nth(1).unwrap());
    let distances: Vec<usize> = digs(&lines[1].split(':').nth(1).unwrap());
    assert_eq!(times.len(), distances.len());
    let mut res = vec![];
    for (time, distance) in times.into_iter().zip(distances.into_iter()) {
        res.push(S { time, distance });
    }
    res
}

fn a(s: &str) -> usize {
    let s = parse(s);

    let mut res = 1;
    for S { time, distance } in s {
        let mut ways_to_win = 0;
        for pushes in 1..time {
            let speed = pushes;
            let rem_time = time - pushes;
            assert!(rem_time > 0 && rem_time < distance);
            let my_distance = speed * rem_time;
            if my_distance > distance {
                ways_to_win += 1;
            } else {
            }
        }
        res *= ways_to_win;
    }
    res
}

fn b(s: &str) -> usize {
    let s = parse(s);

    let mut res = 1;
    for S { time, distance } in s {
        let mut ways_to_win = 0;
        for pushes in 1..time {
            let speed = pushes;
            let rem_time = time - pushes;
            assert!(rem_time > 0 && rem_time < distance);
            let my_distance = speed * rem_time;
            if my_distance > distance {
                ways_to_win += 1;
            } else {
            }
        }
        res *= ways_to_win;
    }
    res
}
