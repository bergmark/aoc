use aoc2021::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s14.txt"), 1588);
    assert_eq!(a("txt/e14.txt"), 2975);
    // NNCB
    // assert_eq!(b("txt/s14.txt", 1).0, "NCNBCHB");
    // assert_eq!(b("txt/s14.txt", 2).0, "NBCCNBBBCBHCB");
    // assert_eq!(b("txt/s14.txt", 3).0, "NBBBCNCCNBBNBNBBCHBHHBCHB");
    // assert_eq!(
    //     b("txt/s14.txt", 4).0,
    //     "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"
    // );
    //assert_eq!(b("txt/s14.txt", 1), count("NCNBCHB"));
    assert_eq!(b("txt/s14.txt", 2), count("NBCCNBBBCBHCB"));
    //    assert_eq!(b("txt/s14.txt", 3), count("NBBBCNCCNBBNBNBBCHBHHBCHB"));
    //    assert_eq!(
    //        b("txt/s14.txt", 4),
    //        count("NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB")
    //    );
    //assert_eq!(b("txt/s14.txt", 28), 2188189693529);
}

fn count(s: &str) -> usize {
    let mut count = Count::default();
    for c in s.chars() {
        count.count(c)
    }
    count.max().unwrap().1 - count.min().unwrap().1
}

#[derive(Debug, Clone)]
enum Line {
    Template(Vec<char>),
    Empty,
    Insertion(Insertion),
}

#[derive(Debug, Clone)]
struct Insertion {
    pair: (char, char),
    val: char,
}

impl FromStr for Line {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, ()> {
        if s.is_empty() {
            Ok(Line::Empty)
        } else if s.contains('-') {
            let (pair, val) = split2(s, " -> ")?;
            assert_eq!(pair.len(), 2);
            let a = pair.chars().next().unwrap();
            let b = pair.chars().nth(1).unwrap();
            let pair = (a, b);
            assert_eq!(val.len(), 1);
            let val = val.chars().next().unwrap();
            Ok(Line::Insertion(Insertion { pair, val }))
        } else {
            Ok(Line::Template(s.chars().collect()))
        }
    }
}

fn a(s: &str) -> usize {
    sol(s, 10)
}

fn sol(s: &str, iterations: usize) -> usize {
    let mut template: Vec<char> = vec![];
    let mut insertions = Insertions::new();
    for line in read_parsed::<Line>(s) {
        match line {
            Line::Template(t) => {
                template = t;
            }
            Line::Insertion(Insertion { pair, val }) => {
                insertions.insert(pair, val);
            }
            Line::Empty => {}
        }
    }

    struct State {
        insertions: Insertions,
        template: Vec<char>,
        i: usize,
        iterations: usize,
    }

    let jobs: JobQueue<(), State> = JobQueue::new(
        State {
            insertions,
            template,
            i: 0,
            iterations,
        },
        vec![()],
    );
    let State { template, .. } = jobs.run(|(), state: &mut State| {
        let State {
            template,
            i,
            insertions,
            iterations,
        } = state;
        let mut new_template: Vec<char> = vec![];

        for (&a, &b) in template.iter().zip(template.iter().skip(1)) {
            let mid = insertions.get((a, b));
            //println!("{} {} -> {}", a, b, mid);
            new_template.push(a);
            new_template.push(mid);
        }
        new_template.push(*template.last().unwrap());

        state.template = new_template;
        //println!("{}", state.template.iter().collect::<String>());
        *i += 1;
        if *i < *iterations {
            vec![()]
        } else {
            vec![]
        }
    });

    // println!("{}", &template.iter().collect::<String>());

    let mut count = Count::default();
    for c in template {
        count.count(c);
    }

    count.max().unwrap().1 - count.min().unwrap().1
}

struct Insertions {
    v: Vec<Option<char>>,
}

impl Insertions {
    fn new() -> Insertions {
        Insertions {
            v: vec![None; 26 * 26],
        }
    }
    fn insert(&mut self, k: (char, char), v: char) {
        self.v[k.bucket()] = Some(v);
    }
    fn get(&self, k: (char, char)) -> char {
        self.v[k.bucket()].unwrap()
    }
}

fn b(s: &str, max_iterations: usize) -> usize {
    let mut template: Vec<char> = vec![];
    let mut insertions = Insertions::new();
    let mut pairs: BTreeSet<(char, char)> = Default::default();
    for line in read_parsed::<Line>(s) {
        match line {
            Line::Template(t) => {
                template = t;
            }
            Line::Insertion(Insertion { pair, val }) => {
                insertions.insert(pair, val);
                pairs.insert(pair);
            }
            Line::Empty => {}
        }
    }

    let mut count = CountBucket::new();
    for c in &template {
        count.count(c);
    }

    let mut cache: BTreeMap<It, CountBucket<char>> = BTreeMap::new();

    for iterations in 1..=max_iterations {
        for &(a, b) in &pairs {
            let it = It { a, b, iterations };
            let mid = insertions.get((a, b));
            let mut count_bucket = CountBucket::new();
            if iterations == 1 {
                println!("{}{} 1 -> {}", a, b, mid);
                count_bucket.count(&mid);
            } else {
                count_bucket.extend(
                    cache
                        .get(&It {
                            a,
                            b: mid,
                            iterations: iterations - 1,
                        })
                        .unwrap(),
                );
                count_bucket.extend(
                    cache
                        .get(&It {
                            a: mid,
                            b,
                            iterations: iterations - 1,
                        })
                        .unwrap(),
                );
            }
            cache.insert(it, count_bucket);
        }
    }

   dbg!(&cache);

    for pair in template.windows(2) {
        let it = It { a: pair[0], b : pair[1], iterations: max_iterations };
        dbg!(&it);
        if let Some(entry) = cache.get(&it) {
            count.extend(entry);
        } else {
            panic!("Cache lookup failure {:?}", it);
        }
    }

    dbg!(&count);

    count.max() - count.min()
}

#[derive(Debug, PartialOrd, Ord, PartialEq, Eq)]
struct It {
    a: char,
    iterations: usize,
    b: char,
}
