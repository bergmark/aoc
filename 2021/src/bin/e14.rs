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
    assert_eq!(b("txt/s14.txt", 1), count("NCNBCHB"));
    assert_eq!(b("txt/s14.txt", 2), count("NBCCNBBBCBHCB"));
    assert_eq!(b("txt/s14.txt", 3), count("NBBBCNCCNBBNBNBBCHBHHBCHB"));
    assert_eq!(
        b("txt/s14.txt", 4),
        count("NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB")
    );
    assert_eq!(b("txt/s14.txt", 40), 2188189693529);
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
    let mut insertions: BTreeMap<(char, char), char> = BTreeMap::new();
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
        insertions: BTreeMap<(char, char), char>,
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
            let mid = *insertions.get(&(a, b)).unwrap();
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

#[derive(Default, Debug)]
struct Insertions(BTreeMap<(char, char), char>);

impl Insertions {
    fn insert(&mut self, k: (char, char), v: char) {
        self.0.insert(k, v);
    }
    fn get(&self, k: (char, char)) -> char {
        *self.0.get(&k).unwrap()
    }
}

fn b(s: &str, max_iterations: usize) -> usize {
    let mut template: Vec<char> = vec![];
    let mut insertions = Insertions::default();
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
        //max_iterations: usize,
        count: Count<char>,
        first: bool,
        insertions: Insertions,
        //result: String,
    }

    let last = *template.last().unwrap();
    let template: Vec<_> = template
        .iter()
        .map(|c| (c, max_iterations))
        .zip(template.iter().skip(1))
        .map(|((a, iterations), b)| It {
            a: *a,
            iterations,
            b: *b,
        })
        .collect();
    let jobs: JobQueue<It, State> = JobQueue::new(
        State {
            //max_iterations,
            count: Count::default(),
            first: true,
            insertions,
            //result: String::new(),
        },
        template,
    );
    let State { mut count, /*mut result,*/ .. } = jobs.prepend_run_rev(|it: It, state: &mut State| {
        let It { a, iterations, b } = it;
        //println!("run: {} {} {}", a, iterations, b);
        let State {
            //max_iterations,
            count,
            first,
            insertions,
            /*result,*/
        } = state;

        if iterations >= 22 {
            println!("{} {}", a, iterations);
        }

        //result.push(a);
        count.count(a);
        get_intermediates(a, iterations, b, &insertions)
    });

    //result.push(last);
    count.count(last);

    //(result,
    count.max().unwrap().1 - count.min().unwrap().1
}

#[derive(Debug)]
struct It {
    a: char,
    iterations: usize,
    b: char,
}

fn get_intermediates(a: char, iterations: usize, mut b: char, insertions: &Insertions) -> Vec<It> {
    if iterations == 0 {
        return vec![]
    }

    //println!("intermediates: {} {} {}", a, iterations, b);
    let mut res = vec![];
    for i in 1..=iterations {
        let next = b;
        b = insertions.get((a, b));
        res.push(It {
            a: b,
            b: next,
            iterations: iterations - i,
        });
    }
    assert_eq!(res.len(), iterations);
    //println!("Adding {:?}", res);
    res
}
