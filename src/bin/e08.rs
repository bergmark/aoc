use aoc2021::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s08b.txt"), 26);
    assert_eq!(a("txt/e08.txt"), 493);
    assert_eq!(b("txt/s08a.txt"), 5353);
    assert_eq!(b("txt/s08b.txt"), 61229);
    assert_eq!(b("txt/e08.txt"), 1010460);
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct Line {
    inputs: Vec<Vec<char>>,
    outputs: Vec<Vec<char>>,
}

impl FromStr for Line {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, ()> {
        let (inputs, outputs) = split2(s, " | ")?;
        let inputs = inputs.split(" ").map(|s| s.chars().collect()).collect();
        let outputs = outputs.split(" ").map(|s| s.chars().collect()).collect();
        Ok(Line { inputs, outputs })
    }
}

fn a(s: &str) -> usize {
    let lines: Vec<Line> = read_parsed(s).collect();

    lines
        .into_iter()
        .map(|line| {
            line.outputs
                .into_iter()
                .filter(|output| {
                    [2, 4, 3, 7].contains(&output.len())
                    // output == &one || output == &four || output == &seven || output == &eight
                })
                .count()
        })
        .sum()
}

fn find(l: &Line, len: usize) -> Vec<char> {
    l.inputs
        .iter()
        .chain(l.outputs.iter())
        .find(|output| output.len() == len)
        .unwrap()
        .into_iter()
        .copied()
        .collect()
}

fn b(s: &str) -> usize {
    let lines: Vec<Line> = read_parsed(s).collect();

    let mut sum = 0;

    for line in lines {
        fn none() -> Option<Vec<char>> {
            None
        }
        let mut numbers = vec![none(); 10];

        fn all() -> Vec<char> {
            ('a'..='g').collect()
        }

        let one = find(&line, 2);
        numbers[1] = Some(one.clone());
        let four = find(&line, 4);
        numbers[4] = Some(four.clone());
        let seven = find(&line, 3);
        numbers[7] = Some(seven.clone());
        let eight = all();
        numbers[8] = Some(eight.clone());

        for input in line.inputs {
            if input.len() == 5 {
                if contains(&input, &one) {
                    numbers[3] = Some(input);
                } else if contains(&input, &remove_common(&four, &one)) {
                    numbers[5] = Some(input);
                } else {
                    numbers[2] = Some(input);
                }
            } else if input.len() == 6 {
                if !contains(&input, &one) {
                    numbers[6] = Some(input);
                } else if contains(&input, &four) {
                    numbers[9] = Some(input);
                } else {
                    numbers[0] = Some(input);
                }
            }
        }

        let mut new_numbers = vec![];
        for n in numbers.into_iter() {
            let mut n = n.unwrap();
            n.sort();
            new_numbers.push(n);
        }

        let mut value = "".to_owned();
        for output in line.outputs {
            let mut output = output.clone();
            output.sort();
            for (j, n) in new_numbers.iter().enumerate() {
                if &output == n {
                    value += &format!("{}", j);
                }
            }
        }

        dbg!(&value);
        sum += value.parse::<usize>().unwrap();
    }

    sum
}

fn contains(a: &[char], b: &[char]) -> bool {
    a.iter().filter(|c| b.contains(c)).count() == b.len()
}

fn remove_common(a: &[char], b: &[char]) -> Vec<char> {
    a.iter().filter(|c| !b.contains(c)).copied().collect()
}

/*
fn b(s: &str) -> usize {
    let lines: Vec<Line> = read_parsed(s).collect();

    use Position::*;

    let number_positions = vec![
        /* 0 */ vec![Top, TopRight, BottomRight, Bottom, BottomLeft, TopLeft],
        /* 1 */ vec![TopRight, BottomRight],
        /* 2 */ vec![Top, TopRight, Middle, BottomLeft, Bottom],
        /* 3 */ vec![Top, TopRight, Middle, BottomRight, Bottom],
        /* 4 */ vec![TopLeft, TopRight, Middle, BottomRight],
        /* 5 */ vec![Top, TopLeft, Middle, BottomRight, Bottom],
        /* 6 */ vec![Top, TopLeft, Middle, BottomRight, Bottom, BottomLeft],
        /* 7 */ vec![Top, TopRight, BottomRight],
        /* 8 */
        vec![
            Top,
            TopRight,
            Middle,
            BottomRight,
            Bottom,
            BottomLeft,
            TopLeft,
        ],
        /* 9 */ vec![Top, TopLeft, TopRight, Middle, BottomRight, Bottom],
    ];

    for line in lines {
        fn all() -> BTreeSet<char> {
            ('a'..='g').collect()
        }

        let mut ns = vec![all(); 10];
        let mut possible = new_possible();

        debug("start", &ns, &possible);

        ns[1] = find(&line, 2);
        assert_eq!(ns[1].len(), 2);

        ns[4] = find(&line, 4);
        assert_eq!(ns[4].len(), 4);

        ns[7] = find(&line, 3);
        assert_eq!(ns[7].len(), 3);

        ns[8] = all();
        assert_eq!(ns[8].len(), 7);

        debug("alpha", &ns, &possible);

        for (i, positions) in number_positions.iter().enumerate() {
            for position in positions {
                foo(&mut possible, *position, &mut ns, i);
            }
        }

        debug("beta", &ns, &possible);

        // These positions cannot contain any value that the 1s contain
        for pos in [Top, TopLeft, Middle, BottomLeft, Bottom] {
            possible.insert(pos, possible[&pos].difference(&ns[1]).copied().collect());
        }
        assert_eq!(possible[&Top].len(), 1);

        debug("gamma", &ns, &possible);

        // -||- 7s contain
        for pos in [TopLeft, Middle, BottomLeft, Bottom] {
            possible.insert(pos, possible[&pos].difference(&ns[1]).copied().collect());
        }

        debug("gamma2", &ns, &possible);

        // -||- 4s contain
        for pos in [Top, BottomLeft, Bottom] {
            possible.insert(pos, possible[&pos].difference(&ns[1]).copied().collect());
        }

        debug("gamma3", &ns, &possible);



        possible.insert(Bottom, possible[&Bottom].difference(&possible[&TopLeft]).copied().collect());
        possible.insert(BottomLeft, possible[&BottomLeft].difference(&possible[&TopLeft]).copied().collect());

        debug("delta", &ns, &possible);

        // Top is now unique
        for pos in Position::all().filter(|&p| p != Top) {
            possible.insert(
                pos,
                possible[&pos]
                    .difference(&possible[&Top])
                    .copied()
                    .collect(),
            );
        }

        debug("epsilon", &ns, &possible);
    }
    1
}

fn foo(possible: &mut Possible, pos: Position, ns: &mut Vec<BTreeSet<char>>, n: usize) {
    let intersection: BTreeSet<char> = possible[&pos].intersection(&ns[n]).copied().collect();
    possible.insert(pos, intersection.clone());
    //ns[n] = intersection;
}

fn debug(s: &str, ns: &[BTreeSet<char>], possible: &Possible) {
    println!("{}", s);
    for (i, n) in ns.iter().enumerate() {
        print!("{}:{} ", i, n.iter().collect::<String>());
    }
    println!();
    for (k, v) in possible {
        print!("{}:{} ", k, v.iter().collect::<String>());
    }
    println!();
}

fn b(s: &str) -> usize {
    let lines: Vec<Line> = read_parsed(s).collect();

    use Position::*;

    for line in lines {
        let mut possible = Possible::new();
        possible.insert(
        let one = find(&line, 2);
        let four = find(&line, 4);
        let seven = find(&line, 3);
        let eight = find(&line, 7);

        possible.insert(TopRight, intersect([one, four, seven, eight]));
        //        possible.insert(
        //            BottomRight,
        //            intersect([four, seven, eight].iter().map(|i| i.iter()))
        //                .copied()
        //                .collect(),
        //        );
        //        possible.insert(
        //            Top,
        //            intersect([seven, eight].iter().map(|i| i.iter()))
        //                .collect::<BTreeSet<_>>()
        //                .difference(
        //                    &intersect([one, four].iter().map(|i| i.iter()))
        //                        .copied()
        //                        .collect(),
        //                )
        //                .copied()
        //                .collect(),
        //        );

        for pos in Position::all() {
            match pos {
                Top => {
                    let u = union_vec(
                        number_positions
                            .iter()
                            .filter(|v| v.contains(&Top))
                    );
                    possible[&Top] = intersect([
                        possible[&Top],
                        u
                    ].into_iter());
                }
                _ => todo!(),
            }
        }
    }

    1
}
*/
/*
type Alternatives = BTreeSet<char>;

type Possible = BTreeMap<Position, Alternatives>;

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Debug)]
enum Position {
    Top,
    TopRight,
    BottomRight,
    Bottom,
    BottomLeft,
    TopLeft,
    Middle,
}

impl Position {
    fn all() -> impl Iterator<Item = Position> {
        use Position::*;
        [
            Top,
            TopRight,
            BottomRight,
            Bottom,
            BottomLeft,
            TopLeft,
            Middle,
        ]
        .into_iter()
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Position::*;
        write!(
            f,
            "{}",
            match self {
                Top => "TT",
                TopRight => "TR",
                BottomRight => "BR",
                Bottom => "BB",
                BottomLeft => "BL",
                TopLeft => "TL",
                Middle => "MM",
            }
        )
    }
}

fn new_possible() -> Possible {
    use Position::*;
    let poss = ('a'..='g').collect::<BTreeSet<_>>();
    BTreeMap::from([
        (Top, poss.clone()),
        (TopRight, poss.clone()),
        (BottomRight, poss.clone()),
        (Bottom, poss.clone()),
        (BottomLeft, poss.clone()),
        (TopLeft, poss.clone()),
        (Middle, poss.clone()),
    ])
}
*/
