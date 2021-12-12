use aoc2021::*;

fn main() {
    run()
}

#[test]
fn test() {
    run()
}

fn run() {
    assert_eq!(a("txt/s12a.txt"), 10);
    assert_eq!(a("txt/s12b.txt"), 19);
    assert_eq!(a("txt/s12c.txt"), 226);
    assert_eq!(a("txt/e12.txt"), 5076);
    assert_eq!(b("txt/s12a.txt"), 36);
    assert_eq!(b("txt/s12b.txt"), 103);
    assert_eq!(b("txt/s12c.txt"), 3509);
    assert_eq!(b("txt/e12.txt"), 145643);
}

#[derive(PartialEq, Eq, Debug, Ord, PartialOrd, Clone, Copy, Hash)]
enum Node {
    Start,
    End,
    Small(Hsh),
    Large(Hsh),
}

struct Line(Node, Node);

impl FromStr for Line {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, ()> {
        let (a, b) = split2(s, "-")?;
        Ok(Line(Node::from_str(a)?, Node::from_str(b)?))
    }
}

impl FromStr for Node {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, ()> {
        use Node::*;
        match s {
            "start" => Ok(Start),
            "end" => Ok(End),
            _ => {
                if s.chars().all(|c| c.is_lowercase()) {
                    Ok(Small(hash(&s.to_owned())))
                } else {
                    Ok(Large(hash(&s.to_owned())))
                }
            }
        }
    }
}

struct Graph<N = Node> {
    map: BTreeMap<N, BTreeSet<N>>,
}

impl<N: PartialEq + Ord + Copy> Graph<N> {
    fn new() -> Graph {
        Graph {
            map: BTreeMap::new(),
        }
    }

    fn insert(&mut self, a: N, b: N) {
        assert!(a != b);
        self.map.entry(a).or_insert_with(Default::default).insert(b);
        self.map.entry(b).or_insert_with(Default::default).insert(a);
    }

    fn neighbors(&self, n: N) -> impl Iterator<Item = N> + '_ {
        self.map.get(&n).unwrap().iter().copied()
    }
}

#[derive(Clone, Debug)]
struct Path {
    small_nodes: Count<Hsh>,
    last: Node,
}

impl Path {
    fn new() -> Path {
        Path {
            small_nodes: Default::default(),
            last: Node::Start,
        }
    }
    fn current_node(&self) -> Node {
        self.last
    }
    fn has_visited(&self, hsh: Hsh) -> bool {
        self.small_nodes.contains(&hsh)
    }
    fn can_visit(&self, node: Node) -> bool {
        use Node::*;
        match node {
            Start => false,
            End => true,
            Large(_) => true,
            Small(hsh) => !self.has_visited(hsh),
        }
    }

    fn can_visit_small_twice(&self) -> bool {
        match self.small_nodes.max() {
            None => true,
            Some((_, v)) => v < 2,
        }
    }

    fn can_visit_2(&self, node: Node, can_visit_small_twice: bool) -> bool {
        use Node::*;
        match node {
            Start => false,
            End => true,
            Large(_) => true,
            Small(hsh) => can_visit_small_twice || !self.has_visited(hsh),
        }
    }

    fn visit(&mut self, node: Node) {
        if let Node::Small(hsh) = node {
            self.small_nodes.count(hsh);
        }
        self.last = node;
    }
}

fn a(s: &str) -> usize {
    let mut graph = Graph::<Node>::new();

    for line in read_parsed::<Line>(s) {
        graph.insert(line.0, line.1);
    }

    let mut paths: Vec<Path> = vec![Path::new()];
    let mut paths_len = 1;
    let mut done_paths: Vec<Path> = vec![];

    //println!();
    while paths_len != 0 {
        let mut next_paths: Vec<Path> = vec![];
        for path in paths {
            let current_node = path.current_node();

            if current_node == Node::End {
                // println!("Done {:?}", &path);
                done_paths.push(path);
            } else {
                let neighbors = graph.neighbors(current_node);
                for neighbor in neighbors {
                    if path.can_visit(neighbor) {
                        let mut s = path.clone();
                        s.visit(neighbor);
                        next_paths.push(s);
                    } else {
                        // Skip
                    }
                }
            }
        }
        paths = next_paths;
        paths_len = paths.len();
    }

    done_paths.len()
}

fn b(s: &str) -> usize {
    let mut graph = Graph::<Node>::new();

    for line in read_parsed::<Line>(s) {
        graph.insert(line.0, line.1);
    }

    let mut paths: Vec<Path> = vec![Path::new()];
    let mut paths_len = 1;
    let mut done_paths: Vec<Path> = vec![];

    //println!();
    while paths_len != 0 {
        let mut next_paths: Vec<Path> = vec![];
        for path in paths {
            let current_node = path.current_node();

            if current_node == Node::End {
                // println!("Done {:?}", &path);
                done_paths.push(path);
            } else {
                let can_visit_small_twice = path.can_visit_small_twice();
                let neighbors = graph.neighbors(current_node);
                for neighbor in neighbors {
                    if path.can_visit_2(neighbor, can_visit_small_twice) {
                        let mut s = path.clone();
                        s.visit(neighbor);
                        next_paths.push(s);
                    }
                }
            }
        }
        paths = next_paths;
        paths_len = paths.len();
    }

    done_paths.len()
}
