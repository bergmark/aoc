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

impl Node {
    fn small(&self) -> Option<Hsh> {
        match self {
            Node::Small(hsh) => Some(*hsh),
            _ => None,
        }
    }
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

    fn can_visit_2(&self, node: Node) -> bool {
        use Node::*;
        match node {
            Start => false,
            End => true,
            Large(_) => true,
            Small(hsh) => !self.has_visited(hsh) || self.can_visit_small_twice(),
        }
    }

    fn visit(&self, node: Node, count: impl FnOnce(Node) -> Option<Hsh>) -> Path {
        let mut path = self.clone();
        if let Some(hsh) = count(node) {
            path.small_nodes.count(hsh);
        }
        path.last = node;
        path
    }
}

fn a(s: &str) -> usize {
    sol(s, |p, n| p.can_visit(n))
}

fn b(s: &str) -> usize {
    sol(s, |p, n| p.can_visit_2(n))
}

fn sol(s: &str, can_visit: impl Fn(&Path, Node) -> bool) -> usize {
    let graph = Graph::from_iter(read_parsed(s).map(|l: Line| (l.0, l.1)));

    let mut paths: Vec<Path> = vec![Path::new()];
    let mut paths_len = 1;
    let mut done_paths = 0;

    while paths_len != 0 {
        let mut next_paths: Vec<Path> = vec![];
        for path in paths {
            let current_node = path.current_node();

            let neighbors = graph.neighbors(current_node);
            for neighbor in neighbors {
                if neighbor == Node::End {
                    done_paths += 1;
                } else if can_visit(&path, neighbor) {
                    next_paths.push(path.visit(neighbor, |n| n.small()));
                }
            }
        }
        paths = next_paths;
        paths_len = paths.len();
    }

    done_paths
}
