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
    //assert_eq!(a("txt/s12b.txt"), 19);
    //assert_eq!(a("txt/s12c.txt"), 226);
    //assert_eq!(a("txt/e12.txt"), 5076);
    assert_eq!(b("txt/s12a.txt"), 36);
    assert_eq!(b("txt/s12b.txt"), 103);
    assert_eq!(b("txt/s12c.txt"), 3509);
    assert_eq!(b("txt/e12.txt"), 145643);
}

#[derive(PartialEq, Eq, Debug, Ord, PartialOrd, Clone)]
enum Node {
    Start,
    End,
    Small(String),
    Large(String),
}

impl Node {
    fn is_small(&self) -> bool {
        matches!(self, Node::Small(_))
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
                    Ok(Small(s.to_owned()))
                } else {
                    Ok(Large(s.to_owned()))
                }
            }
        }
    }
}

struct Graph<N = Node> {
    map: BTreeMap<N, BTreeSet<N>>,
}

impl<N: PartialEq + Ord + Clone> Graph<N> {
    fn new() -> Graph {
        Graph {
            map: BTreeMap::new(),
        }
    }

    fn insert(&mut self, a: N, b: N) {
        assert!(a != b);
        self.map
            .entry(a.clone())
            .or_insert_with(Default::default)
            .insert(b.clone());
        self.map.entry(b).or_insert_with(Default::default).insert(a);
    }

    fn neighbors(&self, n: &N) -> impl Iterator<Item = &N> {
        self.map.get(n).unwrap().iter()
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
struct State {
    nodes: Vec<Node>,
}

impl State {
    fn new() -> State {
        State {
            nodes: vec![Node::Start],
        }
    }
    fn current_node(&self) -> Node {
        self.nodes.last().unwrap().clone()
    }
    fn has_visited(&self, node: &Node) -> bool {
        self.nodes.contains(node)
    }
    fn can_visit(&self, node: &Node) -> bool {
        use Node::*;
        match node {
            Start => false,
            End => true,
            Large(_) => true,
            s @ Small(_) => !self.has_visited(s),
        }
    }

    fn can_visit_small_twice(&self) -> bool {
        let mut occ = BTreeSet::new();
        for node in &self.nodes {
            if node.is_small() {
                if occ.contains(&node) {
                    return false;
                }
                occ.insert(node);
            }
        }
        true
    }

    fn can_visit_2(&self, node: &Node, can_visit_small_twice: bool) -> bool {
        use Node::*;
        match node {
            Start => false,
            End => true,
            Large(_) => true,
            s @ Small(_) => can_visit_small_twice || !self.has_visited(s),
        }
    }

    fn visit(&mut self, node: Node) {
        self.nodes.push(node);
    }
}

fn a(s: &str) -> usize {
    let lines: Vec<Line> = read_parsed(s).collect();

    let mut graph = Graph::<Node>::new();

    for line in lines {
        graph.insert(line.0, line.1);
    }

    let mut states: Vec<State> = vec![State::new()];
    let mut states_len = 1;
    let mut done_states: Vec<State> = vec![];

    //println!();
    while states_len != 0 {
        let mut next_states: Vec<State> = vec![];
        for state in states {
            let current_node = state.current_node();

            if current_node == Node::End {
                // println!("Done {:?}", &state);
                done_states.push(state);
            } else {
                let neighbors = graph.neighbors(&current_node);
                for neighbor in neighbors {
                    if state.can_visit(neighbor) {
                        let mut s = state.clone();
                        s.visit(neighbor.clone());
                        next_states.push(s);
                    } else {
                        // Skip
                    }
                }
            }
        }
        states = next_states;
        states_len = states.len();
    }

    done_states.len()
}

fn b(s: &str) -> usize {
    let lines: Vec<Line> = read_parsed(s).collect();

    let mut graph = Graph::<Node>::new();

    for line in lines {
        graph.insert(line.0, line.1);
    }

    let mut states: Vec<State> = vec![State::new()];
    let mut states_len = 1;
    let mut done_states: Vec<State> = vec![];

    //println!();
    while states_len != 0 {
        let mut next_states: Vec<State> = vec![];
        for state in states {
            let current_node = state.current_node();

            if current_node == Node::End {
                // println!("Done {:?}", &state);
                done_states.push(state);
            } else {
                let can_visit_small_twice = state.can_visit_small_twice();
                let neighbors = graph.neighbors(&current_node);
                for neighbor in neighbors {
                    if state.can_visit_2(neighbor, can_visit_small_twice) {
                        let mut s = state.clone();
                        s.visit(neighbor.clone());
                        next_states.push(s);
                    }
                }
            }
        }
        states = next_states;
        states_len = states.len();
    }

    done_states.len()
}
