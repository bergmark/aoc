#![feature(drain_filter)]
use std::fs;
use std::env;

fn read_file(fp: &str) -> String {
    fs::read_to_string(fp)
        .expect("Could not read file")
        .trim()
        .to_string()
}

fn parse(s: String) -> Vec<Orbit> {
    s.lines().map(|l| {
        let w: Vec<_> = l.split(')').collect();
        Orbit {
            parent: Object(w[0].to_string()),
            orbiter: Object(w[1].to_string()),
        }
    }).collect()
}

#[derive(Debug, PartialEq, Eq)]
pub struct Object(String);

#[derive(Debug)]
struct Orbit {
    parent: Object,
    orbiter: Object
}

#[derive(Debug)]
struct OrbitNode {
    this: Object,
    children: Vec<OrbitNode>
}
impl OrbitNode {
    fn new(o: Object) -> OrbitNode {
        OrbitNode {
            this: o,
            children: Vec::new(),
        }
    }
}

fn main() {
    let args = env::args().collect::<Vec<String>>();
    let input_file = &args[1];
    let input = read_file(input_file);

    let mut orbits = parse(input);
    let mut root = OrbitNode::new(Object("COM".to_string()));
    rec(&mut orbits, &mut root);

    print_orbit_node(&root);
    println!("Answer: {}", rec_children(&root));
}

fn print_orbit_node(o: &OrbitNode) {
    print_orbit_node_rec(0, o);
}

fn print_orbit_node_rec(indent: usize, o: &OrbitNode) {
    let prefix = " ".repeat(indent * 1);
    println!("{}{}", prefix, o.this.0);
    for c in o.children.iter() {
        print_orbit_node_rec(indent + 1, &c)
    }
}

fn rec_children(o: &OrbitNode) -> usize {
    o.children.len() + o.children.iter().map(rec_children).sum::<usize>()
}

fn rec(orbits: &mut Vec<Orbit>, root: &mut OrbitNode) {
    root.children =
        orbits.drain_filter(|o| o.parent == root.this)
        .map(|o| OrbitNode::new(o.orbiter))
        .collect::<Vec<_>>();
    for t in root.children.iter_mut() {
        rec(orbits, t);
    }
}
