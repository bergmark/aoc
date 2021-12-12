use std::collections::{BTreeMap, BTreeSet};

pub struct Graph<N> {
    map: BTreeMap<N, BTreeSet<N>>,
}

impl<N: PartialEq + Ord + Copy> Graph<N> {
    pub fn new() -> Graph<N> {
        Graph {
            map: BTreeMap::new(),
        }
    }

    pub fn from_iter(iter: impl IntoIterator<Item = (N, N)>) -> Graph<N> {
        let mut graph = Graph::new();
        for (a, b) in iter {
            graph.insert(a, b)
        }
        graph
    }

    pub fn insert(&mut self, a: N, b: N) {
        assert!(a != b);
        self.map.entry(a).or_insert_with(Default::default).insert(b);
        self.map.entry(b).or_insert_with(Default::default).insert(a);
    }

    pub fn neighbors(&self, n: N) -> impl Iterator<Item = N> + '_ {
        self.map.get(&n).unwrap().iter().copied()
    }
}
